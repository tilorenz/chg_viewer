use clap::Parser;
use cpp_demangle::Symbol as DemanglerSymbol;
use iced::event::{self, Status};
use petgraph::data::Element;
use petgraph::graph::NodeIndex;
use petgraph::visit::IntoNodeReferences;
use petgraph::Direction;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io;
use std::path::{Path, PathBuf};
use std::string::ToString;

use iced::widget::{
    column, container, row, text, text_input, Column, Container, MouseArea, Rule, Scrollable,
};
use iced::{border, window, Element as IcedElement, Event, Subscription, Theme};

#[derive(Clone, Deserialize, Serialize, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Kind {
    #[serde(rename = "function")]
    Function,
    #[serde(rename = "static function")]
    StaticFunction,
    // StaticFunction(PathBuf),
    #[serde(rename = "variable")]
    Variable,
    #[serde(rename = "static variable")]
    StaticVariable,
    // StaticVariable(PathBuf),
    #[serde(rename = "record")]
    Record,
    Unknown,
    Synthetic,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq, Hash)]
pub enum Change {
    None,
    Added,
    Changed,
    Removed,
}

impl Default for Change {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Deserialize, Serialize, Clone)]
pub enum Object {
    ObjectFile(PathBuf),
    StaticallyLinked(String, PathBuf),
    DynamicallyLinked(PathBuf),
}

#[derive(Clone, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Symbol {
    pub name: String,
    pub kind: Kind,
    #[serde(rename = "path")]
    pub declaration_path: Option<PathBuf>,
}

#[derive(Clone, Deserialize, Serialize, PartialEq, Eq, Hash, Debug)]
pub struct Entity {
    pub symbol: Symbol,

    pub target: Option<PathBuf>,
    pub object: Option<Object>,

    #[serde(
        deserialize_with = "from_hex",
        serialize_with = "to_hex",
        rename = "hash"
    )]
    pub local_hash: u128,

    #[serde(deserialize_with = "from_hex", serialize_with = "to_hex", default)]
    pub global_hash: u128,

    #[serde(default)]
    pub change: Change,

    #[serde(default, skip_serializing)]
    pub deps: Option<Vec<Symbol>>,

    #[serde(default)]
    pub external: bool,
}

use serde::Deserializer;
fn from_hex<'de, D>(deserializer: D) -> Result<u128, D::Error>
where
    D: Deserializer<'de>,
{
    let digest: String = Deserialize::deserialize(deserializer)?;
    Ok(u128::from_str_radix(&digest, 16).unwrap())
}

use serde::Serializer;
fn to_hex<S>(key: &u128, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let str = format!("{:032x}", key);
    serializer.serialize_str(&str)
}

pub type GraphType = petgraph::Graph<Entity, ()>;

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct ChgGraph {
    pub graph: GraphType,
    pub root: Option<NodeIndex>,
}

#[derive(Debug)]
struct App {
    // === Business data ===
    graph: ChgGraph,
    names_to_indices: HashMap<String, Vec<NodeIndex>>,

    // === Gui state ===
    searched_name: String,
    selected_index: Option<NodeIndex>,
    incoming: Vec<NodeIndex>,
    matching: Vec<NodeIndex>,
    outgoing: Vec<NodeIndex>,

    // === Gui layout ===
    window_width: f32,
}

#[derive(Debug, Clone)]
enum Message {
    GraphLoaded, // unused for now, we just assume it's there and won't change
    SearchSymbol(String),
    SelectionChanged(Option<NodeIndex>),
    WindowResized(iced::Size),
}

impl Default for App {
    fn default() -> Self {
        let cli_args = CliArgs::parse();

        let buf = match fs::read_to_string(&cli_args.file_name) {
            Ok(b) => b,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => panic!("File not found."),
            Err(e) if e.kind() == std::io::ErrorKind::PermissionDenied => {
                panic!("Permission Denied")
            }
            Err(e) if e.kind() == std::io::ErrorKind::InvalidData => {
                panic!("File appears to be invalid UTF-8")
            }
            Err(_) => panic!("Couldn't read file {}", &cli_args.file_name),
        };

        let mut app = Self {
            graph: serde_json::from_str(buf.as_str()).unwrap(),
            names_to_indices: HashMap::new(),
            searched_name: String::from(""),
            selected_index: None,
            incoming: Vec::new(),
            matching: Vec::new(),
            outgoing: Vec::new(),
            window_width: 420.0,
        };

        //let mut names_to_indices: HashMap<&str, Vec<NodeIndex>> = HashMap::new();
        for (idx, enti) in app.graph.graph.node_references() {
            app.names_to_indices
                .entry(enti.symbol.name.clone())
                .and_modify(|v| v.push(idx))
                .or_insert(vec![idx]);
        }

        app
    }
}

impl App {
    fn update(&mut self, message: Message) {
        match message {
            Message::SearchSymbol(name) => {
                if let Some(v) = self.names_to_indices.get(name.as_str()) {
                    //println!("Got result for {name}: {v:?}");
                    self.matching = v.clone();
                } else {
                    //println!("No result for {name}");
                    self.matching = Vec::new();
                }
                self.searched_name = name;
            }
            Message::SelectionChanged(idx) => {
                if let Some(idx) = idx {
                    self.selected_index = Some(idx);
                    self.matching = vec![idx];
                    self.incoming = self
                        .graph
                        .graph
                        .neighbors_directed(idx, Direction::Incoming)
                        .collect();
                    self.outgoing = self
                        .graph
                        .graph
                        .neighbors_directed(idx, Direction::Outgoing)
                        .collect();
                } else {
                    self.incoming = Vec::new();
                    self.outgoing = Vec::new();
                }
            }
            Message::WindowResized(size) => {
                self.window_width = size.width;
            }
            _ => (),
        }
    }

    fn view(&self) -> IcedElement<Message> {
        let column_width = self.window_width / 3.0 - 18.0;

        let mut enti_col: iced::widget::Column<Message> =
            column!().spacing(2.0).width(column_width);
        let mut incoming_col: iced::widget::Column<Message> =
            column!().spacing(2.0).width(column_width);
        let mut outgoing_col: iced::widget::Column<Message> =
            column!().spacing(2.0).width(column_width);

        for idx in self.matching.iter() {
            if let Some(enti) = self.graph.graph.node_weight(*idx) {
                enti_col = enti_col.push(entity_view(&enti, *idx, column_width, self.selected_index));
            }
        }
        for idx in self.incoming.iter() {
            if let Some(enti) = self.graph.graph.node_weight(*idx) {
                incoming_col = incoming_col.push(entity_view(&enti, *idx, column_width, self.selected_index));
            }
        }
        for idx in self.outgoing.iter() {
            if let Some(enti) = self.graph.graph.node_weight(*idx) {
                outgoing_col = outgoing_col.push(entity_view(&enti, *idx, column_width, self.selected_index));
            }
        }

        let main_row = row!(
            Scrollable::new(incoming_col),
            Rule::vertical(8),
            Scrollable::new(enti_col),
            Rule::vertical(8),
            Scrollable::new(outgoing_col),
        );

        column!(
            text_input("Search Symbol...", &self.searched_name)
                .on_input(Message::SearchSymbol)
                .on_paste(Message::SearchSymbol),
            main_row
        )
        .into()
    }

    fn theme(&self) -> Theme {
        Theme::Dracula
    }

    fn subscription(&self) -> Subscription<Message> {
        event::listen_with(|event, _status, _id| match event {
            Event::Window(window::Event::Resized(size)) => Some(Message::WindowResized(size)),
            _ => None,
        })
    }
}

// like rounded_box, but more rounded
pub fn well_rounded_container(theme: &Theme) -> container::Style {
    let palette = theme.extended_palette();

    container::Style {
        background: Some(palette.background.weak.color.into()),
        border: border::rounded(10),
        ..container::Style::default()
    }
}

// like rounded_box, but more rounded
pub fn selected_well_rounded_container(theme: &Theme) -> container::Style {
    let palette = theme.extended_palette();

    container::Style {
        background: Some(palette.background.strong.color.into()),
        border: border::rounded(10),
        ..container::Style::default()
    }
}

fn format_enti(enti: &Entity) -> String {
    let decl_path = if let Some(dp) = &enti.symbol.declaration_path {
        dp.to_str().unwrap()
    } else {
        "none"
    };

    let clean_name = if let Ok(name) = DemanglerSymbol::new(&enti.symbol.name) {
        format!("Demangled: {}\n", name.to_string())
    } else {
        String::from("")
    };

    format!(
        "{} {}\n{}Path: {}\nGlobal hash: {:032x}\nLocal hash: {:032x}\n\n",
        serde_json::to_string(&enti.symbol.kind)
            .unwrap()
            .trim_matches('"'),
        enti.symbol.name,
        clean_name,
        decl_path.trim_matches('"'),
        &enti.global_hash,
        &enti.local_hash,
    )
}

fn entity_view(enti: &Entity, idx: NodeIndex, width: f32, selected_idx: Option<NodeIndex>) -> IcedElement<Message> {
    let txt = text(format_enti(enti));

    let mut selected = false;
    if let Some(si) = selected_idx {
        selected = idx == si;
    }
    let style = if selected { selected_well_rounded_container } else { well_rounded_container };

    MouseArea::new(
        Container::new(
            txt, //column!(text(symbol_kind_name),)
        )
        .width(width)
        .padding(3.0)
        .style(style),
    )
    .on_press(Message::SelectionChanged(Some(idx)))
    .into()
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct CliArgs {
    file_name: String,
}

fn main() -> iced::Result {
    iced::application("CHG Viewer", App::update, App::view)
        .theme(App::theme)
        .subscription(App::subscription)
        .run()
}
