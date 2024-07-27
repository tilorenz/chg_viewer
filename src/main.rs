use iced::event::Status;
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

use iced::widget::{column, container, text, text_input, Column};
use iced::{Element as IcedElement, Theme};

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
    graph: ChgGraph,
    names_to_indices: HashMap<String, Vec<NodeIndex>>,
    searched_name: String,
    selected_index: Option<NodeIndex>,
    incoming: Vec<NodeIndex>,
    matching: Vec<NodeIndex>,
    outgoing: Vec<NodeIndex>,
}

#[derive(Debug, Clone)]
enum Message {
    GraphLoaded, // unused for now, we just assume it's there and won't change
    SearchSymbol(String),
    SelectionChanged(Option<NodeIndex>),
}

impl Default for App {
    fn default() -> Self {
        let buf = fs::read_to_string("foo.json").unwrap();
        //let chg_graph: ChgGraph = serde_json::from_str(buf.as_str()).unwrap();

        let mut app = Self {
            graph: serde_json::from_str(buf.as_str()).unwrap(),
            names_to_indices: HashMap::new(),
            searched_name: String::from(""),
            selected_index: None,
            incoming: Vec::new(),
            matching: Vec::new(),
            outgoing: Vec::new(),
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
            _ => (),
        }
    }

    fn view(&self) -> IcedElement<Message> {
        let mut def_files = vec![String::from("none...")];
        for idx in self.matching.iter() {
            if let Some(enti) = self.graph.graph.node_weight(*idx) {
                def_files.push(serde_json::to_string(enti).unwrap());
            }
        }
        //if let Some(idx) = self.matching.get(0) {
        //if let Some(enti) = self.graph.graph.node_weight(*idx) {
        //let st = String::new();
        //def_file = serde_json::to_string(enti).unwrap();
        //}
        //}
        let mut col: iced::widget::Column<Message> = column!(
            text_input("Search Symbol...", &self.searched_name)
                .on_input(Message::SearchSymbol)
                .on_paste(Message::SearchSymbol),
            //text(def_file)
        );

        for idx in self.matching.iter() {
            if let Some(enti) = self.graph.graph.node_weight(*idx) {
                col = col.push(entity_view(&enti));
            }
        }

        //for df in def_files {
        //col = col.push(text(df));
        //}

        col.into()
    }
}

fn entity_view(enti: &Entity) -> IcedElement<Message> {
    let symbol_kind_name = format!(
        "{} {}\nPath: {:?}\nGlobal hash: {}, Local hash: {}\n\n",
        serde_json::to_string(&enti.symbol.kind).unwrap(),
        enti.symbol.name,
        enti.symbol.declaration_path,
        serde_json::to_string(&enti.global_hash).unwrap(),
        serde_json::to_string(&enti.local_hash).unwrap(),
        
    );
    let col: Column<Message> = column!(text(symbol_kind_name),);
    iced::widget::container::Container::new(col).into()
    //col.into()
}

fn main() -> iced::Result {
    iced::run("CHG Viewer", App::update, App::view)

    //loop {
    //let mut name = String::new();
    //io::stdin()
    //.read_line(&mut name)
    //.expect("Schreib deutlich, die Sauklaue kann ja keiner lesen!");

    //let name = name.trim();

    //match names_to_indices.get(name) {
    //Some(indices) => {
    //println!("Found {} entries for symbol {name}", indices.len());
    //for (i, idx) in indices.iter().enumerate() {
    //let enti = &g[*idx];
    //println!("Symbol ({}/{}): {:#?}\n", i+1, indices.len(), enti.symbol);

    //for nbr_idx in g.neighbors_directed(*idx, Direction::Incoming) {
    //println!("Incoming: {:#?}", g[nbr_idx].symbol);
    //}

    //println!("");

    //for nbr_idx in g.neighbors_directed(*idx, Direction::Outgoing) {
    //println!("Outgoing: {:#?}", g[nbr_idx].symbol);
    //}

    //println!("====================================================================");
    //}
    //}
    //None => {
    //println!("Couldn't find symbol with name\n{name}");
    //}
    //}
    //}
}
