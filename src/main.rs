use colored::Colorize;
use petgraph::graph::NodeIndex;
use petgraph::visit::IntoNodeReferences;
use petgraph::Direction;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io::{self, Write};
use std::path::{Path, PathBuf};

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

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ChgGraph {
    pub graph: GraphType,
    pub root: Option<NodeIndex>,
}

fn format_enti(enti: &Entity) -> String {
    let decl_path = if let Some(dp) = &enti.symbol.declaration_path {
        dp.to_str().unwrap().to_owned()
    } else {
        "none".to_owned()
    };

    let sym_kind = serde_json::to_string(&enti.symbol.kind).unwrap();
    let sym_kind = sym_kind.trim_matches('"');
    let sym_kind = if enti.symbol.kind == Kind::Synthetic {
        sym_kind.normal()
    } else {
        sym_kind.yellow()
    };

    let sym_name = if enti.symbol.kind == Kind::Synthetic {
        enti.symbol.name.normal()
    } else {
        enti.symbol.name.bright_blue()
    };

    format!(
        "{} {}\nPath: {}\nGlobal hash: {}, Local hash: {}\n\n",
        sym_kind,
        sym_name,
        decl_path.trim_matches('"'),
        serde_json::to_string(&enti.global_hash)
            .unwrap()
            .trim_matches('"'),
        serde_json::to_string(&enti.local_hash)
            .unwrap()
            .trim_matches('"'),
    )
}

fn main() {
    let buf = fs::read_to_string("foo.json").unwrap();
    let chg_graph: ChgGraph = serde_json::from_str(buf.as_str()).unwrap();
    let g = &chg_graph.graph;

    let mut names_to_indices: HashMap<&str, Vec<NodeIndex>> = HashMap::new();
    for (idx, enti) in g.node_references() {
        names_to_indices
            .entry(enti.symbol.name.as_str())
            .and_modify(|v| v.push(idx))
            .or_insert(vec![idx]);
    }

    loop {
        // this prompt may not be aesthetically pleasing, but when I scroll through the terminal, I
        // want it to stand out as much as possible
        print!("{}", "> ".on_bright_yellow().black().bold());
        _ = io::stdout().flush();
        let mut name = String::new();
        io::stdin()
            .read_line(&mut name)
            .expect("Schreib deutlich, die Sauklaue kann ja keiner lesen!");

        let name = name.trim();

        match names_to_indices.get(name) {
            Some(indices) => {
                println!("Found {} entries for symbol {name}", indices.len());
                for (i, idx) in indices.iter().enumerate() {
                    let enti = &g[*idx];
                    println!(
                        "Symbol ({}/{}):\n{}",
                        i + 1,
                        indices.len(),
                        format_enti(enti)
                    );

                    for nbr_idx in g.neighbors_directed(*idx, Direction::Incoming) {
                        println!("{}:\n{}", "Incoming".green().bold(), format_enti(&g[nbr_idx]));
                    }

                    for nbr_idx in g.neighbors_directed(*idx, Direction::Outgoing) {
                        println!("{}:\n{}", "Outgoing".red().bold(), format_enti(&g[nbr_idx]));
                    }

                    println!(
                        "===================================================================="
                    );
                }
            }
            None => {
                println!("Couldn't find symbol with name\n{name}");
            }
        }
    }
}
