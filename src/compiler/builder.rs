use base64::{engine::general_purpose, Engine};
use flate2::{write::GzEncoder, Compression};
// GOSH I FUCKING LOVE SERDE
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, io::Write};

use crate::parser::ast::Identifier;

#[derive(Serialize, Deserialize, Debug)]
pub struct CodeTemplate {
    pub blocks: Vec<CodeBlock>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct CodeBlock {
    pub id: String,
    #[serde(flatten)]
    pub info: Info,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum Info {
    BlockInfo(BlockInfo),
    BracketInfo(BracketInfo),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "block")]
pub enum BlockInfo {
    #[serde(rename = "event")]
    PlayerEvent { args: Args, action: String },
    #[serde(rename = "entity_event")]
    EntityEvent { args: Args, action: String },
    #[serde(rename = "func")]
    Function { args: Args, data: String },
    #[serde(rename = "process")]
    Process { args: Args, data: String },
    #[serde(rename = "call_func")]
    CallFunction { args: Args, data: String },
    #[serde(rename = "start_process")]
    StartProcess { args: Args, data: String },
    #[serde(rename = "if_var")]
    IfVariable { args: Args, action: String },
    #[serde(rename = "if_player")]
    IfPlayer { args: Args, action: String },
    #[serde(rename = "if_game")]
    IfGame { args: Args, action: String },
    #[serde(rename = "if_entity")]
    IfEntity { args: Args, action: String },
    #[serde(rename = "else")]
    Else,
    #[serde(rename = "set_var")]
    SetVariable { args: Args, action: String },
    #[serde(rename = "player_action")]
    PlayerAction { args: Args, action: String },
    #[serde(rename = "game_action")]
    GameAction { args: Args, action: String },
    #[serde(rename = "entity_action")]
    EntityAction { args: Args, action: String },
    #[serde(rename = "control")]
    Control { args: Args, action: String },
    #[serde(rename = "repeat")]
    Repeat {
        args: Args,
        action: String,
        #[serde(rename = "subAction")]
        sub_action: Option<String>,
    },
    // TODO: Select, NOT
}

#[derive(Serialize, Deserialize, Debug)]
pub struct BracketInfo {
    #[serde(rename = "direct")]
    pub direction: BracketDirection,
    #[serde(rename = "type")]
    pub typ: BracketType,
}

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct Args {
    pub items: Vec<CodeItem>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct CodeItem {
    pub item: CodeValue,
    pub slot: i32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(tag = "id", content = "data")]
pub enum CodeValue {
    #[serde(rename = "txt")]
    Text { name: String },
    #[serde(rename = "num")]
    Number { name: String },
    #[serde(rename = "var")]
    Variable { name: String, scope: VariableScope },
    #[serde(rename = "loc")]
    Location {
        #[serde(rename = "is_block")]
        is_block: bool,
        x: f64,
        y: f64,
        z: f64,
        pitch: f64,
        yaw: f64,
    },
    #[serde(rename = "vec")]
    Vector { x: f64, y: f64, z: f64 },
    #[serde(rename = "snd")]
    Sound {
        sound: String,
        pitch: f64,
        #[serde(rename = "vol")]
        volume: f64,
        variant: Option<String>,
    },
    #[serde(rename = "pot")]
    Potion {
        #[serde(rename = "pot")]
        potion: String,
        #[serde(rename = "dur")]
        duration: f64,
        #[serde(rename = "amp")]
        amplitude: f64,
    },
    #[serde(rename = "g_val")]
    GameValue {
        #[serde(rename = "type")]
        typ: String,
        target: String,
    },
    #[serde(rename = "bl_tag")]
    Tag {
        option: String,
        tag: String,
        action: String,
        block: String,
    },
    #[serde(rename = "part")]
    Particle {
        particle: String,
        cluster: ParticleCluster,
        data: HashMap<String, serde_json::Value>,
    },
    #[serde(rename = "item")]
    Item {
        #[serde(rename = "item")]
        nbt: String,
    },
}
// TODO: particles, spawn eggs?

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub enum VariableScope {
    #[serde(rename = "saved")]
    Save,
    #[serde(rename = "unsaved")]
    Global,
    #[default]
    #[serde(rename = "local")]
    Local,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum BracketDirection {
    #[serde(rename = "open")]
    Open,
    #[serde(rename = "close")]
    Close,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum BracketType {
    #[serde(rename = "norm")]
    Normal,
    #[serde(rename = "repeat")]
    Repeat,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ParticleCluster {
    pub amount: i32,
    pub horizontal: f64,
    pub vertical: f64,
}

// Shorthands

pub fn if_var(var: CodeValue, value: CodeValue) -> CodeBlock {
    CodeBlock {
        id: "block".to_string(),
        info: Info::BlockInfo(BlockInfo::IfVariable {
            args: Args {
                items: vec![
                    CodeItem { item: var, slot: 0 },
                    CodeItem {
                        item: value,
                        slot: 1,
                    },
                ],
            },
            action: "=".to_string(),
        }),
    }
}

pub fn set_var(var: CodeValue, value: CodeValue) -> CodeBlock {
    CodeBlock {
        id: "block".to_string(),
        info: Info::BlockInfo(BlockInfo::SetVariable {
            args: Args {
                items: vec![
                    CodeItem { item: var, slot: 0 },
                    CodeItem {
                        item: value,
                        slot: 1,
                    },
                ],
            },
            action: "=".to_string(),
        }),
    }
}

/// Shorthand for creating an Else CodeBlock
pub fn else_block() -> CodeBlock {
    CodeBlock {
        id: "else".to_string(),
        info: Info::BlockInfo(BlockInfo::Else),
    }
}

/// Shorthand for creating an open bracket CodeBlock
pub fn open_bracket() -> CodeBlock {
    CodeBlock {
        id: "bracket".to_string(),
        info: Info::BracketInfo(BracketInfo {
            direction: BracketDirection::Open,
            typ: BracketType::Normal,
        }),
    }
}

/// Shorthand for creating an open bracket CodeBlock
pub fn open_repeat_bracket() -> CodeBlock {
    CodeBlock {
        id: "bracket".to_string(),
        info: Info::BracketInfo(BracketInfo {
            direction: BracketDirection::Open,
            typ: BracketType::Repeat,
        }),
    }
}

/// Shorthand for creating a close bracket CodeBlock
pub fn close_bracket() -> CodeBlock {
    CodeBlock {
        id: "bracket".to_string(),
        info: Info::BracketInfo(BracketInfo {
            direction: BracketDirection::Close,
            typ: BracketType::Normal,
        }),
    }
}

/// Shorthand for creating a close bracket CodeBlock
pub fn close_repeat_bracket() -> CodeBlock {
    CodeBlock {
        id: "bracket".to_string(),
        info: Info::BracketInfo(BracketInfo {
            direction: BracketDirection::Open,
            typ: BracketType::Repeat,
        }),
    }
}

// Non-serde stuff

impl CodeTemplate {
    pub fn encode(&self) -> String {
        encode_template(serde_json::to_string(self).unwrap())
    }
}

pub fn encode_template(raw: String) -> String {
    // first gzip encode, then base64 encode.
    let mut encoder = GzEncoder::new(Vec::new(), Compression::best());
    encoder.write_all(raw.as_bytes()).unwrap();
    let compressed = encoder.finish().unwrap();
    general_purpose::STANDARD.encode(compressed)
}

impl From<String> for CodeValue {
    fn from(s: String) -> Self {
        CodeValue::Text { name: s }
    }
}

impl From<Identifier> for CodeValue {
    fn from(value: Identifier) -> Self {
        CodeValue::Text { name: value.name }
    }
}

impl From<i32> for CodeValue {
    fn from(i: i32) -> Self {
        CodeValue::Number {
            name: i.to_string(),
        }
    }
}

impl From<f64> for CodeValue {
    fn from(f: f64) -> Self {
        CodeValue::Number {
            name: f.to_string(),
        }
    }
}

impl From<BlockInfo> for CodeBlock {
    fn from(b: BlockInfo) -> Self {
        CodeBlock {
            id: "block".to_string(),
            info: Info::BlockInfo(b),
        }
    }
}

impl CodeValue {
    pub fn as_string(&self) -> Option<String> {
        match self {
            CodeValue::Text { name } => Some(name.clone()),
            CodeValue::Number { name } => Some(name.clone()),
            CodeValue::Variable { name, .. } => Some(format!("%var({})", name)),
            _ => None,
        }
    }

    pub fn into_item(self, slot: i32) -> CodeItem {
        CodeItem { item: self, slot }
    }

    pub fn add_one(self) -> Self {
        // make it so that %math(3+1) is just 4
        if let CodeValue::Number { name } = &self {
            if let Ok(num) = name.parse::<f64>() {
                return CodeValue::Number {
                    name: format!("{}", num + 1f64),
                };
            }
        }

        CodeValue::Number {
            name: format!("%math({}+1)", self.as_string().unwrap()),
        }
    }
}

impl From<Vec<CodeItem>> for Args {
    fn from(v: Vec<CodeItem>) -> Self {
        Args { items: v }
    }
}
