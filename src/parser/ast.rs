use super::Statement;

pub struct AbstractSyntaxTree {
    pub statements: Vec<Statement>,
}

impl AbstractSyntaxTree {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    fn token_literal(&self) -> String {
        self.statements
            .first()
            .map(|stmt| stmt.token_literal())
            .unwrap_or_default()
    }

    fn string_representation(&self) -> String {
        let mut buffer = String::new();

        for stmt in &self.statements {
            buffer.push_str(stmt.string_representation());
        }

        buffer
    }
}
