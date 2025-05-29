#[derive(Clone, Debug)]
pub enum EvalError {
    GenericError,

    UnknownPrefixOp {
        operator: String,
        right_type: String,
    },
    TypeMismatch {
        left_type: String,
        operator: String,
        right_type: String,
    },
    UnknownInfixOp {
        left_type: String,
        operator: String,
        right_type: String,
    },
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::GenericError => write!(f, "Generic error"),

            EvalError::UnknownPrefixOp {
                operator,
                right_type,
            } => write!(f, "unknown operator: {}{}", operator, right_type),

            EvalError::TypeMismatch {
                left_type,
                operator,
                right_type,
            } => write!(
                f,
                "type mismatch: {} {} {}",
                left_type, operator, right_type
            ),

            EvalError::UnknownInfixOp {
                left_type,
                operator,
                right_type,
            } => write!(
                f,
                "unknown operator: {} {} {}",
                left_type, operator, right_type
            ),
        }
    }
}

impl std::error::Error for EvalError {}

impl EvalError {
    pub fn unkown_prefix(operator: impl Into<String>, right_type: impl Into<String>) -> Self {
        EvalError::UnknownPrefixOp {
            operator: operator.into(),
            right_type: right_type.into(),
        }
    }

    pub fn type_mismatch(
        left_type: impl Into<String>,
        operator: impl Into<String>,
        right_type: impl Into<String>,
    ) -> Self {
        EvalError::TypeMismatch {
            left_type: left_type.into(),
            operator: operator.into(),
            right_type: right_type.into(),
        }
    }

    pub fn unkown_infix(
        left_type: impl Into<String>,
        operator: impl Into<String>,
        right_type: impl Into<String>,
    ) -> Self {
        EvalError::UnknownInfixOp {
            left_type: left_type.into(),
            operator: operator.into(),
            right_type: right_type.into(),
        }
    }
}
