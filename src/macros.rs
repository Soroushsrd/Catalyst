#[macro_export]
macro_rules! expect_token {
    ($self: ident, $error_type: expr, $msg:expr) => {
        $self.peek().ok_or_else(|| $self.error($error_type, $msg))?
    };
}
