pub trait Grammar {
    fn is_lambda(&self, ch: char, step: usize, stored: &str) -> bool;

    fn is_ident(&self, ch: char, step: usize, stored: &str) -> bool;

    fn is_body_del(&self, ch: char, step: usize, stored: &str) -> bool;

    fn is_open_paren(&self, ch: char, step: usize, stored: &str) -> bool;

    fn is_close_paren(&self, ch: char, step: usize, stored: &str) -> bool;

    fn is_whitespace(&self, ch: char) -> bool;
}

#[derive(Copy, Clone, Debug)]
pub struct Default;

impl Grammar for Default {
    fn is_lambda(&self, ch: char, step: usize, _: &str) -> bool {
        step == 0 && (ch == '\\' || ch == 'λ')
    }

    fn is_ident(&self, ch: char, _: usize, _: &str) -> bool {
        !self.is_lambda(ch, 0, "") && !self.is_whitespace(ch)
            && !self.is_open_paren(ch, 0, "")
            && !self.is_close_paren(ch, 0, "")
            && !self.is_body_del(ch, 0, "")
    }

    fn is_body_del(&self, ch: char, step: usize, _: &str) -> bool {
        step == 0 && ch == '.'
    }

    fn is_open_paren(&self, ch: char, step: usize, _: &str) -> bool {
        step == 0 && ch == '('
    }

    fn is_close_paren(&self, ch: char, step: usize, _: &str) -> bool {
        step == 0 && ch == ')'
    }

    fn is_whitespace(&self, ch: char) -> bool {
        ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r'
    }
}
