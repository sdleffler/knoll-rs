use word::Word;

#[derive(Debug)]
pub struct Registers<W: Word> {
    pub environment: usize,
    pub template: usize,
    pub accumulators: Vec<W>,
}
