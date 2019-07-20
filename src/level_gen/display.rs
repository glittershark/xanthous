use std::io::{self, Write};

pub fn print_generated_level<W>(
    level: &Vec<Vec<bool>>,
    out: &mut W,
) -> io::Result<()>
where
    W: Write,
{
    for row in level {
        for cell in row {
            write!(out, "{}", if *cell { "X" } else { " " })?;
        }
        write!(out, "\n")?;
    }
    Ok(())
}
