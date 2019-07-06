use super::Draw;
use super::{make_box, BoxStyle};
use crate::types::{BoundingBox, Position, Positioned};
use std::fmt::{self, Debug};
use std::io::{self, Write};

pub struct Viewport<W> {
    /// The box describing the visible part of the viewport.
    ///
    /// Generally the size of the terminal, and positioned at 0, 0
    pub outer: BoundingBox,

    /// The box describing the inner part of the viewport
    ///
    /// Its position is relative to `outer.inner()`, and its size should generally not
    /// be smaller than outer
    pub inner: BoundingBox,

    /// The actual screen that the viewport writes to
    pub out: W,
}

impl<W> Debug for Viewport<W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Viewport {{ outer: {:?}, inner: {:?}, out: <OUT> }}",
            self.outer, self.inner
        )
    }
}

impl<W> Viewport<W> {
    /// Returns true if the (inner-relative) position of the given entity is
    /// visible within this viewport
    fn visible<E: Positioned>(&self, ent: &E) -> bool {
        self.on_screen(ent.position()).within(self.outer.inner())
    }

    /// Convert the given inner-relative position to one on the actual screen
    fn on_screen(&self, pos: Position) -> Position {
        pos + self.inner.position + self.outer.inner().position
    }
}

impl<W: Write> Viewport<W> {
    /// Draw the given entity to the viewport at its position, if visible
    pub fn draw<T: Draw>(&mut self, entity: &T) -> io::Result<()> {
        if !self.visible(entity) {
            return Ok(());
        }
        write!(
            self,
            "{}",
            (entity.position()
                + self.inner.position
                + self.outer.inner().position)
                .cursor_goto()
        )?;
        entity.do_draw(self)
    }

    /// Clear whatever is drawn at the given inner-relative position, if visible
    pub fn clear(&mut self, pos: Position) -> io::Result<()> {
        write!(self, "{} ", self.on_screen(pos).cursor_goto(),)
    }

    /// Initialize this viewport by drawing its outer box to the screen
    pub fn init(&mut self) -> io::Result<()> {
        write!(self, "{}", make_box(BoxStyle::Thin, self.outer.dimensions))
    }
}

impl<W> Positioned for Viewport<W> {
    fn position(&self) -> Position {
        self.outer.position
    }
}

impl<W: Write> Write for Viewport<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.out.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.out.flush()
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.out.write_all(buf)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Dimensions;
    // use proptest::prelude::*;

    #[test]
    fn test_visible() {
        assert!(Viewport {
            outer: BoundingBox::at_origin(Dimensions { w: 10, h: 10 }),
            inner: BoundingBox {
                position: Position { x: -10, y: -10 },
                dimensions: Dimensions { w: 15, h: 15 },
            },
            out: (),
        }
        .visible(&Position { x: 13, y: 13 }));

        assert!(!Viewport {
            outer: BoundingBox::at_origin(Dimensions { w: 10, h: 10 }),
            inner: BoundingBox {
                position: Position { x: -10, y: -10 },
                dimensions: Dimensions { w: 15, h: 15 },
            },
            out: (),
        }
        .visible(&Position { x: 1, y: 1 }));
    }

    // proptest! {
    //     #[test]
    //     fn nothing_is_visible_in_viewport_off_screen(pos: Position, outer: BoundingBox) {
    //         let invisible_viewport = Viewport {
    //             outer,
    //             inner: BoundingBox {
    //                 position: Position {x: -(outer.dimensions.w as i16), y: -(outer.dimensions.h as i16)},
    //                 dimensions: outer.dimensions,
    //             },
    //             out: ()
    //         };

    //         assert!(!invisible_viewport.visible(&pos));
    //     }
    // }
}
