use crate::display::utils::clone_times;
use crate::display::utils::times;
use crate::types::pos;
use crate::types::BoundingBox;
use crate::types::Dimensions;
use crate::types::Neighbors;
use itertools::Itertools;
use proptest::prelude::Arbitrary;
use proptest::strategy;
use proptest_derive::Arbitrary;
use std::io::{self, Write};

// Box Drawing
//  	    0 	1 	2 	3 	4 	5 	6 	7 	8 	9 	A 	B 	C 	D 	E 	F
// U+250x 	─ 	━ 	│ 	┃ 	┄ 	┅ 	┆ 	┇ 	┈ 	┉ 	┊ 	┋ 	┌ 	┍ 	┎ 	┏
// U+251x 	┐ 	┑ 	┒ 	┓ 	└ 	┕ 	┖ 	┗ 	┘ 	┙ 	┚ 	┛ 	├ 	┝ 	┞ 	┟
// U+252x 	┠ 	┡ 	┢ 	┣ 	┤ 	┥ 	┦ 	┧ 	┨ 	┩ 	┪ 	┫ 	┬ 	┭ 	┮ 	┯
// U+253x 	┰ 	┱ 	┲ 	┳ 	┴ 	┵ 	┶ 	┷ 	┸ 	┹ 	┺ 	┻ 	┼ 	┽ 	┾ 	┿
// U+254x 	╀ 	╁ 	╂ 	╃ 	╄ 	╅ 	╆ 	╇ 	╈ 	╉ 	╊ 	╋ 	╌ 	╍ 	╎ 	╏
// U+255x 	═ 	║ 	╒ 	╓ 	╔ 	╕ 	╖ 	╗ 	╘ 	╙ 	╚ 	╛ 	╜ 	╝ 	╞ 	╟
// U+256x 	╠ 	╡ 	╢ 	╣ 	╤ 	╥ 	╦ 	╧ 	╨ 	╩ 	╪ 	╫ 	╬ 	╭ 	╮ 	╯
// U+257x 	╰ 	╱ 	╲ 	╳ 	╴ 	╵ 	╶ 	╷ 	╸ 	╹ 	╺ 	╻ 	╼ 	╽ 	╾ 	╿

static BOX: char = '☐';

static BOX_CHARS: [[char; 16]; 8] = [
    // 0
    [
        // 0    1    2    3    4    5    6    7    8    9
        '─', '━', '│', '┃', '┄', '┅', '┆', '┇', '┈', '┉',
        // 10
        '┊', '┋', '┌', '┍', '┎', '┏',
    ],
    // 1
    [
        // 0    1    2    3    4    5    6    7    8    9
        '┐', '┑', '┒', '┓', '└', '┕', '┖', '┗', '┘', '┙',
        '┚', '┛', '├', '┝', '┞', '┟',
    ],
    // 2
    [
        // 0    1    2    3    4    5    6    7    8    9
        '┠', '┡', '┢', '┣', '┤', '┥', '┦', '┧', '┨', '┩',
        '┪', '┫', '┬', '┭', '┮', '┯',
    ],
    // 3
    [
        // 0    1    2    3    4    5    6    7    8    9
        '┰', '┱', '┲', '┳', '┴', '┵', '┶', '┷', '┸', '┹',
        '┺', '┻', '┼', '┽', '┾', '┿',
    ],
    // 4
    [
        // 0    1    2    3    4    5    6    7    8    9
        '╀', '╁', '╂', '╃', '╄', '╅', '╆', '╇', '╈', '╉',
        '╊', '╋', '╌', '╍', '╎', '╏',
    ],
    // 5
    [
        // 0    1    2    3    4    5    6    7    8    9
        '═', '║', '╒', '╓', '╔', '╕', '╖', '╗', '╘', '╙',
        '╚', '╛', '╜', '╝', '╞', '╟',
    ],
    // 6
    [
        // 0    1    2    3    4    5    6    7    8    9
        '╠', '╡', '╢', '╣', '╤', '╥', '╦', '╧', '╨', '╩',
        '╪', '╫', '╬', '╭', '╮', '╯',
    ],
    // 7
    [
        // 0    1    2    3    4    5    6    7    8    9
        '╰', '╱', '╲', '╳', '╴', '╵', '╶', '╷', '╸', '╹',
        '╺', '╻', '╼', '╽', '╾', '╿',
    ],
];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BoxStyle {
    Thin,
    Thick,
    Dotted,
    ThickDotted,
    Dashed,
    ThickDashed,
    Double,
}

impl Arbitrary for BoxStyle {
    type Parameters = ();
    type Strategy = strategy::Just<Self>;
    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        // TODO
        strategy::Just(BoxStyle::Thin)
    }
}

pub trait Stylable {
    fn style(&self, style: BoxStyle) -> char;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Arbitrary)]
enum Corner {
    TopRight,
    TopLeft,
    BottomRight,
    BottomLeft,
}

impl Stylable for Corner {
    fn style(&self, style: BoxStyle) -> char {
        use BoxStyle::*;
        use Corner::*;

        match (self, style) {
            (TopRight, Thin) => BOX_CHARS[1][0],
            (TopLeft, Thin) => BOX_CHARS[0][12],
            (BottomRight, Thin) => BOX_CHARS[1][8],
            (BottomLeft, Thin) => BOX_CHARS[1][4],
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Arbitrary)]
enum Line {
    H,
    V,
}

impl Stylable for Line {
    fn style(&self, style: BoxStyle) -> char {
        use BoxStyle::*;
        use Line::*;
        match (self, style) {
            (H, Thin) => BOX_CHARS[0][0],
            (V, Thin) => BOX_CHARS[0][2],
            _ => unimplemented!(),
        }
    }
}

impl Stylable for Neighbors<Option<BoxStyle>> {
    fn style(&self, _style: BoxStyle) -> char {
        use BoxStyle::*;
        match (self.left, self.right, self.top, self.bottom) {
            (None, None, None, None) => BOX,
            (Some(Thin), None, None, None) => BOX_CHARS[7][4],
            (None, Some(Thin), None, None) => BOX_CHARS[7][6],
            (None, None, Some(Thin), None) => BOX_CHARS[7][5],
            (None, None, None, Some(Thin)) => BOX_CHARS[7][7],
            (Some(Thin), Some(Thin), None, None) => Line::H.style(Thin),
            (Some(Thin), None, Some(Thin), None) => {
                Corner::BottomRight.style(Thin)
            }
            (Some(Thin), None, None, Some(Thin)) => {
                Corner::TopRight.style(Thin)
            }
            (None, Some(Thin), Some(Thin), None) => {
                Corner::BottomLeft.style(Thin)
            }
            (None, Some(Thin), None, Some(Thin)) => Corner::TopLeft.style(Thin),
            (None, None, Some(Thin), Some(Thin)) => Line::V.style(Thin),
            (None, Some(Thin), Some(Thin), Some(Thin)) => BOX_CHARS[1][12],
            (Some(Thin), None, Some(Thin), Some(Thin)) => BOX_CHARS[2][4],
            (Some(Thin), Some(Thin), None, Some(Thin)) => BOX_CHARS[2][12],
            (Some(Thin), Some(Thin), Some(Thin), None) => BOX_CHARS[3][4],
            (Some(Thin), Some(Thin), Some(Thin), Some(Thin)) => {
                BOX_CHARS[3][12]
            }
            neighs => panic!("unimplemented: {:?}", neighs),
        }
    }
}

#[must_use]
pub fn make_box(style: BoxStyle, dims: Dimensions) -> String {
    if dims.h == 0 || dims.w == 0 {
        "".to_string()
    } else if dims.h == 1 && dims.w == 1 {
        BOX.to_string()
    } else if dims.h == 1 {
        times(Line::H.style(style), dims.w)
    } else if dims.w == 1 {
        (0..dims.h).map(|_| Line::V.style(style)).join("\n\r")
    } else {
        let h_line: String = times(Line::H.style(style), dims.w - 2);
        let v_line = Line::V.style(style);
        let v_walls: String = clone_times(
            format!(
                "{}{}{}\n\r",
                v_line,
                times::<_, String>(' ', dims.w - 2),
                v_line
            ),
            dims.h - 2,
        );

        format!(
            "{}{}{}\n\r{}{}{}{}",
            Corner::TopLeft.style(style),
            h_line,
            Corner::TopRight.style(style),
            v_walls,
            Corner::BottomLeft.style(style),
            h_line,
            Corner::BottomRight.style(style),
        )
    }
}

/// Draw the box described by the given BoundingBox's position and dimensions to
/// the given output, with the given style
pub fn draw_box<W: Write>(
    out: &mut W,
    bbox: BoundingBox,
    style: BoxStyle,
) -> io::Result<()> {
    let box_str = make_box(style, bbox.dimensions);
    if bbox.position.x == 0 {
        write!(out, "{}{}", bbox.position.cursor_goto(), box_str)?;
    } else {
        for (i, line) in box_str.split("\n\r").enumerate() {
            debug!("line: {:?}!", line);
            write!(
                out,
                "{}{}",
                (bbox.position + pos(0, i as i16)).cursor_goto(),
                line
            )?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn make_thin_box() {
        let res = make_box(BoxStyle::Thin, Dimensions { w: 10, h: 10 });
        assert_eq!(
            res,
            "┌────────┐
\r│        │
\r│        │
\r│        │
\r│        │
\r│        │
\r│        │
\r│        │
\r│        │
\r└────────┘"
        );
    }

    proptest! {
        #[test]
        fn box_has_height_lines(dims: Dimensions, style: BoxStyle) {
            let res = make_box(style, dims);
            prop_assume!((dims.w > 0 && dims.h > 0));
            assert_eq!(res.split("\n\r").count(), dims.h as usize);
        }

        #[test]
        fn box_lines_have_width_length(dims: Dimensions, style: BoxStyle) {
            let res = make_box(style, dims);
            prop_assume!(dims.w == 0 && dims.h == 0 || (dims.w > 0 && dims.h > 0));
            assert!(res.split("\n\r").all(|l| l.chars().count() == dims.w as usize));
        }
    }
}
