use crate::display::utils::clone_times;
use crate::display::utils::times;
use crate::types::Dimensions;
use itertools::Itertools;
use proptest::prelude::Arbitrary;
use proptest::strategy;
use proptest_derive::Arbitrary;

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
    [
        // 0    1    2    3    4    5    6    7    8    9
        '─', '━', '│', '┃', '┄', '┅', '┆', '┇', '┈', '┉',
        // 10
        '┊', '┋', '┌', '┍', '┎', '┏',
    ],
    [
        // 0    1    2    3    4    5    6    7    8    9
        '┐', '┑', '┒', '┓', '└', '┕', '┖', '┗', '┘', '┙',
        '┚', '┛', '├', '┝', '┞', '┟',
    ],
    [
        // 0    1    2    3    4    5    6    7    8    9
        '┠', '┡', '┢', '┣', '┤', '┥', '┦', '┧', '┨', '┩',
        '┪', '┫', '┬', '┭', '┮', '┯',
    ],
    [
        // 0    1    2    3    4    5    6    7    8    9
        '┰', '┱', '┲', '┳', '┴', '┵', '┶', '┷', '┸', '┹',
        '┺', '┻', '┼', '┽', '┾', '┿',
    ],
    [
        // 0    1    2    3    4    5    6    7    8    9
        '╀', '╁', '╂', '╃', '╄', '╅', '╆', '╇', '╈', '╉',
        '╊', '╋', '╌', '╍', '╎', '╏',
    ],
    [
        // 0    1    2    3    4    5    6    7    8    9
        '═', '║', '╒', '╓', '╔', '╕', '╖', '╗', '╘', '╙',
        '╚', '╛', '╜', '╝', '╞', '╟',
    ],
    [
        // 0    1    2    3    4    5    6    7    8    9
        '╠', '╡', '╢', '╣', '╤', '╥', '╦', '╧', '╨', '╩',
        '╪', '╫', '╬', '╭', '╮', '╯',
    ],
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

trait Stylable {
    fn style(self, style: BoxStyle) -> char;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Arbitrary)]
enum Corner {
    TopRight,
    TopLeft,
    BottomRight,
    BottomLeft,
}

impl Stylable for Corner {
    fn style(self, style: BoxStyle) -> char {
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
    fn style(self, style: BoxStyle) -> char {
        use BoxStyle::*;
        use Line::*;
        match (self, style) {
            (H, Thin) => BOX_CHARS[0][0],
            (V, Thin) => BOX_CHARS[0][2],
            _ => unimplemented!(),
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
