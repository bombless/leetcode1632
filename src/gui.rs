use macroquad::prelude::*;

pub async fn run(matrix: &Vec<Vec<i32>>) {
    let m = matrix.len();
    let n = matrix[0].len();
    const CELL_WIDTH: f32 = 30.;
    const CELL_HEIGHT: f32 = 30.;
    const CELL_THIKNESS: f32 = 2.;
    const BAR_HEIGHT: f32 = 30.;
    const LEFT_MARGIN: f32 = 30.;
    loop {
        clear_background(GRAY);

        for i in 0 .. m {
            for j in 0 .. n {
                draw_rectangle_lines(
                    i as f32 * (CELL_WIDTH - CELL_THIKNESS / 2.) + LEFT_MARGIN,
                    j as f32 * (CELL_HEIGHT - CELL_THIKNESS / 2.) + BAR_HEIGHT,
                    CELL_WIDTH,
                    CELL_HEIGHT,
                    CELL_THIKNESS,
                    RED
                );
            }
        }

        next_frame().await
    }
}