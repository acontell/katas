function Pencil(ctx, conf) {
    let canvas = ctx.canvas;
    let height = canvas.height;
    let width = canvas.width;
    let squareSide = conf.squareSide;
    let colors = conf.colors;

    this.draw = matrix => {
        clear();
        drawMatrix(matrix);
    };

    function clear() {
        ctx.clearRect(0, 0, width, height);
    }

    function drawMatrix(matrix) {
        matrix.forEach((row, rowIdx) =>
            row.forEach((column, columnIdx) => {
                    ctx.fillStyle = colors[matrix[rowIdx][columnIdx]];
                    ctx.fillRect(columnIdx * squareSide, rowIdx * squareSide, squareSide, squareSide);
                }
            )
        );
    }
}

module.exports = Pencil;