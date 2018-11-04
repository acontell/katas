function Pencil(ctx, conf) {
    let canvas = ctx.canvas;
    let height = canvas.height;
    let width = canvas.width;
    let squareSide = conf.squareSide;
    let separatingLineYPoint = height - conf.scoreOffset;
    let scoreYPoint = height - conf.scoreStartingPoint;
    let font = conf.font;
    let colors = conf.colors;
    let separatingLineColor = colors.separatingLineColor;
    let scoreMessage = conf.scoreMessage;

    this.clear = () => ctx.clearRect(0, 0, width, height);
    this.drawMatrix = matrix => {
        matrix.forEach((row, rowIdx) =>
            row.forEach((column, columnIdx) => {
                    ctx.fillStyle = colors[matrix[rowIdx][columnIdx]];
                    ctx.fillRect(columnIdx * squareSide, rowIdx * squareSide, squareSide, squareSide);
                }
            )
        );
    };
    this.drawSeparatingLine = () => {
        ctx.fillStyle = separatingLineColor;
        ctx.beginPath();
        ctx.moveTo(0, separatingLineYPoint);
        ctx.lineTo(width, separatingLineYPoint);
        ctx.stroke();
    };
    this.drawScore = score => {
        ctx.font = font;
        ctx.strokeText(scoreMessage + score, 0, scoreYPoint);
    };
}

module.exports = Pencil;