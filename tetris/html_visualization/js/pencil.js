const colors = {
    0: '#0000cc',
    1: '#cc0000',
    2: '#333300',
    3: '#cccc00',
    4: '#3399ff',
    5: '#ff3399',
    6: '#006600',
    'default': '#ffffff'
};

function Pencil(ctx, conf) {
    let canvas = ctx.canvas;
    let height = canvas.height;
    let width = canvas.width;
    let squareSide = conf.squareSide;
    let separatingLineYPoint = height - conf.scoreOffset;
    let scoreYPoint = height - conf.scoreStartingPoint;
    let font = conf.font;

    this.clear = () => ctx.clearRect(0, 0, width, height);
    this.drawMatrix = matrix => {
        matrix.forEach((row, rowIdx) =>
            row.forEach((column, columnIdx) => {
                    ctx.fillStyle = colors[matrix[rowIdx][columnIdx]] || colors['default'];
                    ctx.fillRect(columnIdx * squareSide, rowIdx * squareSide, squareSide, squareSide);
                }
            )
        );
    };
    this.drawSeparatingLine = () => {
        ctx.fillStyle = '#000000';
        ctx.beginPath();
        ctx.moveTo(0, separatingLineYPoint);
        ctx.lineTo(width, separatingLineYPoint);
        ctx.stroke();
    };
    this.drawScore = score => {
        ctx.font = font;
        ctx.strokeText('Score: ' + score, 0, scoreYPoint);
    };
}

module.exports = Pencil;