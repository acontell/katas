function Pencil(ctx, conf) {
    let canvas = ctx.canvas;
    let height = canvas.height;
    let width = canvas.width;
    let squareSide = conf.squareSide;
    let separatingLineYPoint = conf.separatingLineYPoint;
    let scoreY = conf.scoreY;
    let scoreX = conf.scoreX;
    let font = conf.font;
    let colors = conf.colors;
    let separatingLineColor = colors.separatingLineColor;
    let scoreMessage = conf.scoreMessage;
    let gameOverMessage = conf.gameOverMessage;
    let gameOverX = conf.gameOverX;
    let gameOverY = conf.gameOverY;

    this.drawGameOver = () => fillText(gameOverMessage, gameOverX, gameOverY);

    function fillText(text, x, y) {
        ctx.font = font;
        ctx.fillText(text, x, y);
    }

    this.drawGame = (matrix, score) => {
        clear();
        drawMatrix(matrix);
        drawSeparatingLine();
        drawScore(score);
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

    function drawSeparatingLine() {
        ctx.fillStyle = separatingLineColor;
        ctx.beginPath();
        ctx.moveTo(0, separatingLineYPoint);
        ctx.lineTo(width, separatingLineYPoint);
        ctx.stroke();
    }

    function drawScore(score) {
        fillText(scoreMessage + score, scoreX, scoreY);
    }
}

module.exports = Pencil;