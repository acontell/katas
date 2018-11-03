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

module.exports = {
    draw: (ctx, matrix, squareSide) => {
        matrix.forEach((row, rowIdx) =>
            row.forEach((column, columnIdx) => {
                    ctx.fillStyle = colors[matrix[rowIdx][columnIdx]] || colors['default'];
                    ctx.fillRect(columnIdx * squareSide, rowIdx * squareSide, squareSide, squareSide);
                }
            )
        );
    }
};