const numberOfRows = 24;
const numberOfColumns = 10;
const squareSide = 25;
const width = numberOfColumns * squareSide;
const scoreOffset = 45;
const height = numberOfRows * squareSide + scoreOffset;
const emptyCellKey = 'empty';
const pencilConf = {
    separatingLineYPoint: height - scoreOffset,
    scoreY: height - 10,
    scoreX: 5,
    gameOverX: width / 7,
    gameOverY: height / 2,
    squareSide: squareSide,
    font: '30px Arial',
    scoreMessage: 'Score: ',
    gameOverMessage: 'GAME OVER',
    colors: {
        0: '#0000cc',
        1: '#cc0000',
        2: '#333300',
        3: '#cccc00',
        4: '#3399ff',
        5: '#ff3399',
        6: '#006600',
        'separatingLineColor': '#000000'
    }
};
pencilConf.colors[emptyCellKey] = '#ffffff';

module.exports = {
    numberOfRows: numberOfRows,
    numberOfColumns: numberOfColumns,
    width: width,
    height: height,
    emptyCellKey: emptyCellKey,
    pencilConf: pencilConf,
    fps: 10,
    canvasId: 'canvas',
    scoreConf: {
        linePoints: 100,
        tetris: 800,
        backToBack: 1200
    },
    keys: {
        37: 'moveLeft',//Left
        39: 'moveRight',// Right
        40: 'moveDown',// Down,
        90: 'rotate'// z
    }
};