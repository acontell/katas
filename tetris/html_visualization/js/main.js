// In charge of creating the game and start the visualization.
const _ = require('lodash');
const Game = require('../../app/game');
const Board = require('../../app/board');
const pieceFactory = require('../../app/piece_factory');
const boardRules = require('../../app/board_rules');
const GameActions = require('../../app/game_actions');
const gameActions = new GameActions(start, cancel);
const numberOfRows = 24;
const numberOfColumns = 10;
const squareSide = 40;
const width = numberOfColumns * squareSide;
const height = numberOfRows * squareSide;

const board = new Board(numberOfRows, numberOfColumns, pieceFactory, boardRules);
const game = new Game(board, 300, gameActions);
const ctx = document.getElementById('canvas').getContext('2d');
const logger = document.getElementById('logger');
ctx.canvas.width = width;
ctx.canvas.height = height;

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
var animationId;
var keepGoing = true;

function log(text) {
    logger.innerHTML = text;
}

function cancel() {
    log("CLOSING");
    keepGoing = false;
    cancelAnimationFrame(animationId);
}

function start(tickFnc, interval) {
    log("START FUNCTION!!!");

    function draw(matrix) {
        matrix.forEach((row, rowIdx) =>
            row.forEach((column, columnIdx) => {
                    ctx.fillStyle = colors[matrix[rowIdx][columnIdx]] || colors['default'];
                    ctx.fillRect(columnIdx * squareSide, rowIdx * squareSide, squareSide, squareSide);
                }
            )
        );
    }

    function mainLoop() {
        log("MAIN LOOP!!!");

        // Clear canvas
        ctx.clearRect(0, 0, height, width);

        // Draw matrix
        draw(tickFnc());

        // Re-draw.
        if (keepGoing) {
            setTimeout(() => animationId = requestAnimationFrame(mainLoop), interval);
        }
    }

    animationId = requestAnimationFrame(mainLoop);
}

// Let the fun begin!
game.start();