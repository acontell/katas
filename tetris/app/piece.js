const _ = require('lodash');
const sorter = require('./blocks_sorter');
const sortByRowAscColumnDesc = sorter.sortByRowAscColumnDesc;
const sortByColumnAsc = sorter.sortByColumnAsc;

function Piece(blocks, id, rotation) {
    let rotationState = rotation || 0;

    this.getBlocks = () => blocks;
    this.getId = () => id;
    this.moveDown = () => blocks.forEach(block => block.moveDown());
    this.moveRight = () => blocks.forEach(block => block.moveRight());
    this.moveLeft = () => blocks.forEach(block => block.moveLeft());
    this.getLowestBlock = () => _.last(sortByRowAscColumnDesc(blocks));
    this.getHighestBlock = () => _.head(sortByRowAscColumnDesc(blocks));
    this.getFarRightBlock = () => _.last(sortByColumnAsc(blocks));
    this.getFarLeftBlock = () => _.head(sortByColumnAsc(blocks));
    this.getInitialBlock = () => blocks.find(block => block.isInitialBlock());
    this.getRotationState = () => rotationState;
    this.isEmpty = () => _.isEmpty(blocks);
    this.clearBlocks = lines => blocks = blocks.filter(block => !lines.includes(block.getRow()));
    this.collapseBlocks = rows => blocks.forEach(block => block.goDown(block.getNumberOfRowsBelow(rows)));
}

module.exports = Piece;