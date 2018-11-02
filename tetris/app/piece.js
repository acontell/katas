const _ = require('lodash');

function Piece(blocks, id, rotation) {
    let rotationState = rotation || 0;

    this.getBlocks = () => blocks;
    this.getId = () => id;
    this.moveDown = () => _.forEach(blocks, block => block.moveDown());
    this.moveRight = () => _.forEach(blocks, block => block.moveRight());
    this.moveLeft = () => _.forEach(blocks, block => block.moveLeft());
    this.getLowestBlock = () => _.last(sortByRowAscColumnDesc());
    this.getHighestBlock = () => _.head(sortByRowAscColumnDesc());
    this.getFarRightBlock = () => _.last(sortByColumnAsc());
    this.getFarLeftBlock = () => _.head(sortByColumnAsc());
    this.getRotatingBlock = () => _.find(blocks, block => block.isRotatingOrigin());
    this.getRotationState = () => rotationState;
    this.isEmpty = () => _.size(blocks) === 0;
    this.clearBlocks = lines => blocks = blocks.filter(block => !_.includes(lines, block.getRow()));
    this.collapse = rows => blocks.forEach(block => block.addRows(block.getNumberOfRowsBelow(rows)));

    function sortByRowAscColumnDesc() {
        return _.sortBy(blocks, block => block.getRow(), block => -block.getColumn());
    }

    function sortByColumnAsc() {
        return _.sortBy(blocks, block => block.getColumn());
    }
}

module.exports = Piece;