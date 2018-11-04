const _ = require('lodash');

function Block(row, column, isInitialBlock) {
    this.getRow = () => row;
    this.getColumn = () => column;
    this.moveDown = () => ++row;
    this.goDown = numberOfRows => row += numberOfRows;
    this.moveRight = () => ++column;
    this.moveLeft = () => --column;
    this.getNumberOfRowsBelow = rows => _.size(rows.filter(aRow => aRow > row));
    this.isInitialBlock = () => Boolean(isInitialBlock);
    this.equals = aBlock => row === aBlock.getRow() && column === aBlock.getColumn();
}

module.exports = Block;