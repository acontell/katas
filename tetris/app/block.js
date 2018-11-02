const _ = require('lodash');

function Block(row, column, isRotatingOrigin) {
    this.getRow = () => row;
    this.getColumn = () => column;
    this.moveDown = () => ++row;
    this.moveRight = () => ++column;
    this.moveLeft = () => --column;
    this.getNumberOfRowsBelow = rows => _.size(rows.filter(aRow => aRow > row));
    this.addRows = numberOfRows => row += numberOfRows;
    this.isRotatingOrigin = () => Boolean(isRotatingOrigin);
    this.equals = aBlock => row === aBlock.getRow() && column === aBlock.getColumn();
}

module.exports = Block;