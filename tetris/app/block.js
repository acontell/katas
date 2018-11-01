const _ = require('lodash');

function Block(row, column, isRotatingCenter) {
    this.getRow = () => row;
    this.getColumn = () => column;
    this.moveDown = () => ++row;
    this.updatePosition = lines => {
        row += _.size(_.filter(lines, line => line > row));
        return this;
    };
    this.isRotatingCenter = () => Boolean(isRotatingCenter);
    this.equals = aBlock => row === aBlock.getRow() && column === aBlock.getColumn();
}

module.exports = Block;