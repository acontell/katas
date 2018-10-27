function Block(row, column) {
    this.getRow = () => row;
    this.getColumn = () => column;
    this.equals = aBlock => row === aBlock.getRow() && column === aBlock.getColumn();
    this.moveDown = () => ++row;
    //this.toString = () => "ROW ->" + row + ", COLUMN ->" + column;
}

module.exports = Block;