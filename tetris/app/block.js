function Block(row, column) {
    let isMarkedForDeletion = false;

    this.getRow = () => row;
    this.getColumn = () => column;
    this.equals = aBlock => row === aBlock.getRow() && column === aBlock.getColumn();
    this.moveDown = () => ++row;
    this.isMarkedForDeletion = () => isMarkedForDeletion;
    this.markForDeletion = () => isMarkedForDeletion = true;
}

module.exports = Block;