const _ = require('lodash');

function Piece(blocks, id) {
    this.getBlocks = () => blocks;
    this.getId = () => id;
    this.moveDown = () => _.forEach(blocks, block => block.moveDown());
    this.getLowestBlock = () => _.last(_.sortBy(blocks, block => block.getRow(), block => -block.getColumn()));
    this.removeDeletedBlocks = () => blocks = _.filter(blocks, block => !block.isMarkedForDeletion());
    this.isEmpty = () => _.size(blocks) === 0;
}

module.exports = Piece;