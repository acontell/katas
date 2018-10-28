const _ = require('lodash');

function Piece(blocks, id) {
    this.getInitialBlock = () => _.head(blocks);
    this.getBlocks = () => blocks;
    this.getId = () => id;
    this.moveDown = () => _.forEach(blocks, block => block.moveDown());
    this.getLowestBlock = () => _.last(_.sortBy(blocks, block => block.getRow()));
    this.removeDeletedBlocks = () => blocks = _.filter(blocks, block => !block.isMarkedForDeletion());
    this.isEmpty = () => _.size(blocks) === 0;
}

module.exports = Piece;