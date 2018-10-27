const _ = require('lodash');

function Piece(blocks, id) {
    this.getInitialBlock = () => _.head(blocks);
    this.getBlocks = () => blocks;
    this.getId = () => id;
    this.moveDown = () => {
        _.forEach(blocks, block => block.moveDown());
        return this;
    };
}

module.exports = Piece;