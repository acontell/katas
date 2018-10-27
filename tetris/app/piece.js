const _ = require('lodash');

function Piece(blocks, id) {
    this.getInitialBlock = () => _.head(blocks);
    this.getBlocks = () => blocks;
    this.getId = () => id;
    this.getNumberOfBlocks = () => _.size(blocks);
}

module.exports = Piece;