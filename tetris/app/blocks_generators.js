const Piece = require('./piece');
const Block = require('./block');
const _ = require('lodash');

// Shape
// #
// #
// #
// #
function myBlocks(initialBlock) {
    return _.range(4)
        .map((x) => new Block(initialBlock.getRow() + x, initialBlock.getColumn()));
}

function TypeOnePiece(initialBlock) {
    let blocks = myBlocks(initialBlock);
    Piece.call(this, _.head(blocks));

    this.getBlocks = () => blocks;
}

TypeOnePiece.prototype = Object.create(Piece.prototype);
Object.defineProperty(TypeOnePiece.prototype, 'constructor', {
    value: TypeOnePiece,
    enumerable: false,
    writable: true
});

module.exports = TypeOnePiece;