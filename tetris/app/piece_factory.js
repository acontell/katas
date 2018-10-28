const _ = require('lodash');
const blocksGenerators = require('./blocks_generators');
const Piece = require('./piece');
const Block = require('./block');
const defaultBlock = new Block(0, 0);
const getRandomId = () => _.random(0, blocksGenerators.getNumberOfGenerators() - 1);

function PieceFactory() {
    this.getRandomPiece = initialBlock => this.getPiece(getRandomId(), initialBlock);
    this.getPiece = (id, initialBlock) => new Piece(blocksGenerators.getGeneratorById(id)(initialBlock), id);
    this.getListOfAvailablePieces = () => _.range(blocksGenerators.getNumberOfGenerators()).map(id => this.getPiece(id, defaultBlock));
}

module.exports = new PieceFactory;