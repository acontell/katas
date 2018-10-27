const _ = require('lodash');
const blocksGenerators = require('./blocks_generators');
const Piece = require('./piece');
const Block = require('./block');
const defaultBlock = new Block(0, 0);
const getRandomId = () => _.random(0, blocksGenerators.getNumberOfGenerators() - 1);

function getPiece(id, initialBlock) {
    let generate = blocksGenerators.getGeneratorById(id);
    return new Piece(generate(initialBlock), id);
}

function PieceFactory() {
    this.getRandomPiece = initialBlock => getPiece(getRandomId(), initialBlock);
    this.getListOfAvailablePieces = () => _.range(blocksGenerators.getNumberOfGenerators()).map(id => getPiece(id, defaultBlock));
}

module.exports = new PieceFactory;