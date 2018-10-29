const _ = require('lodash');
const shapeFactory = require('./shape_factory');
const Piece = require('./piece');
const Block = require('./block');
const defaultBlock = new Block(0, 0);
const getRandomId = () => _.random(0, shapeFactory.getNumberOfShapes() - 1);

function PieceFactory() {
    this.getRandomPiece = initialBlock => this.getPiece(getRandomId(), initialBlock);
    this.getPiece = (id, initialBlock) => new Piece(shapeFactory.getBlocksOfShape(id, initialBlock), id);
    this.getListOfAvailablePieces = () => _.range(shapeFactory.getNumberOfShapes()).map(id => this.getPiece(id, defaultBlock));
}

module.exports = new PieceFactory;