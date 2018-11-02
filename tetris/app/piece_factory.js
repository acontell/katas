const _ = require('lodash');
const shapeFactory = require('./shape_factory');
const Piece = require('./piece');
const getRandomId = () => _.random(0, shapeFactory.getNumberOfShapes() - 1);

function PieceFactory() {
    this.getRandomPiece = initialBlock => this.getPiece(getRandomId(), initialBlock);
    this.getPiece = (id, initialBlock, rotation) => new Piece(shapeFactory.getBlocksOfShape(id, initialBlock, rotation), id, rotation);
    this.getNumberAvailablePieces = () => shapeFactory.getNumberOfShapes();
    this.getRotatedPiece = piece => this.getPiece(piece.getId(), piece.getRotatingBlock(), piece.getRotationState() + 1);
}

module.exports = new PieceFactory;