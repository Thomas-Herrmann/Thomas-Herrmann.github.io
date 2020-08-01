"use strict";
var __haste_prog_id = '26239b7fd0cf519a5954a4015855e9e9a27287da505c4d7bf9c9a5089da6a97b';
var __haste_script_elem = typeof document == 'object' ? document.currentScript : null;
var gameState;

var c, ctx;
var tileWidth = 50;
var tileHeight = 50;

var drawGameState, drawOptions, drawRoll;

const numTilesX = 15;
const numTilesY = 15;

const playerColor = {
    Green: [3, 166, 120],
    Yellow: [242, 159, 5],
    Red: [242, 7, 70],
    Blue: [4, 196, 217]
}

const playerColorDark = {
    Green: [2, 132, 95],
    Yellow: [193, 127, 4],
    Red: [193, 5, 56],
    Blue: [3, 156, 173]
}

const iconColor = [80, 80, 80];
const boardColor = [255, 255, 255];
const diceColor = boardColor;

function onHasteStart() {
    c = document.getElementById("canvas");
    c.width = c.scrollWidth;
    c.height = c.scrollHeight;
    ctx = c.getContext("2d");

    ctx.imageSmoothingEnabled = false;

    tileWidth = c.width / numTilesX;
    tileHeight = c.height / numTilesY;

    function drawBoardPrev() {
        drawBoard(drawGameState, drawOptions, drawRoll);
    }

    window.onresize = function(event) {    
        c.width = c.scrollWidth;
        c.height = c.scrollHeight;
        tileWidth = c.width / numTilesX;
        tileHeight = c.height / numTilesY;

        requestAnimationFrame(drawBoardPrev);
    };
}

function drawTiles(tiles, drawFunc, ...args) {
    for (var i = 0; i < tiles.length; i++) {
        let tile = tiles[i];
        let posX = tile[0] * tileWidth;
        let posY = tile[1] * tileHeight;

        drawFunc(posX, posY, ...args)
    }
}

function cssColor(r, g, b) {
    return "rgb(" + r + "," + g + "," + b + ")";
}

function drawSolid(posX, posY, r, g, b, border = (tileWidth * 0.015)) {
    ctx.fillStyle = cssColor(r, g, b);
    ctx.fillRect(posX + border, posY + border, tileWidth - border * 2, tileHeight - border * 2);
}

function drawGlobe(posX, posY) {
    ctx.beginPath();
    ctx.strokeStyle = cssColor(...iconColor);
    ctx.lineWidth = tileWidth * 0.05;
    ctx.arc(posX + tileWidth / 2, posY + tileHeight / 2, tileWidth / 2.5, 0, Math.PI*2);
    ctx.stroke();
}

function drawStar(posX, posY) {
    let centerX = posX + tileWidth / 2;
    let centerY = posY + tileHeight / 2;

    const starOuterRadiusEven = tileWidth / 2 * 0.9;
    const starOuterRadiusOdd = tileWidth / 2 * 0.65;
    const starInnerRadius = tileWidth / 2 * 0.2;
    const numPoints = 12;

    const angInterval = Math.PI * 2 / numPoints;

    let getPoint = (ang, rad) => [centerX + Math.cos(ang) * rad, centerY + Math.sin(ang) * rad];

    ctx.beginPath();
    ctx.moveTo(...getPoint(0), starOuterRadiusEven);
    for (let i = 0; i < numPoints; i++) {
        let outerRadius = i % 2 == 0 ? starOuterRadiusEven : starOuterRadiusOdd;
        ctx.lineTo(...getPoint(angInterval * (i + 0.5), starInnerRadius));
        ctx.lineTo(...getPoint(angInterval * (i + 1), outerRadius));
    }
    ctx.fillStyle = cssColor(...iconColor);
    ctx.lineWidth = tileWidth * 0.05;
    ctx.fill();
}

function drawPlayer(posX, posY, player, gameState) {
    let field = posToField(posX, posY, player);
    let numPieces = Haste.numPiecesAt(gameState, player, field);

    ctx.beginPath();
    ctx.fillStyle = cssColor(...playerColorDark[player]);
    ctx.arc(posX + tileWidth * 0.5, posY + tileHeight * 0.5, tileWidth * 0.3, 0, 2 * Math.PI);
    ctx.fill();
    ctx.beginPath();
    ctx.fillStyle = cssColor(255, 255, 255);
    ctx.arc(posX + tileWidth * 0.5, posY + tileHeight * 0.5, tileWidth * 0.2, 0, 2 * Math.PI);
    ctx.fill();

    ctx.fillStyle = cssColor(...iconColor);
    ctx.font = "bold " + Math.floor(tileWidth * 0.35) + "px helvetica";
    ctx.fillText(numPieces.toString(), posX + tileWidth * 0.4, posY + tileHeight * 0.62, tileWidth);
}


function drawDiceAnimation(gameState, options, roll, prevRoll) {
    let posX = dicePositions[0][0] * tileWidth;
    let posY = dicePositions[0][1] * tileHeight;

    let angle = 0.0;
    let frameCount = 0;

    function animation() {
        ctx.setTransform(1, 0, 0, 1, 0, 0);
        ctx.clearRect(posX, posY, tileWidth, tileHeight);
        
        let transX = posX + tileWidth * 0.5;
        let transY = posY + tileHeight * 0.5;

        ctx.translate(transX, transY);
        ctx.rotate(angle);
        ctx.translate(-transX, -transY);

        drawDice(posX, posY, prevRoll);

        if (frameCount < 100) {
            ++frameCount;
            angle += (2 * Math.PI) / 100;
            requestAnimationFrame(animation);
        }
        else {
            ctx.setTransform(1, 0, 0, 1, 0, 0);
            drawBoard(drawGameState, drawOptions, drawRoll);
        }
    }

    drawGameState = gameState;
    drawOptions = options;
    drawRoll = roll;
    requestAnimationFrame(animation);
}


function drawDice(posX, posY, num) {
    
    function drawDot(mulX, mulY) {
        ctx.beginPath();
        ctx.arc(posX + tileWidth * mulX, posY + tileHeight * mulY, tileWidth * 0.08, 0, 2 * Math.PI);
        ctx.fill();
    }

    ctx.fillStyle = cssColor(...diceColor);
    ctx.fillRect(posX + tileWidth * 0.15, posY + tileHeight * 0.15, tileWidth * 0.7, tileHeight * 0.7)
    ctx.fillStyle = cssColor(...iconColor);

    switch(num) {
        case -1:
            ctx.font = "bold " + Math.floor(tileWidth * 0.6) + "px helvetica";
            ctx.fillText("?", posX + tileWidth * 0.33, posY + tileHeight * 0.7, tileWidth);
            break;
        case 1:
            drawDot(0.5, 0.5);
            break;
        case 2:
            drawDot(0.3, 0.3);
            drawDot(0.7, 0.7);
            break;
        case 3:
            drawDot(0.3, 0.3);
            drawDot(0.5, 0.5);
            drawDot(0.7, 0.7);
            break;
        case 4:
            drawDot(0.3, 0.3);
            drawDot(0.7, 0.7);
            drawDot(0.3, 0.7);
            drawDot(0.7, 0.3);
            break;
        case 5:
            drawDot(0.3, 0.3);
            drawDot(0.7, 0.7);
            drawDot(0.3, 0.7);
            drawDot(0.7, 0.3);
            drawDot(0.5, 0.5);
            break;
        case 6:
            drawDot(0.3, 0.3);
            drawDot(0.7, 0.7);
            drawDot(0.3, 0.7);
            drawDot(0.7, 0.3);
            drawDot(0.3, 0.5);
            drawDot(0.7, 0.5);
            break;
    }
}

function drawHighlight(posX, posY, color = [255, 255, 255]) {
    ctx.beginPath();
    ctx.strokeStyle = cssColor(...color);
    ctx.lineWidth = tileWidth * 0.1;
    
    ctx.strokeRect(posX + tileWidth * 0.05, posY + tileHeight * 0.05, tileWidth * 0.9, tileHeight * 0.9);
}

const boardPositions = [
    [1, 6], [2, 6], [3, 6], [4, 6], [5, 6],
    [6, 5], [6, 4], [6, 3], [6, 2], [6, 1],
    [6, 0], [7, 0], [8, 0],
    [8, 1], [8, 2], [8, 3], [8, 4], [8, 5],
    [9, 6], [10, 6], [11, 6], [12, 6], [13, 6],
    [14, 6], [14, 7], [14, 8],
    [13, 8], [12, 8], [11, 8], [10, 8], [9, 8],
    [8, 9], [8, 10], [8, 11], [8, 12], [8, 13], 
    [8, 14], [7, 14], [6, 14], 
    [6, 13], [6, 12], [6, 11], [6, 10], [6, 9], 
    [5, 8], [4, 8], [3, 8], [2, 8], [1, 8],
    [0, 8], [0, 7], [0, 6]
];

const homePositions = {
    Green: [[1, 7], [2, 7], [3, 7], [4, 7], [5, 7], [6, 7]],
    Yellow: [[7, 1], [7, 2], [7, 3], [7, 4], [7, 5], [7, 6]],
    Red: [[13, 7], [12, 7], [11, 7], [10, 7], [9, 7], [8, 7]],
    Blue: [[7, 13], [7, 12], [7, 11], [7, 10], [7, 9], [7, 8]],
}

const outPositions = {
    Green: [[2, 2], [3, 2], [2, 3], [3, 3]],
    Yellow: [[11, 2], [12, 2], [11, 3], [12, 3]],
    Blue: [[2, 11], [3, 11], [2, 12], [3, 12]],
    Red: [[11, 11], [12, 11], [11, 12], [12, 12]]
}

const startPosition = {
    Green: [1, 6],
    Yellow: [8, 1],
    Blue: [6, 13],
    Red: [13, 8]
}

const dicePositions =  [[7, 7]];

const starCells = [5, 11, 18, 24, 31, 37, 44, 50];
const globeCells = [0, 8, 13, 21, 26, 34, 39, 47];

const playerOffsets = {
    Green: 13 * 0,
    Yellow: 13 * 1,
    Red: 13 * 2,
    Blue: 13 * 3
}

function playerPosToTilePos(player, pos) {
    if (pos > 50) {
        return homePositions[player][pos - 51];
    }

    let newPos = (pos + playerOffsets[player]) % 52;
    return boardPositions[newPos];
}

function drawStaticBoard() {
    ctx.clearRect(0, 0, c.width, c.height);

    /*for (let x = 0; x < numTilesX; x++) {
        for (let y = 0; y < numTilesY; y++) {
            drawTiles([[x, y]], drawSolid, 100, 100, 100, 0);
        }
    }*/

    drawTiles(boardPositions, drawSolid, ...boardColor);

    drawTiles([...homePositions.Green, ...outPositions.Green, startPosition.Green], drawSolid, ...playerColor.Green);
    drawTiles([...homePositions.Yellow, ...outPositions.Yellow, startPosition.Yellow], drawSolid, ...playerColor.Yellow);
    drawTiles([...homePositions.Red, ...outPositions.Red, startPosition.Red], drawSolid, ...playerColor.Red);
    drawTiles([...homePositions.Blue, ...outPositions.Blue, startPosition.Blue], drawSolid, ...playerColor.Blue);

    drawTiles(globeCells.map((v, i) => boardPositions[v]), drawGlobe);
    drawTiles(starCells.map((v, i) => boardPositions[v]), drawStar);
}

function drawBoard(gameState, options, roll) {

    drawGameState = gameState; // save for window resize
    drawOptions = options;
    drawRoll = roll;

    ctx.setTransform(1, 0, 0, 1, 0, 0);

    drawStaticBoard();

    // Draw dice
    drawTiles(dicePositions, drawDice, roll);

    // Draw players
    for (let player in gameState.pieces) {
        let playerPieces = gameState.pieces[player];

        for (let pieceIndex in gameState.pieces[player]) {
            let piece = playerPieces[pieceIndex];

            if (piece.piece == "Out") {
                drawTiles([outPositions[player][pieceIndex - 1]], drawPlayer, player, gameState);
            }
            else if (piece.piece == "Active") {
                drawTiles([playerPosToTilePos(player, piece.field)], drawPlayer, player, gameState);
            }
        }
    }

    // Draw options
    let player = gameState.turn;
    switch (gameState.stage.stage) {
        case "Roll":
            drawTiles(dicePositions, drawHighlight, playerColorDark[player]);
        break;

        case "SelectPiece":
            for (let option of options) {
                if (option.option == "Play") {
                    drawTiles([outPositions[player][option.piece - 1]], drawHighlight, playerColorDark[player]);
                }
                else if (option.option == "Move") {
                    let pos = gameState.pieces[player][option.piece].field;
                    drawTiles([playerPosToTilePos(player, pos)], drawHighlight, playerColorDark[player]);
                }
            }
        break;

        case "SelectField":
            for (let option of options) {
                if (option.piece == gameState.stage.pieceIndex) {
                    if (option.option == "Play") {
                        drawTiles([startPosition[player]], drawHighlight, playerColorDark[player]);
                    }
                    else if (option.option == "Move") {
                        drawTiles([playerPosToTilePos(player, option.field)], drawHighlight, playerColorDark[player]);
                    }
                }
            }
        break;

        case "GameFinished":
        break;
    }
}

function posToField(posX, posY, player) {
    let tileX = Math.floor(posX / tileWidth);
    let tileY = Math.floor(posY / tileHeight);
    let tilePosStr = [tileX, tileY].toString();
 
    let toStrArr = (arr) => arr.map((v, i) => v.toString())
    
    // Check if field is on board
    let field = toStrArr(boardPositions).indexOf(tilePosStr);
    if (field != -1) {
        // Offset according to player
        field -= playerOffsets[player];

        return (field + 52) % 52;
    }
    
    // Check if field is in a home position
    field = toStrArr(homePositions[player]).indexOf(tilePosStr);
    if (field != -1) {
        return field + 51;
    }

    // Check if field is in an out position
    field = toStrArr(outPositions[player]).indexOf(tilePosStr);
    if (field != -1) {
        return field - 4;
    }

    // Check if field is dice
    field = toStrArr(dicePositions).indexOf(tilePosStr);
    if (field != -1) {
        return -5; // Clicked dice
    }

    // Field not found
    return null;
}
// This object will hold all exports.
var Haste = {};
if(typeof window === 'undefined' && typeof global !== 'undefined') window = global;
window['__haste_crypto'] = window.crypto || window.msCrypto;
if(window['__haste_crypto'] && !window['__haste_crypto'].subtle && window.crypto.webkitSubtle) {
    window['__haste_crypto'].subtle = window.crypto.webkitSubtle;
}

/* Constructor functions for small ADTs. */
function T0(t){this._=t;}
function T1(t,a){this._=t;this.a=a;}
function T2(t,a,b){this._=t;this.a=a;this.b=b;}
function T3(t,a,b,c){this._=t;this.a=a;this.b=b;this.c=c;}
function T4(t,a,b,c,d){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;}
function T5(t,a,b,c,d,e){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;this.e=e;}
function T6(t,a,b,c,d,e,f){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;this.e=e;this.f=f;}

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

/* Hint to optimizer that an imported symbol is strict. */
function __strict(x) {return x}

// A tailcall.
function F(f) {
    this.f = f;
}

// A partially applied function. Invariant: members are never thunks.
function PAP(f, args) {
    this.f = f;
    this.args = args;
    this.arity = f.length - args.length;
}

// "Zero" object; used to avoid creating a whole bunch of new objects
// in the extremely common case of a nil-like data constructor.
var __Z = new T0(0);

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

// Indicates that a closure-creating tail loop isn't done.
var __continue = {};

/* Generic apply.
   Applies a function *or* a partial application object to a list of arguments.
   See https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/FunctionCalls
   for more information.
*/
function A(f, args) {
    while(true) {
        f = E(f);
        if(f instanceof Function) {
            if(args.length === f.length) {
                return f.apply(null, args);
            } else if(args.length < f.length) {
                return new PAP(f, args);
            } else {
                var f2 = f.apply(null, args.slice(0, f.length));
                args = args.slice(f.length);
                f = B(f2);
            }
        } else if(f instanceof PAP) {
            if(args.length === f.arity) {
                return f.f.apply(null, f.args.concat(args));
            } else if(args.length < f.arity) {
                return new PAP(f.f, f.args.concat(args));
            } else {
                var f2 = f.f.apply(null, f.args.concat(args.slice(0, f.arity)));
                args = args.slice(f.arity);
                f = B(f2);
            }
        } else {
            return f;
        }
    }
}

function A1(f, x) {
    f = E(f);
    if(f instanceof Function) {
        return f.length === 1 ? f(x) : new PAP(f, [x]);
    } else if(f instanceof PAP) {
        return f.arity === 1 ? f.f.apply(null, f.args.concat([x]))
                             : new PAP(f.f, f.args.concat([x]));
    } else {
        return f;
    }
}

function A2(f, x, y) {
    f = E(f);
    if(f instanceof Function) {
        switch(f.length) {
        case 2:  return f(x, y);
        case 1:  return A1(B(f(x)), y);
        default: return new PAP(f, [x,y]);
        }
    } else if(f instanceof PAP) {
        switch(f.arity) {
        case 2:  return f.f.apply(null, f.args.concat([x,y]));
        case 1:  return A1(B(f.f.apply(null, f.args.concat([x]))), y);
        default: return new PAP(f.f, f.args.concat([x,y]));
        }
    } else {
        return f;
    }
}

function A3(f, x, y, z) {
    f = E(f);
    if(f instanceof Function) {
        switch(f.length) {
        case 3:  return f(x, y, z);
        case 2:  return A1(B(f(x, y)), z);
        case 1:  return A2(B(f(x)), y, z);
        default: return new PAP(f, [x,y,z]);
        }
    } else if(f instanceof PAP) {
        switch(f.arity) {
        case 3:  return f.f.apply(null, f.args.concat([x,y,z]));
        case 2:  return A1(B(f.f.apply(null, f.args.concat([x,y]))), z);
        case 1:  return A2(B(f.f.apply(null, f.args.concat([x]))), y, z);
        default: return new PAP(f.f, f.args.concat([x,y,z]));
        }
    } else {
        return f;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            if(t.x === __updatable) {
                var f = t.f;
                t.f = __blackhole;
                t.x = f();
            } else {
                return t.f();
            }
        }
        if(t.x === __updatable) {
            throw 'Infinite loop!';
        } else {
            return t.x;
        }
    } else {
        return t;
    }
}

/* Tail call chain counter. */
var C = 0, Cs = [];

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    Cs.push(C);
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        C = 0;
        f = fun();
    }
    C = Cs.pop();
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw E(err);
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return {_:0, a:(a-a%b)/b, b:a%b};
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return {_:0, a:x & 0xffffffff, b:x > 0x7fffffff};
}

function subC(a, b) {
    var x = a-b;
    return {_:0, a:x & 0xffffffff, b:x < -2147483648};
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, __Z);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [str.charCodeAt(i), acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return {_:1,a:str.charCodeAt(i),b:new T(function() {
            return unAppCStr(str,chrs,i+1);
        })};
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str._ == 1; str = E(str.b)) {
        s += String.fromCharCode(E(str.a));
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x.a;
    return x.b;
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return 0;
    } else if(a == b) {
        return 1;
    }
    return 2;
}

/* Convert a JS exception into a Haskell JSException */
function __hsException(e) {
  e = e.toString();
  var x = new Long(738919189, 2683596561, true)
  var y = new Long(3648966346, 573393410, true);
  var t = new T5(0, x, y
                  , new T5(0, x, y
                            , unCStr("haste-prim")
                            , unCStr("Haste.Prim.Foreign")
                            , unCStr("JSException")), __Z, __Z);
  var show = function(x) {return unCStr(E(x).a);}
  var dispEx = function(x) {return unCStr("JavaScript exception: " + E(x).a);}
  var showList = function(_, s) {return unAppCStr(e, s);}
  var showsPrec = function(_, _p, s) {return unAppCStr(e, s);}
  var showDict = new T3(0, showsPrec, show, showList);
  var self;
  var fromEx = function(_) {return new T1(1, self);}
  var dict = new T5(0, t, showDict, null /* toException */, fromEx, dispEx);
  self = new T2(0, dict, new T1(0, e));
  return self;
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        if(typeof e._ === 'undefined') {
            e = __hsException(e);
        }
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Object) {
        return x._;
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round, rintDouble = jsRound, rintFloat = jsRound;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt64(i) {
    return popCnt(i.low) + popCnt(i.high);
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function __clz(bits, x) {
    x &= (Math.pow(2, bits)-1);
    if(x === 0) {
        return bits;
    } else {
        return bits - (1 + Math.floor(Math.log(x)/Math.LN2));
    }
}

// TODO: can probably be done much faster with arithmetic tricks like __clz
function __ctz(bits, x) {
    var y = 1;
    x &= (Math.pow(2, bits)-1);
    if(x === 0) {
        return bits;
    }
    for(var i = 0; i < bits; ++i) {
        if(y & x) {
            return i;
        } else {
            y <<= 1;
        }
    }
    return 0;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    if(x === 0) {
        return __decodedZeroF;
    }
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return {_:0, a:sign*man, b:exp};
}

var __decodedZero = {_:0,a:1,b:0,c:0,d:0};
var __decodedZeroF = {_:0,a:1,b:0};

function decodeDouble(x) {
    if(x === 0) {
        // GHC 7.10+ *really* doesn't like 0 to be represented as anything
        // but zeroes all the way.
        return __decodedZero;
    }
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return {_:0, a:sign, b:manHigh, c:manLow, d:exp};
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

window['jsGetMouseCoords'] = function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs._) {
        strs = E(strs);
        arr.push(E(strs.a));
        strs = E(strs.b);
    }
    return arr.join(sep);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return __Z;
    }
    return {_:1,a:hs};
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return {_:0, a:jsRead(obj)};
    case 'string':
        return {_:1, a:obj};
    case 'boolean':
        return {_:2, a:obj}; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return {_:3, a:arr2lst_json(obj, 0)};
        } else if (obj == null) {
            return {_:5};
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = {_:1, a:{_:0, a:ks[i], b:toHS(obj[ks[i]])}, b:xs};
            }
            return {_:4, a:xs};
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return __Z;
    }
    return {_:1, a:toHS(arr[elem]), b:new T(function() {return arr2lst_json(arr,elem+1);}),c:true}
}

/* gettimeofday(2) */
function gettimeofday(tv, _tz) {
    var t = new Date().getTime();
    writeOffAddr("i32", 4, tv, 0, (t/1000)|0);
    writeOffAddr("i32", 4, tv, 1, ((t%1000)*1000)|0);
    return 0;
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

/* bn.js by Fedor Indutny, see doc/LICENSE.bn for license */
var __bn = {};
(function (module, exports) {
'use strict';

function BN(number, base, endian) {
  // May be `new BN(bn)` ?
  if (number !== null &&
      typeof number === 'object' &&
      Array.isArray(number.words)) {
    return number;
  }

  this.negative = 0;
  this.words = null;
  this.length = 0;

  if (base === 'le' || base === 'be') {
    endian = base;
    base = 10;
  }

  if (number !== null)
    this._init(number || 0, base || 10, endian || 'be');
}
if (typeof module === 'object')
  module.exports = BN;
else
  exports.BN = BN;

BN.BN = BN;
BN.wordSize = 26;

BN.max = function max(left, right) {
  if (left.cmp(right) > 0)
    return left;
  else
    return right;
};

BN.min = function min(left, right) {
  if (left.cmp(right) < 0)
    return left;
  else
    return right;
};

BN.prototype._init = function init(number, base, endian) {
  if (typeof number === 'number') {
    return this._initNumber(number, base, endian);
  } else if (typeof number === 'object') {
    return this._initArray(number, base, endian);
  }
  if (base === 'hex')
    base = 16;

  number = number.toString().replace(/\s+/g, '');
  var start = 0;
  if (number[0] === '-')
    start++;

  if (base === 16)
    this._parseHex(number, start);
  else
    this._parseBase(number, base, start);

  if (number[0] === '-')
    this.negative = 1;

  this.strip();

  if (endian !== 'le')
    return;

  this._initArray(this.toArray(), base, endian);
};

BN.prototype._initNumber = function _initNumber(number, base, endian) {
  if (number < 0) {
    this.negative = 1;
    number = -number;
  }
  if (number < 0x4000000) {
    this.words = [ number & 0x3ffffff ];
    this.length = 1;
  } else if (number < 0x10000000000000) {
    this.words = [
      number & 0x3ffffff,
      (number / 0x4000000) & 0x3ffffff
    ];
    this.length = 2;
  } else {
    this.words = [
      number & 0x3ffffff,
      (number / 0x4000000) & 0x3ffffff,
      1
    ];
    this.length = 3;
  }

  if (endian !== 'le')
    return;

  // Reverse the bytes
  this._initArray(this.toArray(), base, endian);
};

BN.prototype._initArray = function _initArray(number, base, endian) {
  if (number.length <= 0) {
    this.words = [ 0 ];
    this.length = 1;
    return this;
  }

  this.length = Math.ceil(number.length / 3);
  this.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    this.words[i] = 0;

  var off = 0;
  if (endian === 'be') {
    for (var i = number.length - 1, j = 0; i >= 0; i -= 3) {
      var w = number[i] | (number[i - 1] << 8) | (number[i - 2] << 16);
      this.words[j] |= (w << off) & 0x3ffffff;
      this.words[j + 1] = (w >>> (26 - off)) & 0x3ffffff;
      off += 24;
      if (off >= 26) {
        off -= 26;
        j++;
      }
    }
  } else if (endian === 'le') {
    for (var i = 0, j = 0; i < number.length; i += 3) {
      var w = number[i] | (number[i + 1] << 8) | (number[i + 2] << 16);
      this.words[j] |= (w << off) & 0x3ffffff;
      this.words[j + 1] = (w >>> (26 - off)) & 0x3ffffff;
      off += 24;
      if (off >= 26) {
        off -= 26;
        j++;
      }
    }
  }
  return this.strip();
};

function parseHex(str, start, end) {
  var r = 0;
  var len = Math.min(str.length, end);
  for (var i = start; i < len; i++) {
    var c = str.charCodeAt(i) - 48;

    r <<= 4;

    // 'a' - 'f'
    if (c >= 49 && c <= 54)
      r |= c - 49 + 0xa;

    // 'A' - 'F'
    else if (c >= 17 && c <= 22)
      r |= c - 17 + 0xa;

    // '0' - '9'
    else
      r |= c & 0xf;
  }
  return r;
}

BN.prototype._parseHex = function _parseHex(number, start) {
  // Create possibly bigger array to ensure that it fits the number
  this.length = Math.ceil((number.length - start) / 6);
  this.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    this.words[i] = 0;

  // Scan 24-bit chunks and add them to the number
  var off = 0;
  for (var i = number.length - 6, j = 0; i >= start; i -= 6) {
    var w = parseHex(number, i, i + 6);
    this.words[j] |= (w << off) & 0x3ffffff;
    this.words[j + 1] |= w >>> (26 - off) & 0x3fffff;
    off += 24;
    if (off >= 26) {
      off -= 26;
      j++;
    }
  }
  if (i + 6 !== start) {
    var w = parseHex(number, start, i + 6);
    this.words[j] |= (w << off) & 0x3ffffff;
    this.words[j + 1] |= w >>> (26 - off) & 0x3fffff;
  }
  this.strip();
};

function parseBase(str, start, end, mul) {
  var r = 0;
  var len = Math.min(str.length, end);
  for (var i = start; i < len; i++) {
    var c = str.charCodeAt(i) - 48;

    r *= mul;

    // 'a'
    if (c >= 49)
      r += c - 49 + 0xa;

    // 'A'
    else if (c >= 17)
      r += c - 17 + 0xa;

    // '0' - '9'
    else
      r += c;
  }
  return r;
}

BN.prototype._parseBase = function _parseBase(number, base, start) {
  // Initialize as zero
  this.words = [ 0 ];
  this.length = 1;

  // Find length of limb in base
  for (var limbLen = 0, limbPow = 1; limbPow <= 0x3ffffff; limbPow *= base)
    limbLen++;
  limbLen--;
  limbPow = (limbPow / base) | 0;

  var total = number.length - start;
  var mod = total % limbLen;
  var end = Math.min(total, total - mod) + start;

  var word = 0;
  for (var i = start; i < end; i += limbLen) {
    word = parseBase(number, i, i + limbLen, base);

    this.imuln(limbPow);
    if (this.words[0] + word < 0x4000000)
      this.words[0] += word;
    else
      this._iaddn(word);
  }

  if (mod !== 0) {
    var pow = 1;
    var word = parseBase(number, i, number.length, base);

    for (var i = 0; i < mod; i++)
      pow *= base;
    this.imuln(pow);
    if (this.words[0] + word < 0x4000000)
      this.words[0] += word;
    else
      this._iaddn(word);
  }
};

BN.prototype.copy = function copy(dest) {
  dest.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    dest.words[i] = this.words[i];
  dest.length = this.length;
  dest.negative = this.negative;
};

BN.prototype.clone = function clone() {
  var r = new BN(null);
  this.copy(r);
  return r;
};

// Remove leading `0` from `this`
BN.prototype.strip = function strip() {
  while (this.length > 1 && this.words[this.length - 1] === 0)
    this.length--;
  return this._normSign();
};

BN.prototype._normSign = function _normSign() {
  // -0 = 0
  if (this.length === 1 && this.words[0] === 0)
    this.negative = 0;
  return this;
};

var zeros = [
  '',
  '0',
  '00',
  '000',
  '0000',
  '00000',
  '000000',
  '0000000',
  '00000000',
  '000000000',
  '0000000000',
  '00000000000',
  '000000000000',
  '0000000000000',
  '00000000000000',
  '000000000000000',
  '0000000000000000',
  '00000000000000000',
  '000000000000000000',
  '0000000000000000000',
  '00000000000000000000',
  '000000000000000000000',
  '0000000000000000000000',
  '00000000000000000000000',
  '000000000000000000000000',
  '0000000000000000000000000'
];

var groupSizes = [
  0, 0,
  25, 16, 12, 11, 10, 9, 8,
  8, 7, 7, 7, 7, 6, 6,
  6, 6, 6, 6, 6, 5, 5,
  5, 5, 5, 5, 5, 5, 5,
  5, 5, 5, 5, 5, 5, 5
];

var groupBases = [
  0, 0,
  33554432, 43046721, 16777216, 48828125, 60466176, 40353607, 16777216,
  43046721, 10000000, 19487171, 35831808, 62748517, 7529536, 11390625,
  16777216, 24137569, 34012224, 47045881, 64000000, 4084101, 5153632,
  6436343, 7962624, 9765625, 11881376, 14348907, 17210368, 20511149,
  24300000, 28629151, 33554432, 39135393, 45435424, 52521875, 60466176
];

BN.prototype.toString = function toString(base, padding) {
  base = base || 10;
  var padding = padding | 0 || 1;
  if (base === 16 || base === 'hex') {
    var out = '';
    var off = 0;
    var carry = 0;
    for (var i = 0; i < this.length; i++) {
      var w = this.words[i];
      var word = (((w << off) | carry) & 0xffffff).toString(16);
      carry = (w >>> (24 - off)) & 0xffffff;
      if (carry !== 0 || i !== this.length - 1)
        out = zeros[6 - word.length] + word + out;
      else
        out = word + out;
      off += 2;
      if (off >= 26) {
        off -= 26;
        i--;
      }
    }
    if (carry !== 0)
      out = carry.toString(16) + out;
    while (out.length % padding !== 0)
      out = '0' + out;
    if (this.negative !== 0)
      out = '-' + out;
    return out;
  } else if (base === (base | 0) && base >= 2 && base <= 36) {
    var groupSize = groupSizes[base];
    var groupBase = groupBases[base];
    var out = '';
    var c = this.clone();
    c.negative = 0;
    while (c.cmpn(0) !== 0) {
      var r = c.modn(groupBase).toString(base);
      c = c.idivn(groupBase);

      if (c.cmpn(0) !== 0)
        out = zeros[groupSize - r.length] + r + out;
      else
        out = r + out;
    }
    if (this.cmpn(0) === 0)
      out = '0' + out;
    while (out.length % padding !== 0)
      out = '0' + out;
    if (this.negative !== 0)
      out = '-' + out;
    return out;
  } else {
    throw 'Base should be between 2 and 36';
  }
};

BN.prototype.toJSON = function toJSON() {
  return this.toString(16);
};

BN.prototype.toArray = function toArray(endian, length) {
  this.strip();
  var littleEndian = endian === 'le';
  var res = new Array(this.byteLength());
  res[0] = 0;

  var q = this.clone();
  if (!littleEndian) {
    // Assume big-endian
    for (var i = 0; q.cmpn(0) !== 0; i++) {
      var b = q.andln(0xff);
      q.iushrn(8);

      res[res.length - i - 1] = b;
    }
  } else {
    for (var i = 0; q.cmpn(0) !== 0; i++) {
      var b = q.andln(0xff);
      q.iushrn(8);

      res[i] = b;
    }
  }

  if (length) {
    while (res.length < length) {
      if (littleEndian)
        res.push(0);
      else
        res.unshift(0);
    }
  }

  return res;
};

if (Math.clz32) {
  BN.prototype._countBits = function _countBits(w) {
    return 32 - Math.clz32(w);
  };
} else {
  BN.prototype._countBits = function _countBits(w) {
    var t = w;
    var r = 0;
    if (t >= 0x1000) {
      r += 13;
      t >>>= 13;
    }
    if (t >= 0x40) {
      r += 7;
      t >>>= 7;
    }
    if (t >= 0x8) {
      r += 4;
      t >>>= 4;
    }
    if (t >= 0x02) {
      r += 2;
      t >>>= 2;
    }
    return r + t;
  };
}

// Return number of used bits in a BN
BN.prototype.bitLength = function bitLength() {
  var hi = 0;
  var w = this.words[this.length - 1];
  var hi = this._countBits(w);
  return (this.length - 1) * 26 + hi;
};

BN.prototype.byteLength = function byteLength() {
  return Math.ceil(this.bitLength() / 8);
};

// Return negative clone of `this`
BN.prototype.neg = function neg() {
  if (this.cmpn(0) === 0)
    return this.clone();

  var r = this.clone();
  r.negative = this.negative ^ 1;
  return r;
};

BN.prototype.ineg = function ineg() {
  this.negative ^= 1;
  return this;
};

// Or `num` with `this` in-place
BN.prototype.iuor = function iuor(num) {
  while (this.length < num.length)
    this.words[this.length++] = 0;

  for (var i = 0; i < num.length; i++)
    this.words[i] = this.words[i] | num.words[i];

  return this.strip();
};

BN.prototype.ior = function ior(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuor(num);
};


// Or `num` with `this`
BN.prototype.or = function or(num) {
  if (this.length > num.length)
    return this.clone().ior(num);
  else
    return num.clone().ior(this);
};

BN.prototype.uor = function uor(num) {
  if (this.length > num.length)
    return this.clone().iuor(num);
  else
    return num.clone().iuor(this);
};


// And `num` with `this` in-place
BN.prototype.iuand = function iuand(num) {
  // b = min-length(num, this)
  var b;
  if (this.length > num.length)
    b = num;
  else
    b = this;

  for (var i = 0; i < b.length; i++)
    this.words[i] = this.words[i] & num.words[i];

  this.length = b.length;

  return this.strip();
};

BN.prototype.iand = function iand(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuand(num);
};


// And `num` with `this`
BN.prototype.and = function and(num) {
  if (this.length > num.length)
    return this.clone().iand(num);
  else
    return num.clone().iand(this);
};

BN.prototype.uand = function uand(num) {
  if (this.length > num.length)
    return this.clone().iuand(num);
  else
    return num.clone().iuand(this);
};


// Xor `num` with `this` in-place
BN.prototype.iuxor = function iuxor(num) {
  // a.length > b.length
  var a;
  var b;
  if (this.length > num.length) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  for (var i = 0; i < b.length; i++)
    this.words[i] = a.words[i] ^ b.words[i];

  if (this !== a)
    for (; i < a.length; i++)
      this.words[i] = a.words[i];

  this.length = a.length;

  return this.strip();
};

BN.prototype.ixor = function ixor(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuxor(num);
};


// Xor `num` with `this`
BN.prototype.xor = function xor(num) {
  if (this.length > num.length)
    return this.clone().ixor(num);
  else
    return num.clone().ixor(this);
};

BN.prototype.uxor = function uxor(num) {
  if (this.length > num.length)
    return this.clone().iuxor(num);
  else
    return num.clone().iuxor(this);
};


// Add `num` to `this` in-place
BN.prototype.iadd = function iadd(num) {
  // negative + positive
  if (this.negative !== 0 && num.negative === 0) {
    this.negative = 0;
    var r = this.isub(num);
    this.negative ^= 1;
    return this._normSign();

  // positive + negative
  } else if (this.negative === 0 && num.negative !== 0) {
    num.negative = 0;
    var r = this.isub(num);
    num.negative = 1;
    return r._normSign();
  }

  // a.length > b.length
  var a;
  var b;
  if (this.length > num.length) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  var carry = 0;
  for (var i = 0; i < b.length; i++) {
    var r = (a.words[i] | 0) + (b.words[i] | 0) + carry;
    this.words[i] = r & 0x3ffffff;
    carry = r >>> 26;
  }
  for (; carry !== 0 && i < a.length; i++) {
    var r = (a.words[i] | 0) + carry;
    this.words[i] = r & 0x3ffffff;
    carry = r >>> 26;
  }

  this.length = a.length;
  if (carry !== 0) {
    this.words[this.length] = carry;
    this.length++;
  // Copy the rest of the words
  } else if (a !== this) {
    for (; i < a.length; i++)
      this.words[i] = a.words[i];
  }

  return this;
};

// Add `num` to `this`
BN.prototype.add = function add(num) {
  if (num.negative !== 0 && this.negative === 0) {
    num.negative = 0;
    var res = this.sub(num);
    num.negative ^= 1;
    return res;
  } else if (num.negative === 0 && this.negative !== 0) {
    this.negative = 0;
    var res = num.sub(this);
    this.negative = 1;
    return res;
  }

  if (this.length > num.length)
    return this.clone().iadd(num);
  else
    return num.clone().iadd(this);
};

// Subtract `num` from `this` in-place
BN.prototype.isub = function isub(num) {
  // this - (-num) = this + num
  if (num.negative !== 0) {
    num.negative = 0;
    var r = this.iadd(num);
    num.negative = 1;
    return r._normSign();

  // -this - num = -(this + num)
  } else if (this.negative !== 0) {
    this.negative = 0;
    this.iadd(num);
    this.negative = 1;
    return this._normSign();
  }

  // At this point both numbers are positive
  var cmp = this.cmp(num);

  // Optimization - zeroify
  if (cmp === 0) {
    this.negative = 0;
    this.length = 1;
    this.words[0] = 0;
    return this;
  }

  // a > b
  var a;
  var b;
  if (cmp > 0) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  var carry = 0;
  for (var i = 0; i < b.length; i++) {
    var r = (a.words[i] | 0) - (b.words[i] | 0) + carry;
    carry = r >> 26;
    this.words[i] = r & 0x3ffffff;
  }
  for (; carry !== 0 && i < a.length; i++) {
    var r = (a.words[i] | 0) + carry;
    carry = r >> 26;
    this.words[i] = r & 0x3ffffff;
  }

  // Copy rest of the words
  if (carry === 0 && i < a.length && a !== this)
    for (; i < a.length; i++)
      this.words[i] = a.words[i];
  this.length = Math.max(this.length, i);

  if (a !== this)
    this.negative = 1;

  return this.strip();
};

// Subtract `num` from `this`
BN.prototype.sub = function sub(num) {
  return this.clone().isub(num);
};

function smallMulTo(self, num, out) {
  out.negative = num.negative ^ self.negative;
  var len = (self.length + num.length) | 0;
  out.length = len;
  len = (len - 1) | 0;

  // Peel one iteration (compiler can't do it, because of code complexity)
  var a = self.words[0] | 0;
  var b = num.words[0] | 0;
  var r = a * b;

  var lo = r & 0x3ffffff;
  var carry = (r / 0x4000000) | 0;
  out.words[0] = lo;

  for (var k = 1; k < len; k++) {
    // Sum all words with the same `i + j = k` and accumulate `ncarry`,
    // note that ncarry could be >= 0x3ffffff
    var ncarry = carry >>> 26;
    var rword = carry & 0x3ffffff;
    var maxJ = Math.min(k, num.length - 1);
    for (var j = Math.max(0, k - self.length + 1); j <= maxJ; j++) {
      var i = (k - j) | 0;
      var a = self.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      ncarry = (ncarry + ((r / 0x4000000) | 0)) | 0;
      lo = (lo + rword) | 0;
      rword = lo & 0x3ffffff;
      ncarry = (ncarry + (lo >>> 26)) | 0;
    }
    out.words[k] = rword | 0;
    carry = ncarry | 0;
  }
  if (carry !== 0) {
    out.words[k] = carry | 0;
  } else {
    out.length--;
  }

  return out.strip();
}

function bigMulTo(self, num, out) {
  out.negative = num.negative ^ self.negative;
  out.length = self.length + num.length;

  var carry = 0;
  var hncarry = 0;
  for (var k = 0; k < out.length - 1; k++) {
    // Sum all words with the same `i + j = k` and accumulate `ncarry`,
    // note that ncarry could be >= 0x3ffffff
    var ncarry = hncarry;
    hncarry = 0;
    var rword = carry & 0x3ffffff;
    var maxJ = Math.min(k, num.length - 1);
    for (var j = Math.max(0, k - self.length + 1); j <= maxJ; j++) {
      var i = k - j;
      var a = self.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      ncarry = (ncarry + ((r / 0x4000000) | 0)) | 0;
      lo = (lo + rword) | 0;
      rword = lo & 0x3ffffff;
      ncarry = (ncarry + (lo >>> 26)) | 0;

      hncarry += ncarry >>> 26;
      ncarry &= 0x3ffffff;
    }
    out.words[k] = rword;
    carry = ncarry;
    ncarry = hncarry;
  }
  if (carry !== 0) {
    out.words[k] = carry;
  } else {
    out.length--;
  }

  return out.strip();
}

BN.prototype.mulTo = function mulTo(num, out) {
  var res;
  if (this.length + num.length < 63)
    res = smallMulTo(this, num, out);
  else
    res = bigMulTo(this, num, out);
  return res;
};

// Multiply `this` by `num`
BN.prototype.mul = function mul(num) {
  var out = new BN(null);
  out.words = new Array(this.length + num.length);
  return this.mulTo(num, out);
};

// In-place Multiplication
BN.prototype.imul = function imul(num) {
  if (this.cmpn(0) === 0 || num.cmpn(0) === 0) {
    this.words[0] = 0;
    this.length = 1;
    return this;
  }

  var tlen = this.length;
  var nlen = num.length;

  this.negative = num.negative ^ this.negative;
  this.length = this.length + num.length;
  this.words[this.length - 1] = 0;

  for (var k = this.length - 2; k >= 0; k--) {
    // Sum all words with the same `i + j = k` and accumulate `carry`,
    // note that carry could be >= 0x3ffffff
    var carry = 0;
    var rword = 0;
    var maxJ = Math.min(k, nlen - 1);
    for (var j = Math.max(0, k - tlen + 1); j <= maxJ; j++) {
      var i = k - j;
      var a = this.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      carry += (r / 0x4000000) | 0;
      lo += rword;
      rword = lo & 0x3ffffff;
      carry += lo >>> 26;
    }
    this.words[k] = rword;
    this.words[k + 1] += carry;
    carry = 0;
  }

  // Propagate overflows
  var carry = 0;
  for (var i = 1; i < this.length; i++) {
    var w = (this.words[i] | 0) + carry;
    this.words[i] = w & 0x3ffffff;
    carry = w >>> 26;
  }

  return this.strip();
};

BN.prototype.imuln = function imuln(num) {
  // Carry
  var carry = 0;
  for (var i = 0; i < this.length; i++) {
    var w = (this.words[i] | 0) * num;
    var lo = (w & 0x3ffffff) + (carry & 0x3ffffff);
    carry >>= 26;
    carry += (w / 0x4000000) | 0;
    // NOTE: lo is 27bit maximum
    carry += lo >>> 26;
    this.words[i] = lo & 0x3ffffff;
  }

  if (carry !== 0) {
    this.words[i] = carry;
    this.length++;
  }

  return this;
};

BN.prototype.muln = function muln(num) {
  return this.clone().imuln(num);
};

// `this` * `this`
BN.prototype.sqr = function sqr() {
  return this.mul(this);
};

// `this` * `this` in-place
BN.prototype.isqr = function isqr() {
  return this.mul(this);
};

// Shift-left in-place
BN.prototype.iushln = function iushln(bits) {
  var r = bits % 26;
  var s = (bits - r) / 26;
  var carryMask = (0x3ffffff >>> (26 - r)) << (26 - r);

  if (r !== 0) {
    var carry = 0;
    for (var i = 0; i < this.length; i++) {
      var newCarry = this.words[i] & carryMask;
      var c = ((this.words[i] | 0) - newCarry) << r;
      this.words[i] = c | carry;
      carry = newCarry >>> (26 - r);
    }
    if (carry) {
      this.words[i] = carry;
      this.length++;
    }
  }

  if (s !== 0) {
    for (var i = this.length - 1; i >= 0; i--)
      this.words[i + s] = this.words[i];
    for (var i = 0; i < s; i++)
      this.words[i] = 0;
    this.length += s;
  }

  return this.strip();
};

BN.prototype.ishln = function ishln(bits) {
  return this.iushln(bits);
};

// Shift-right in-place
BN.prototype.iushrn = function iushrn(bits, hint, extended) {
  var h;
  if (hint)
    h = (hint - (hint % 26)) / 26;
  else
    h = 0;

  var r = bits % 26;
  var s = Math.min((bits - r) / 26, this.length);
  var mask = 0x3ffffff ^ ((0x3ffffff >>> r) << r);
  var maskedWords = extended;

  h -= s;
  h = Math.max(0, h);

  // Extended mode, copy masked part
  if (maskedWords) {
    for (var i = 0; i < s; i++)
      maskedWords.words[i] = this.words[i];
    maskedWords.length = s;
  }

  if (s === 0) {
    // No-op, we should not move anything at all
  } else if (this.length > s) {
    this.length -= s;
    for (var i = 0; i < this.length; i++)
      this.words[i] = this.words[i + s];
  } else {
    this.words[0] = 0;
    this.length = 1;
  }

  var carry = 0;
  for (var i = this.length - 1; i >= 0 && (carry !== 0 || i >= h); i--) {
    var word = this.words[i] | 0;
    this.words[i] = (carry << (26 - r)) | (word >>> r);
    carry = word & mask;
  }

  // Push carried bits as a mask
  if (maskedWords && carry !== 0)
    maskedWords.words[maskedWords.length++] = carry;

  if (this.length === 0) {
    this.words[0] = 0;
    this.length = 1;
  }

  this.strip();

  return this;
};

BN.prototype.ishrn = function ishrn(bits, hint, extended) {
  return this.iushrn(bits, hint, extended);
};

// Shift-left
BN.prototype.shln = function shln(bits) {
  var x = this.clone();
  var neg = x.negative;
  x.negative = false;
  x.ishln(bits);
  x.negative = neg;
  return x;
};

BN.prototype.ushln = function ushln(bits) {
  return this.clone().iushln(bits);
};

// Shift-right
BN.prototype.shrn = function shrn(bits) {
  var x = this.clone();
  if(x.negative) {
      x.negative = false;
      x.ishrn(bits);
      x.negative = true;
      return x.isubn(1);
  } else {
      return x.ishrn(bits);
  }
};

BN.prototype.ushrn = function ushrn(bits) {
  return this.clone().iushrn(bits);
};

// Test if n bit is set
BN.prototype.testn = function testn(bit) {
  var r = bit % 26;
  var s = (bit - r) / 26;
  var q = 1 << r;

  // Fast case: bit is much higher than all existing words
  if (this.length <= s) {
    return false;
  }

  // Check bit and return
  var w = this.words[s];

  return !!(w & q);
};

// Add plain number `num` to `this`
BN.prototype.iaddn = function iaddn(num) {
  if (num < 0)
    return this.isubn(-num);

  // Possible sign change
  if (this.negative !== 0) {
    if (this.length === 1 && (this.words[0] | 0) < num) {
      this.words[0] = num - (this.words[0] | 0);
      this.negative = 0;
      return this;
    }

    this.negative = 0;
    this.isubn(num);
    this.negative = 1;
    return this;
  }

  // Add without checks
  return this._iaddn(num);
};

BN.prototype._iaddn = function _iaddn(num) {
  this.words[0] += num;

  // Carry
  for (var i = 0; i < this.length && this.words[i] >= 0x4000000; i++) {
    this.words[i] -= 0x4000000;
    if (i === this.length - 1)
      this.words[i + 1] = 1;
    else
      this.words[i + 1]++;
  }
  this.length = Math.max(this.length, i + 1);

  return this;
};

// Subtract plain number `num` from `this`
BN.prototype.isubn = function isubn(num) {
  if (num < 0)
    return this.iaddn(-num);

  if (this.negative !== 0) {
    this.negative = 0;
    this.iaddn(num);
    this.negative = 1;
    return this;
  }

  this.words[0] -= num;

  // Carry
  for (var i = 0; i < this.length && this.words[i] < 0; i++) {
    this.words[i] += 0x4000000;
    this.words[i + 1] -= 1;
  }

  return this.strip();
};

BN.prototype.addn = function addn(num) {
  return this.clone().iaddn(num);
};

BN.prototype.subn = function subn(num) {
  return this.clone().isubn(num);
};

BN.prototype.iabs = function iabs() {
  this.negative = 0;

  return this;
};

BN.prototype.abs = function abs() {
  return this.clone().iabs();
};

BN.prototype._ishlnsubmul = function _ishlnsubmul(num, mul, shift) {
  // Bigger storage is needed
  var len = num.length + shift;
  var i;
  if (this.words.length < len) {
    var t = new Array(len);
    for (var i = 0; i < this.length; i++)
      t[i] = this.words[i];
    this.words = t;
  } else {
    i = this.length;
  }

  // Zeroify rest
  this.length = Math.max(this.length, len);
  for (; i < this.length; i++)
    this.words[i] = 0;

  var carry = 0;
  for (var i = 0; i < num.length; i++) {
    var w = (this.words[i + shift] | 0) + carry;
    var right = (num.words[i] | 0) * mul;
    w -= right & 0x3ffffff;
    carry = (w >> 26) - ((right / 0x4000000) | 0);
    this.words[i + shift] = w & 0x3ffffff;
  }
  for (; i < this.length - shift; i++) {
    var w = (this.words[i + shift] | 0) + carry;
    carry = w >> 26;
    this.words[i + shift] = w & 0x3ffffff;
  }

  if (carry === 0)
    return this.strip();

  carry = 0;
  for (var i = 0; i < this.length; i++) {
    var w = -(this.words[i] | 0) + carry;
    carry = w >> 26;
    this.words[i] = w & 0x3ffffff;
  }
  this.negative = 1;

  return this.strip();
};

BN.prototype._wordDiv = function _wordDiv(num, mode) {
  var shift = this.length - num.length;

  var a = this.clone();
  var b = num;

  // Normalize
  var bhi = b.words[b.length - 1] | 0;
  var bhiBits = this._countBits(bhi);
  shift = 26 - bhiBits;
  if (shift !== 0) {
    b = b.ushln(shift);
    a.iushln(shift);
    bhi = b.words[b.length - 1] | 0;
  }

  // Initialize quotient
  var m = a.length - b.length;
  var q;

  if (mode !== 'mod') {
    q = new BN(null);
    q.length = m + 1;
    q.words = new Array(q.length);
    for (var i = 0; i < q.length; i++)
      q.words[i] = 0;
  }

  var diff = a.clone()._ishlnsubmul(b, 1, m);
  if (diff.negative === 0) {
    a = diff;
    if (q)
      q.words[m] = 1;
  }

  for (var j = m - 1; j >= 0; j--) {
    var qj = (a.words[b.length + j] | 0) * 0x4000000 +
             (a.words[b.length + j - 1] | 0);

    // NOTE: (qj / bhi) is (0x3ffffff * 0x4000000 + 0x3ffffff) / 0x2000000 max
    // (0x7ffffff)
    qj = Math.min((qj / bhi) | 0, 0x3ffffff);

    a._ishlnsubmul(b, qj, j);
    while (a.negative !== 0) {
      qj--;
      a.negative = 0;
      a._ishlnsubmul(b, 1, j);
      if (a.cmpn(0) !== 0)
        a.negative ^= 1;
    }
    if (q)
      q.words[j] = qj;
  }
  if (q)
    q.strip();
  a.strip();

  // Denormalize
  if (mode !== 'div' && shift !== 0)
    a.iushrn(shift);
  return { div: q ? q : null, mod: a };
};

BN.prototype.divmod = function divmod(num, mode, positive) {
  if (this.negative !== 0 && num.negative === 0) {
    var res = this.neg().divmod(num, mode);
    var div;
    var mod;
    if (mode !== 'mod')
      div = res.div.neg();
    if (mode !== 'div') {
      mod = res.mod.neg();
      if (positive && mod.neg)
        mod = mod.add(num);
    }
    return {
      div: div,
      mod: mod
    };
  } else if (this.negative === 0 && num.negative !== 0) {
    var res = this.divmod(num.neg(), mode);
    var div;
    if (mode !== 'mod')
      div = res.div.neg();
    return { div: div, mod: res.mod };
  } else if ((this.negative & num.negative) !== 0) {
    var res = this.neg().divmod(num.neg(), mode);
    var mod;
    if (mode !== 'div') {
      mod = res.mod.neg();
      if (positive && mod.neg)
        mod = mod.isub(num);
    }
    return {
      div: res.div,
      mod: mod
    };
  }

  // Both numbers are positive at this point

  // Strip both numbers to approximate shift value
  if (num.length > this.length || this.cmp(num) < 0)
    return { div: new BN(0), mod: this };

  // Very short reduction
  if (num.length === 1) {
    if (mode === 'div')
      return { div: this.divn(num.words[0]), mod: null };
    else if (mode === 'mod')
      return { div: null, mod: new BN(this.modn(num.words[0])) };
    return {
      div: this.divn(num.words[0]),
      mod: new BN(this.modn(num.words[0]))
    };
  }

  return this._wordDiv(num, mode);
};

// Find `this` / `num`
BN.prototype.div = function div(num) {
  return this.divmod(num, 'div', false).div;
};

// Find `this` % `num`
BN.prototype.mod = function mod(num) {
  return this.divmod(num, 'mod', false).mod;
};

BN.prototype.umod = function umod(num) {
  return this.divmod(num, 'mod', true).mod;
};

// Find Round(`this` / `num`)
BN.prototype.divRound = function divRound(num) {
  var dm = this.divmod(num);

  // Fast case - exact division
  if (dm.mod.cmpn(0) === 0)
    return dm.div;

  var mod = dm.div.negative !== 0 ? dm.mod.isub(num) : dm.mod;

  var half = num.ushrn(1);
  var r2 = num.andln(1);
  var cmp = mod.cmp(half);

  // Round down
  if (cmp < 0 || r2 === 1 && cmp === 0)
    return dm.div;

  // Round up
  return dm.div.negative !== 0 ? dm.div.isubn(1) : dm.div.iaddn(1);
};

BN.prototype.modn = function modn(num) {
  var p = (1 << 26) % num;

  var acc = 0;
  for (var i = this.length - 1; i >= 0; i--)
    acc = (p * acc + (this.words[i] | 0)) % num;

  return acc;
};

// In-place division by number
BN.prototype.idivn = function idivn(num) {
  var carry = 0;
  for (var i = this.length - 1; i >= 0; i--) {
    var w = (this.words[i] | 0) + carry * 0x4000000;
    this.words[i] = (w / num) | 0;
    carry = w % num;
  }

  return this.strip();
};

BN.prototype.divn = function divn(num) {
  return this.clone().idivn(num);
};

BN.prototype.isEven = function isEven() {
  return (this.words[0] & 1) === 0;
};

BN.prototype.isOdd = function isOdd() {
  return (this.words[0] & 1) === 1;
};

// And first word and num
BN.prototype.andln = function andln(num) {
  return this.words[0] & num;
};

BN.prototype.cmpn = function cmpn(num) {
  var negative = num < 0;
  if (negative)
    num = -num;

  if (this.negative !== 0 && !negative)
    return -1;
  else if (this.negative === 0 && negative)
    return 1;

  num &= 0x3ffffff;
  this.strip();

  var res;
  if (this.length > 1) {
    res = 1;
  } else {
    var w = this.words[0] | 0;
    res = w === num ? 0 : w < num ? -1 : 1;
  }
  if (this.negative !== 0)
    res = -res;
  return res;
};

// Compare two numbers and return:
// 1 - if `this` > `num`
// 0 - if `this` == `num`
// -1 - if `this` < `num`
BN.prototype.cmp = function cmp(num) {
  if (this.negative !== 0 && num.negative === 0)
    return -1;
  else if (this.negative === 0 && num.negative !== 0)
    return 1;

  var res = this.ucmp(num);
  if (this.negative !== 0)
    return -res;
  else
    return res;
};

// Unsigned comparison
BN.prototype.ucmp = function ucmp(num) {
  // At this point both numbers have the same sign
  if (this.length > num.length)
    return 1;
  else if (this.length < num.length)
    return -1;

  var res = 0;
  for (var i = this.length - 1; i >= 0; i--) {
    var a = this.words[i] | 0;
    var b = num.words[i] | 0;

    if (a === b)
      continue;
    if (a < b)
      res = -1;
    else if (a > b)
      res = 1;
    break;
  }
  return res;
};
})(undefined, __bn);

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return {_:0, a:0, b:undefined};
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return {_:0, a:1, b:val};
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;
var __stable_table;

function makeStableName(x) {
    if(x instanceof Object) {
        if(!x.stableName) {
            x.stableName = __next_stable_name;
            __next_stable_name += 1;
        }
        return {type: 'obj', name: x.stableName};
    } else {
        return {type: 'prim', name: Number(x)};
    }
}

function eqStableName(x, y) {
    return (x.type == y.type && x.name == y.name) ? 1 : 0;
}

// TODO: inefficient compared to real fromInt?
__bn.Z = new __bn.BN(0);
__bn.ONE = new __bn.BN(1);
__bn.MOD32 = new __bn.BN(0x100000000); // 2^32
var I_fromNumber = function(x) {return new __bn.BN(x);}
var I_fromInt = I_fromNumber;
var I_fromBits = function(lo,hi) {
    var x = new __bn.BN(lo >>> 0);
    var y = new __bn.BN(hi >>> 0);
    y.ishln(32);
    x.iadd(y);
    return x;
}
var I_fromString = function(s) {return new __bn.BN(s);}
var I_toInt = function(x) {return I_toNumber(x.mod(__bn.MOD32));}
var I_toWord = function(x) {return I_toInt(x) >>> 0;};
// TODO: inefficient!
var I_toNumber = function(x) {return Number(x.toString());}
var I_equals = function(a,b) {return a.cmp(b) === 0;}
var I_compare = function(a,b) {return a.cmp(b);}
var I_compareInt = function(x,i) {return x.cmp(new __bn.BN(i));}
var I_negate = function(x) {return x.neg();}
var I_add = function(a,b) {return a.add(b);}
var I_sub = function(a,b) {return a.sub(b);}
var I_mul = function(a,b) {return a.mul(b);}
var I_mod = function(a,b) {return I_rem(I_add(b, I_rem(a, b)), b);}
var I_quotRem = function(a,b) {
    var qr = a.divmod(b);
    return {_:0, a:qr.div, b:qr.mod};
}
var I_div = function(a,b) {
    if((a.cmp(__bn.Z)>=0) != (a.cmp(__bn.Z)>=0)) {
        if(a.cmp(a.rem(b), __bn.Z) !== 0) {
            return a.div(b).sub(__bn.ONE);
        }
    }
    return a.div(b);
}
var I_divMod = function(a,b) {
    return {_:0, a:I_div(a,b), b:a.mod(b)};
}
var I_quot = function(a,b) {return a.div(b);}
var I_rem = function(a,b) {return a.mod(b);}
var I_and = function(a,b) {return a.and(b);}
var I_or = function(a,b) {return a.or(b);}
var I_xor = function(a,b) {return a.xor(b);}
var I_shiftLeft = function(a,b) {return a.shln(b);}
var I_shiftRight = function(a,b) {return a.shrn(b);}
var I_signum = function(x) {return x.cmp(new __bn.BN(0));}
var I_abs = function(x) {return x.abs();}
var I_decodeDouble = function(x) {
    var dec = decodeDouble(x);
    var mantissa = I_fromBits(dec.c, dec.b);
    if(dec.a < 0) {
        mantissa = I_negate(mantissa);
    }
    return {_:0, a:dec.d, b:mantissa};
}
var I_toString = function(x) {return x.toString();}
var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    if(x.isNegative()) {
        return I_negate(I_fromInt64(x.negate()));
    } else {
        return I_fromBits(x.low, x.high);
    }
}

function I_toInt64(x) {
    if(x.negative) {
        return I_toInt64(I_negate(x)).negate();
    } else {
        return new Long(I_toInt(x), I_toInt(I_shiftRight(x,32)));
    }
}

function I_fromWord64(x) {
    return I_fromBits(x.toInt(), x.shru(32).toInt());
}

function I_toWord64(x) {
    var w = I_toInt64(x);
    w.unsigned = true;
    return w;
}

/**
 * @license long.js (c) 2013 Daniel Wirtz <dcode@dcode.io>
 * Released under the Apache License, Version 2.0
 * see: https://github.com/dcodeIO/long.js for details
 */
function Long(low, high, unsigned) {
    this.low = low | 0;
    this.high = high | 0;
    this.unsigned = !!unsigned;
}

var INT_CACHE = {};
var UINT_CACHE = {};
function cacheable(x, u) {
    return u ? 0 <= (x >>>= 0) && x < 256 : -128 <= (x |= 0) && x < 128;
}

function __fromInt(value, unsigned) {
    var obj, cachedObj, cache;
    if (unsigned) {
        if (cache = cacheable(value >>>= 0, true)) {
            cachedObj = UINT_CACHE[value];
            if (cachedObj)
                return cachedObj;
        }
        obj = new Long(value, (value | 0) < 0 ? -1 : 0, true);
        if (cache)
            UINT_CACHE[value] = obj;
        return obj;
    } else {
        if (cache = cacheable(value |= 0, false)) {
            cachedObj = INT_CACHE[value];
            if (cachedObj)
                return cachedObj;
        }
        obj = new Long(value, value < 0 ? -1 : 0, false);
        if (cache)
            INT_CACHE[value] = obj;
        return obj;
    }
}

function __fromNumber(value, unsigned) {
    if (isNaN(value) || !isFinite(value))
        return unsigned ? UZERO : ZERO;
    if (unsigned) {
        if (value < 0)
            return UZERO;
        if (value >= TWO_PWR_64_DBL)
            return MAX_UNSIGNED_VALUE;
    } else {
        if (value <= -TWO_PWR_63_DBL)
            return MIN_VALUE;
        if (value + 1 >= TWO_PWR_63_DBL)
            return MAX_VALUE;
    }
    if (value < 0)
        return __fromNumber(-value, unsigned).neg();
    return new Long((value % TWO_PWR_32_DBL) | 0, (value / TWO_PWR_32_DBL) | 0, unsigned);
}
var pow_dbl = Math.pow;
var TWO_PWR_16_DBL = 1 << 16;
var TWO_PWR_24_DBL = 1 << 24;
var TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL;
var TWO_PWR_64_DBL = TWO_PWR_32_DBL * TWO_PWR_32_DBL;
var TWO_PWR_63_DBL = TWO_PWR_64_DBL / 2;
var TWO_PWR_24 = __fromInt(TWO_PWR_24_DBL);
var ZERO = __fromInt(0);
Long.ZERO = ZERO;
var UZERO = __fromInt(0, true);
Long.UZERO = UZERO;
var ONE = __fromInt(1);
Long.ONE = ONE;
var UONE = __fromInt(1, true);
Long.UONE = UONE;
var NEG_ONE = __fromInt(-1);
Long.NEG_ONE = NEG_ONE;
var MAX_VALUE = new Long(0xFFFFFFFF|0, 0x7FFFFFFF|0, false);
Long.MAX_VALUE = MAX_VALUE;
var MAX_UNSIGNED_VALUE = new Long(0xFFFFFFFF|0, 0xFFFFFFFF|0, true);
Long.MAX_UNSIGNED_VALUE = MAX_UNSIGNED_VALUE;
var MIN_VALUE = new Long(0, 0x80000000|0, false);
Long.MIN_VALUE = MIN_VALUE;
var __lp = Long.prototype;
__lp.toInt = function() {return this.unsigned ? this.low >>> 0 : this.low;};
__lp.toNumber = function() {
    if (this.unsigned)
        return ((this.high >>> 0) * TWO_PWR_32_DBL) + (this.low >>> 0);
    return this.high * TWO_PWR_32_DBL + (this.low >>> 0);
};
__lp.isZero = function() {return this.high === 0 && this.low === 0;};
__lp.isNegative = function() {return !this.unsigned && this.high < 0;};
__lp.isOdd = function() {return (this.low & 1) === 1;};
__lp.eq = function(other) {
    if (this.unsigned !== other.unsigned && (this.high >>> 31) === 1 && (other.high >>> 31) === 1)
        return false;
    return this.high === other.high && this.low === other.low;
};
__lp.neq = function(other) {return !this.eq(other);};
__lp.lt = function(other) {return this.comp(other) < 0;};
__lp.lte = function(other) {return this.comp(other) <= 0;};
__lp.gt = function(other) {return this.comp(other) > 0;};
__lp.gte = function(other) {return this.comp(other) >= 0;};
__lp.compare = function(other) {
    if (this.eq(other))
        return 0;
    var thisNeg = this.isNegative(),
        otherNeg = other.isNegative();
    if (thisNeg && !otherNeg)
        return -1;
    if (!thisNeg && otherNeg)
        return 1;
    if (!this.unsigned)
        return this.sub(other).isNegative() ? -1 : 1;
    return (other.high >>> 0) > (this.high >>> 0) || (other.high === this.high && (other.low >>> 0) > (this.low >>> 0)) ? -1 : 1;
};
__lp.comp = __lp.compare;
__lp.negate = function() {
    if (!this.unsigned && this.eq(MIN_VALUE))
        return MIN_VALUE;
    return this.not().add(ONE);
};
__lp.neg = __lp.negate;
__lp.add = function(addend) {
    var a48 = this.high >>> 16;
    var a32 = this.high & 0xFFFF;
    var a16 = this.low >>> 16;
    var a00 = this.low & 0xFFFF;

    var b48 = addend.high >>> 16;
    var b32 = addend.high & 0xFFFF;
    var b16 = addend.low >>> 16;
    var b00 = addend.low & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 + b00;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 + b16;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 + b32;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 + b48;
    c48 &= 0xFFFF;
    return new Long((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
};
__lp.subtract = function(subtrahend) {return this.add(subtrahend.neg());};
__lp.sub = __lp.subtract;
__lp.multiply = function(multiplier) {
    if (this.isZero())
        return ZERO;
    if (multiplier.isZero())
        return ZERO;
    if (this.eq(MIN_VALUE))
        return multiplier.isOdd() ? MIN_VALUE : ZERO;
    if (multiplier.eq(MIN_VALUE))
        return this.isOdd() ? MIN_VALUE : ZERO;

    if (this.isNegative()) {
        if (multiplier.isNegative())
            return this.neg().mul(multiplier.neg());
        else
            return this.neg().mul(multiplier).neg();
    } else if (multiplier.isNegative())
        return this.mul(multiplier.neg()).neg();

    if (this.lt(TWO_PWR_24) && multiplier.lt(TWO_PWR_24))
        return __fromNumber(this.toNumber() * multiplier.toNumber(), this.unsigned);

    var a48 = this.high >>> 16;
    var a32 = this.high & 0xFFFF;
    var a16 = this.low >>> 16;
    var a00 = this.low & 0xFFFF;

    var b48 = multiplier.high >>> 16;
    var b32 = multiplier.high & 0xFFFF;
    var b16 = multiplier.low >>> 16;
    var b00 = multiplier.low & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 * b00;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 * b00;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c16 += a00 * b16;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 * b00;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c32 += a16 * b16;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c32 += a00 * b32;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
    c48 &= 0xFFFF;
    return new Long((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
};
__lp.mul = __lp.multiply;
__lp.divide = function(divisor) {
    if (divisor.isZero())
        throw Error('division by zero');
    if (this.isZero())
        return this.unsigned ? UZERO : ZERO;
    var approx, rem, res;
    if (this.eq(MIN_VALUE)) {
        if (divisor.eq(ONE) || divisor.eq(NEG_ONE))
            return MIN_VALUE;
        else if (divisor.eq(MIN_VALUE))
            return ONE;
        else {
            var halfThis = this.shr(1);
            approx = halfThis.div(divisor).shl(1);
            if (approx.eq(ZERO)) {
                return divisor.isNegative() ? ONE : NEG_ONE;
            } else {
                rem = this.sub(divisor.mul(approx));
                res = approx.add(rem.div(divisor));
                return res;
            }
        }
    } else if (divisor.eq(MIN_VALUE))
        return this.unsigned ? UZERO : ZERO;
    if (this.isNegative()) {
        if (divisor.isNegative())
            return this.neg().div(divisor.neg());
        return this.neg().div(divisor).neg();
    } else if (divisor.isNegative())
        return this.div(divisor.neg()).neg();

    res = ZERO;
    rem = this;
    while (rem.gte(divisor)) {
        approx = Math.max(1, Math.floor(rem.toNumber() / divisor.toNumber()));
        var log2 = Math.ceil(Math.log(approx) / Math.LN2),
            delta = (log2 <= 48) ? 1 : pow_dbl(2, log2 - 48),
            approxRes = __fromNumber(approx),
            approxRem = approxRes.mul(divisor);
        while (approxRem.isNegative() || approxRem.gt(rem)) {
            approx -= delta;
            approxRes = __fromNumber(approx, this.unsigned);
            approxRem = approxRes.mul(divisor);
        }
        if (approxRes.isZero())
            approxRes = ONE;

        res = res.add(approxRes);
        rem = rem.sub(approxRem);
    }
    return res;
};
__lp.div = __lp.divide;
__lp.modulo = function(divisor) {return this.sub(this.div(divisor).mul(divisor));};
__lp.mod = __lp.modulo;
__lp.not = function not() {return new Long(~this.low, ~this.high, this.unsigned);};
__lp.and = function(other) {return new Long(this.low & other.low, this.high & other.high, this.unsigned);};
__lp.or = function(other) {return new Long(this.low | other.low, this.high | other.high, this.unsigned);};
__lp.xor = function(other) {return new Long(this.low ^ other.low, this.high ^ other.high, this.unsigned);};

__lp.shl = function(numBits) {
    if ((numBits &= 63) === 0)
        return this;
    else if (numBits < 32)
        return new Long(this.low << numBits, (this.high << numBits) | (this.low >>> (32 - numBits)), this.unsigned);
    else
        return new Long(0, this.low << (numBits - 32), this.unsigned);
};

__lp.shr = function(numBits) {
    if ((numBits &= 63) === 0)
        return this;
    else if (numBits < 32)
        return new Long((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >> numBits, this.unsigned);
    else
        return new Long(this.high >> (numBits - 32), this.high >= 0 ? 0 : -1, this.unsigned);
};

__lp.shru = function(numBits) {
    numBits &= 63;
    if (numBits === 0)
        return this;
    else {
        var high = this.high;
        if (numBits < 32) {
            var low = this.low;
            return new Long((low >>> numBits) | (high << (32 - numBits)), high >>> numBits, this.unsigned);
        } else if (numBits === 32)
            return new Long(high, 0, this.unsigned);
        else
            return new Long(high >>> (numBits - 32), 0, this.unsigned);
    }
};

__lp.toSigned = function() {return this.unsigned ? new Long(this.low, this.high, false) : this;};
__lp.toUnsigned = function() {return this.unsigned ? this : new Long(this.low, this.high, true);};

// Int64
function hs_eqInt64(x, y) {return x.eq(y);}
function hs_neInt64(x, y) {return x.neq(y);}
function hs_ltInt64(x, y) {return x.lt(y);}
function hs_leInt64(x, y) {return x.lte(y);}
function hs_gtInt64(x, y) {return x.gt(y);}
function hs_geInt64(x, y) {return x.gte(y);}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shl(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shr(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shru(bits);}
function hs_int64ToInt(x) {return x.toInt();}
var hs_intToInt64 = __fromInt;

// Word64
function hs_wordToWord64(x) {return __fromInt(x, true);}
function hs_word64ToWord(x) {return x.toInt(x);}
function hs_mkWord64(low, high) {return new Long(low,high,true);}
function hs_and64(a,b) {return a.and(b);};
function hs_or64(a,b) {return a.or(b);};
function hs_xor64(a,b) {return a.xor(b);};
function hs_not64(x) {return x.not();}
var hs_eqWord64 = hs_eqInt64;
var hs_neWord64 = hs_neInt64;
var hs_ltWord64 = hs_ltInt64;
var hs_leWord64 = hs_leInt64;
var hs_gtWord64 = hs_gtInt64;
var hs_geWord64 = hs_geInt64;
var hs_quotWord64 = hs_quotInt64;
var hs_remWord64 = hs_remInt64;
var hs_uncheckedShiftL64 = hs_uncheckedIShiftL64;
var hs_uncheckedShiftRL64 = hs_uncheckedIShiftRL64;
function hs_int64ToWord64(x) {return x.toUnsigned();}
function hs_word64ToInt64(x) {return x.toSigned();}

// Joseph Myers' MD5 implementation, ported to work on typed arrays.
// Used under the BSD license.
function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s, n) {
    var a = s['v']['w8'];
    var orig_n = n,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=n; i+=64) {
        md5cycle(state, md5blk(a.subarray(i-64, i)));
    }
    a = a.subarray(i-64);
    n = n < (i-64) ? 0 : n-(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<n; i++)
        tail[i>>2] |= a[i] << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = orig_n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s[i]
            + (s[i+1] << 8)
            + (s[i+2] << 16)
            + (s[i+3] << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s, n) {
    return hex(md51(s, n));
}

window['md5'] = md5;

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

function __hsbase_MD5Init(ctx) {}
// Note that this is a one time "update", since that's all that's used by
// GHC.Fingerprint.
function __hsbase_MD5Update(ctx, s, n) {
    ctx.md5 = md51(s, n);
}
function __hsbase_MD5Final(out, ctx) {
    var a = out['v']['i32'];
    a[0] = ctx.md5[0];
    a[1] = ctx.md5[1];
    a[2] = ctx.md5[2];
    a[3] = ctx.md5[3];
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = new Array(n);
    for(var i = 0; i < n; ++i) {
        arr[i] = x;
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    return new ByteArray(new ArrayBuffer(n));
}

// Wrap a JS ArrayBuffer into a ByteArray. Truncates the array length to the
// closest multiple of 8 bytes.
function wrapByteArr(buffer) {
    var diff = buffer.byteLength % 8;
    if(diff != 0) {
        var buffer = buffer.slice(0, buffer.byteLength-diff);
    }
    return new ByteArray(buffer);
}

function ByteArray(buffer) {
    var len = buffer.byteLength;
    var views =
        { 'i8' : new Int8Array(buffer)
        , 'i16': len % 2 ? null : new Int16Array(buffer)
        , 'i32': len % 4 ? null : new Int32Array(buffer)
        , 'w8' : new Uint8Array(buffer)
        , 'w16': len % 2 ? null : new Uint16Array(buffer)
        , 'w32': len % 4 ? null : new Uint32Array(buffer)
        , 'f32': len % 4 ? null : new Float32Array(buffer)
        , 'f64': len % 8 ? null : new Float64Array(buffer)
        };
    this['b'] = buffer;
    this['v'] = views;
    this['off'] = 0;
}
window['newArr'] = newArr;
window['newByteArr'] = newByteArr;
window['wrapByteArr'] = wrapByteArr;
window['ByteArray'] = ByteArray;

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function writeOffAddr64(addr, off, x) {
    addr['v']['w32'][addr.off/8 + off*2] = x.low;
    addr['v']['w32'][addr.off/8 + off*2 + 1] = x.high;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

function readOffAddr64(signed, addr, off) {
    var w64 = hs_mkWord64( addr['v']['w32'][addr.off/8 + off*2]
                         , addr['v']['w32'][addr.off/8 + off*2 + 1]);
    return signed ? hs_word64ToInt64(w64) : w64;
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return {_:0, a:1, b:E(w).val};
}

function finalizeWeak(w) {
    return {_:0, a:B(A1(E(w).fin, __Z))};
}

/* For foreign import ccall "wrapper" */
function createAdjustor(args, f, a, b) {
    return function(){
        var x = f.apply(null, arguments);
        while(x instanceof F) {x = x.f();}
        return x;
    };
}

var __apply = function(f,as) {
    var arr = [];
    for(; as._ === 1; as = as.b) {
        arr.push(as.a);
    }
    arr.reverse();
    return f.apply(null, arr);
}
var __app0 = function(f) {return f();}
var __app1 = function(f,a) {return f(a);}
var __app2 = function(f,a,b) {return f(a,b);}
var __app3 = function(f,a,b,c) {return f(a,b,c);}
var __app4 = function(f,a,b,c,d) {return f(a,b,c,d);}
var __app5 = function(f,a,b,c,d,e) {return f(a,b,c,d,e);}
var __jsNull = function() {return null;}
var __isUndef = function(x) {return typeof x == 'undefined';}
var __eq = function(a,b) {return a===b;}
var __createJSFunc = function(arity, f){
    if(f instanceof Function && arity === f.length) {
        return (function() {
            var x = f.apply(null,arguments);
            if(x instanceof T) {
                if(x.f !== __blackhole) {
                    var ff = x.f;
                    x.f = __blackhole;
                    return x.x = ff();
                }
                return x.x;
            } else {
                while(x instanceof F) {
                    x = x.f();
                }
                return E(x);
            }
        });
    } else {
        return (function(){
            var as = Array.prototype.slice.call(arguments);
            as.push(0);
            return E(B(A(f,as)));
        });
    }
}


function __arr2lst(elem,arr) {
    if(elem >= arr.length) {
        return __Z;
    }
    return {_:1,
            a:arr[elem],
            b:new T(function(){return __arr2lst(elem+1,arr);})};
}

function __lst2arr(xs) {
    var arr = [];
    xs = E(xs);
    for(;xs._ === 1; xs = E(xs.b)) {
        arr.push(E(xs.a));
    }
    return arr;
}

var __new = function() {return ({});}
var __set = function(o,k,v) {o[k]=v;}
var __get = function(o,k) {return o[k];}
var __has = function(o,k) {return o[k]!==undefined;}

/* Code for creating and querying the static pointer table. */
window.__hs_spt = [];

function __spt_insert(ptr) {
    ptr = E(B(ptr));
    var ks = [ (ptr.a.a.low>>>0).toString(16)
             , (ptr.a.a.high>>>0).toString(16)
             , (ptr.a.b.low>>>0).toString(16)
             , (ptr.a.b.high>>>0).toString(16)
             ]
    var key = ks.join();
    window.__hs_spt[key] = ptr;
}

function hs_spt_lookup(k) {
    var ks = [ k['v']['w32'][0].toString(16)
             , k['v']['w32'][1].toString(16)
             , k['v']['w32'][2].toString(16)
             , k['v']['w32'][3].toString(16)
             ]
    var key = ks.join();
    return window.__hs_spt[key];
}

var _0/* $fExceptionNestedAtomically_ww2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("base"));
}),
_1/* $fExceptionNestedAtomically_ww4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Control.Exception.Base"));
}),
_2/* $fExceptionPatternMatchFail_ww5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PatternMatchFail"));
}),
_3/* $fExceptionPatternMatchFail_wild */ = new T5(0,new Long/* EXTERNAL */(18445595, 3739165398, true),new Long/* EXTERNAL */(52003073, 3246954884, true),_0/* Control.Exception.Base.$fExceptionNestedAtomically_ww2 */,_1/* Control.Exception.Base.$fExceptionNestedAtomically_ww4 */,_2/* Control.Exception.Base.$fExceptionPatternMatchFail_ww5 */),
_4/* [] */ = __Z/* EXTERNAL */,
_5/* $fExceptionPatternMatchFail2 */ = new T5(0,new Long/* EXTERNAL */(18445595, 3739165398, true),new Long/* EXTERNAL */(52003073, 3246954884, true),_3/* Control.Exception.Base.$fExceptionPatternMatchFail_wild */,_4/* GHC.Types.[] */,_4/* GHC.Types.[] */),
_6/* $fExceptionPatternMatchFail1 */ = function(_7/* s4EcJ */){
  return E(_5/* Control.Exception.Base.$fExceptionPatternMatchFail2 */);
},
_8/* $p1Exception */ = function(_9/* s2V9U */){
  return E(E(_9/* s2V9U */).a);
},
_a/* cast */ = function(_b/* s295g */, _c/* s295h */, _d/* s295i */){
  var _e/* s295j */ = B(A1(_b/* s295g */,_/* EXTERNAL */)),
  _f/* s295p */ = B(A1(_c/* s295h */,_/* EXTERNAL */)),
  _g/* s295w */ = hs_eqWord64/* EXTERNAL */(_e/* s295j */.a, _f/* s295p */.a);
  if(!_g/* s295w */){
    return __Z/* EXTERNAL */;
  }else{
    var _h/* s295B */ = hs_eqWord64/* EXTERNAL */(_e/* s295j */.b, _f/* s295p */.b);
    return (!_h/* s295B */) ? __Z/* EXTERNAL */ : new T1(1,_d/* s295i */);
  }
},
_i/* $fExceptionPatternMatchFail_$cfromException */ = function(_j/* s4EcY */){
  var _k/* s4EcZ */ = E(_j/* s4EcY */);
  return new F(function(){return _a/* Data.Typeable.cast */(B(_8/* GHC.Exception.$p1Exception */(_k/* s4EcZ */.a)), _6/* Control.Exception.Base.$fExceptionPatternMatchFail1 */, _k/* s4EcZ */.b);});
},
_l/* $fExceptionPatternMatchFail_$cshow */ = function(_m/* s4EcV */){
  return E(E(_m/* s4EcV */).a);
},
_n/* $fExceptionPatternMatchFail_$ctoException */ = function(_o/* B1 */){
  return new T2(0,_p/* Control.Exception.Base.$fExceptionPatternMatchFail */,_o/* B1 */);
},
_q/* ++ */ = function(_r/* s3hH */, _s/* s3hI */){
  var _t/* s3hJ */ = E(_r/* s3hH */);
  return (_t/* s3hJ */._==0) ? E(_s/* s3hI */) : new T2(1,_t/* s3hJ */.a,new T(function(){
    return B(_q/* GHC.Base.++ */(_t/* s3hJ */.b, _s/* s3hI */));
  }));
},
_u/* $fShowPatternMatchFail1 */ = function(_v/* s4EcP */, _w/* s4EcQ */){
  return new F(function(){return _q/* GHC.Base.++ */(E(_v/* s4EcP */).a, _w/* s4EcQ */);});
},
_x/* showList__1 */ = 44,
_y/* showList__2 */ = 93,
_z/* showList__3 */ = 91,
_A/* showList__ */ = function(_B/* sf7e */, _C/* sf7f */, _D/* sf7g */){
  var _E/* sf7h */ = E(_C/* sf7f */);
  if(!_E/* sf7h */._){
    return new F(function(){return unAppCStr/* EXTERNAL */("[]", _D/* sf7g */);});
  }else{
    var _F/* sf7t */ = new T(function(){
      var _G/* sf7s */ = new T(function(){
        var _H/* sf7l */ = function(_I/* sf7m */){
          var _J/* sf7n */ = E(_I/* sf7m */);
          if(!_J/* sf7n */._){
            return E(new T2(1,_y/* GHC.Show.showList__2 */,_D/* sf7g */));
          }else{
            var _K/* sf7r */ = new T(function(){
              return B(A2(_B/* sf7e */,_J/* sf7n */.a, new T(function(){
                return B(_H/* sf7l */(_J/* sf7n */.b));
              })));
            });
            return new T2(1,_x/* GHC.Show.showList__1 */,_K/* sf7r */);
          }
        };
        return B(_H/* sf7l */(_E/* sf7h */.b));
      });
      return B(A2(_B/* sf7e */,_E/* sf7h */.a, _G/* sf7s */));
    });
    return new T2(1,_z/* GHC.Show.showList__3 */,_F/* sf7t */);
  }
},
_L/* $fShowPatternMatchFail_$cshowList */ = function(_M/* s4EcT */, _N/* s4EcU */){
  return new F(function(){return _A/* GHC.Show.showList__ */(_u/* Control.Exception.Base.$fShowPatternMatchFail1 */, _M/* s4EcT */, _N/* s4EcU */);});
},
_O/* $fShowPatternMatchFail_$cshowsPrec */ = function(_P/* s4EcK */, _Q/* s4EcL */, _R/* s4EcM */){
  return new F(function(){return _q/* GHC.Base.++ */(E(_Q/* s4EcL */).a, _R/* s4EcM */);});
},
_S/* $fShowPatternMatchFail */ = new T3(0,_O/* Control.Exception.Base.$fShowPatternMatchFail_$cshowsPrec */,_l/* Control.Exception.Base.$fExceptionPatternMatchFail_$cshow */,_L/* Control.Exception.Base.$fShowPatternMatchFail_$cshowList */),
_p/* $fExceptionPatternMatchFail */ = new T(function(){
  return new T5(0,_6/* Control.Exception.Base.$fExceptionPatternMatchFail1 */,_S/* Control.Exception.Base.$fShowPatternMatchFail */,_n/* Control.Exception.Base.$fExceptionPatternMatchFail_$ctoException */,_i/* Control.Exception.Base.$fExceptionPatternMatchFail_$cfromException */,_l/* Control.Exception.Base.$fExceptionPatternMatchFail_$cshow */);
}),
_T/* lvl5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Non-exhaustive patterns in"));
}),
_U/* toException */ = function(_V/* s2Va8 */){
  return E(E(_V/* s2Va8 */).c);
},
_W/* lvl */ = function(_X/* s2Vcp */, _Y/* s2Vcq */){
  return new F(function(){return die/* EXTERNAL */(new T(function(){
    return B(A2(_U/* GHC.Exception.toException */,_Y/* s2Vcq */, _X/* s2Vcp */));
  }));});
},
_Z/* throw1 */ = function(_10/* B2 */, _11/* B1 */){
  return new F(function(){return _W/* GHC.Exception.lvl */(_10/* B2 */, _11/* B1 */);});
},
_12/* $wspan */ = function(_13/* sbH9 */, _14/* sbHa */){
  var _15/* sbHb */ = E(_14/* sbHa */);
  if(!_15/* sbHb */._){
    return new T2(0,_4/* GHC.Types.[] */,_4/* GHC.Types.[] */);
  }else{
    var _16/* sbHc */ = _15/* sbHb */.a;
    if(!B(A1(_13/* sbH9 */,_16/* sbHc */))){
      return new T2(0,_4/* GHC.Types.[] */,_15/* sbHb */);
    }else{
      var _17/* sbHf */ = new T(function(){
        var _18/* sbHg */ = B(_12/* GHC.List.$wspan */(_13/* sbH9 */, _15/* sbHb */.b));
        return new T2(0,_18/* sbHg */.a,_18/* sbHg */.b);
      });
      return new T2(0,new T2(1,_16/* sbHc */,new T(function(){
        return E(E(_17/* sbHf */).a);
      })),new T(function(){
        return E(E(_17/* sbHf */).b);
      }));
    }
  }
},
_19/* untangle1 */ = 32,
_1a/* untangle2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("\n"));
}),
_1b/* untangle3 */ = function(_1c/* s3N7b */){
  return (E(_1c/* s3N7b */)==124) ? false : true;
},
_1d/* untangle */ = function(_1e/* s3N7f */, _1f/* s3N7g */){
  var _1g/* s3N7i */ = B(_12/* GHC.List.$wspan */(_1b/* GHC.IO.Exception.untangle3 */, B(unCStr/* EXTERNAL */(_1e/* s3N7f */)))),
  _1h/* s3N7j */ = _1g/* s3N7i */.a,
  _1i/* s3N7l */ = function(_1j/* s3N7m */, _1k/* s3N7n */){
    var _1l/* s3N7q */ = new T(function(){
      var _1m/* s3N7p */ = new T(function(){
        return B(_q/* GHC.Base.++ */(_1f/* s3N7g */, new T(function(){
          return B(_q/* GHC.Base.++ */(_1k/* s3N7n */, _1a/* GHC.IO.Exception.untangle2 */));
        },1)));
      });
      return B(unAppCStr/* EXTERNAL */(": ", _1m/* s3N7p */));
    },1);
    return new F(function(){return _q/* GHC.Base.++ */(_1j/* s3N7m */, _1l/* s3N7q */);});
  },
  _1n/* s3N7r */ = E(_1g/* s3N7i */.b);
  if(!_1n/* s3N7r */._){
    return new F(function(){return _1i/* s3N7l */(_1h/* s3N7j */, _4/* GHC.Types.[] */);});
  }else{
    if(E(_1n/* s3N7r */.a)==124){
      return new F(function(){return _1i/* s3N7l */(_1h/* s3N7j */, new T2(1,_19/* GHC.IO.Exception.untangle1 */,_1n/* s3N7r */.b));});
    }else{
      return new F(function(){return _1i/* s3N7l */(_1h/* s3N7j */, _4/* GHC.Types.[] */);});
    }
  }
},
_1o/* patError */ = function(_1p/* s4EiE */){
  return new F(function(){return _Z/* GHC.Exception.throw1 */(new T1(0,new T(function(){
    return B(_1d/* GHC.IO.Exception.untangle */(_1p/* s4EiE */, _T/* Control.Exception.Base.lvl5 */));
  })), _p/* Control.Exception.Base.$fExceptionPatternMatchFail */);});
},
_1q/* $fFromAnyGameState10 */ = new T(function(){
  return B(_1o/* Control.Exception.Base.patError */("LudoJS.hs:(81,18)-(85,27)|case"));
}),
_1r/* $fFromAnyGameState11 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Yellow"));
}),
_1s/* $fFromAnyGameState12 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Red"));
}),
_1t/* $fFromAnyGameState13 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Green"));
}),
_1u/* $fFromAnyGameState14 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Blue"));
}),
_1v/* eqString */ = function(_1w/* s3mO */, _1x/* s3mP */){
  while(1){
    var _1y/* s3mQ */ = E(_1w/* s3mO */);
    if(!_1y/* s3mQ */._){
      return (E(_1x/* s3mP */)._==0) ? true : false;
    }else{
      var _1z/* s3mW */ = E(_1x/* s3mP */);
      if(!_1z/* s3mW */._){
        return false;
      }else{
        if(E(_1y/* s3mQ */.a)!=E(_1z/* s3mW */.a)){
          return false;
        }else{
          _1w/* s3mO */ = _1y/* s3mQ */.b;
          _1x/* s3mP */ = _1z/* s3mW */.b;
          continue;
        }
      }
    }
  }
},
_1A/* $fFromAnyGameState9 */ = function(_1B/* shM4 */, _/* EXTERNAL */){
  return new T(function(){
    var _1C/* shM9 */ = String/* EXTERNAL */(E(_1B/* shM4 */)),
    _1D/* shMe */ = fromJSStr/* EXTERNAL */(_1C/* shM9 */);
    if(!B(_1v/* GHC.Base.eqString */(_1D/* shMe */, _1u/* LudoJS.$fFromAnyGameState14 */))){
      if(!B(_1v/* GHC.Base.eqString */(_1D/* shMe */, _1t/* LudoJS.$fFromAnyGameState13 */))){
        if(!B(_1v/* GHC.Base.eqString */(_1D/* shMe */, _1s/* LudoJS.$fFromAnyGameState12 */))){
          if(!B(_1v/* GHC.Base.eqString */(_1D/* shMe */, _1r/* LudoJS.$fFromAnyGameState11 */))){
            return E(_1q/* LudoJS.$fFromAnyGameState10 */);
          }else{
            return 3;
          }
        }else{
          return 2;
        }
      }else{
        return 1;
      }
    }else{
      return 0;
    }
  });
},
_1E/* $fFromAnyGameState15 */ = "turn",
_1F/* $fFromAnyGameState16 */ = "stage",
_1G/* $fFromAnyGameState4 */ = function(_1H/* si5T */, _/* EXTERNAL */){
  var _1I/* si5V */ = E(_1H/* si5T */);
  if(!_1I/* si5V */._){
    return _4/* GHC.Types.[] */;
  }else{
    var _1J/* si5Y */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_1I/* si5V */.a, _/* EXTERNAL */)),
    _1K/* si61 */ = B(_1G/* LudoJS.$fFromAnyGameState4 */(_1I/* si5V */.b, _/* EXTERNAL */));
    return new T2(1,_1J/* si5Y */,_1K/* si61 */);
  }
},
_1L/* $fFromAnyGameState5 */ = "finished",
_1M/* $fToAnyOption1 */ = "field",
_1N/* $fToAnyOption5 */ = "piece",
_1O/* Blue */ = 0,
_1P/* Green */ = 1,
_1Q/* Red */ = 2,
_1R/* Tip */ = new T0(1),
_1S/* Yellow */ = 3,
_1T/* lvl12 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Failure in Data.Map.balanceL"));
}),
_1U/* $wpoly_fail2 */ = function(_1V/* s2ee1 */){
  return new F(function(){return err/* EXTERNAL */(_1T/* Data.Map.Base.lvl12 */);});
},
_1W/* lvl13 */ = new T(function(){
  return B(_1U/* Data.Map.Base.$wpoly_fail2 */(_/* EXTERNAL */));
}),
_1X/* balanceL */ = function(_1Y/* s2ee2 */, _1Z/* s2ee3 */, _20/* s2ee4 */, _21/* s2ee5 */){
  var _22/* s2ee6 */ = E(_21/* s2ee5 */);
  if(!_22/* s2ee6 */._){
    var _23/* s2ee7 */ = _22/* s2ee6 */.a,
    _24/* s2eec */ = E(_20/* s2ee4 */);
    if(!_24/* s2eec */._){
      var _25/* s2eed */ = _24/* s2eec */.a,
      _26/* s2eee */ = _24/* s2eec */.b,
      _27/* s2eef */ = _24/* s2eec */.c;
      if(_25/* s2eed */<=(imul/* EXTERNAL */(3, _23/* s2ee7 */)|0)){
        return new T5(0,(1+_25/* s2eed */|0)+_23/* s2ee7 */|0,E(_1Y/* s2ee2 */),_1Z/* s2ee3 */,E(_24/* s2eec */),E(_22/* s2ee6 */));
      }else{
        var _28/* s2eeo */ = E(_24/* s2eec */.d);
        if(!_28/* s2eeo */._){
          var _29/* s2eep */ = _28/* s2eeo */.a,
          _2a/* s2eeu */ = E(_24/* s2eec */.e);
          if(!_2a/* s2eeu */._){
            var _2b/* s2eev */ = _2a/* s2eeu */.a,
            _2c/* s2eew */ = _2a/* s2eeu */.b,
            _2d/* s2eex */ = _2a/* s2eeu */.c,
            _2e/* s2eey */ = _2a/* s2eeu */.d;
            if(_2b/* s2eev */>=(imul/* EXTERNAL */(2, _29/* s2eep */)|0)){
              var _2f/* s2eeD */ = function(_2g/* s2eeE */){
                var _2h/* s2eeF */ = E(_2a/* s2eeu */.e);
                return (_2h/* s2eeF */._==0) ? new T5(0,(1+_25/* s2eed */|0)+_23/* s2ee7 */|0,E(_2c/* s2eew */),_2d/* s2eex */,E(new T5(0,(1+_29/* s2eep */|0)+_2g/* s2eeE */|0,E(_26/* s2eee */),_27/* s2eef */,E(_28/* s2eeo */),E(_2e/* s2eey */))),E(new T5(0,(1+_23/* s2ee7 */|0)+_2h/* s2eeF */.a|0,E(_1Y/* s2ee2 */),_1Z/* s2ee3 */,E(_2h/* s2eeF */),E(_22/* s2ee6 */)))) : new T5(0,(1+_25/* s2eed */|0)+_23/* s2ee7 */|0,E(_2c/* s2eew */),_2d/* s2eex */,E(new T5(0,(1+_29/* s2eep */|0)+_2g/* s2eeE */|0,E(_26/* s2eee */),_27/* s2eef */,E(_28/* s2eeo */),E(_2e/* s2eey */))),E(new T5(0,1+_23/* s2ee7 */|0,E(_1Y/* s2ee2 */),_1Z/* s2ee3 */,E(_1R/* Data.Map.Base.Tip */),E(_22/* s2ee6 */))));
              },
              _2i/* s2ef2 */ = E(_2e/* s2eey */);
              if(!_2i/* s2ef2 */._){
                return new F(function(){return _2f/* s2eeD */(_2i/* s2ef2 */.a);});
              }else{
                return new F(function(){return _2f/* s2eeD */(0);});
              }
            }else{
              return new T5(0,(1+_25/* s2eed */|0)+_23/* s2ee7 */|0,E(_26/* s2eee */),_27/* s2eef */,E(_28/* s2eeo */),E(new T5(0,(1+_23/* s2ee7 */|0)+_2b/* s2eev */|0,E(_1Y/* s2ee2 */),_1Z/* s2ee3 */,E(_2a/* s2eeu */),E(_22/* s2ee6 */))));
            }
          }else{
            return E(_1W/* Data.Map.Base.lvl13 */);
          }
        }else{
          return E(_1W/* Data.Map.Base.lvl13 */);
        }
      }
    }else{
      return new T5(0,1+_23/* s2ee7 */|0,E(_1Y/* s2ee2 */),_1Z/* s2ee3 */,E(_1R/* Data.Map.Base.Tip */),E(_22/* s2ee6 */));
    }
  }else{
    var _2j/* s2efg */ = E(_20/* s2ee4 */);
    if(!_2j/* s2efg */._){
      var _2k/* s2efh */ = _2j/* s2efg */.a,
      _2l/* s2efi */ = _2j/* s2efg */.b,
      _2m/* s2efj */ = _2j/* s2efg */.c,
      _2n/* s2efl */ = _2j/* s2efg */.e,
      _2o/* s2efm */ = E(_2j/* s2efg */.d);
      if(!_2o/* s2efm */._){
        var _2p/* s2efn */ = _2o/* s2efm */.a,
        _2q/* s2efs */ = E(_2n/* s2efl */);
        if(!_2q/* s2efs */._){
          var _2r/* s2eft */ = _2q/* s2efs */.a,
          _2s/* s2efu */ = _2q/* s2efs */.b,
          _2t/* s2efv */ = _2q/* s2efs */.c,
          _2u/* s2efw */ = _2q/* s2efs */.d;
          if(_2r/* s2eft */>=(imul/* EXTERNAL */(2, _2p/* s2efn */)|0)){
            var _2v/* s2efB */ = function(_2w/* s2efC */){
              var _2x/* s2efD */ = E(_2q/* s2efs */.e);
              return (_2x/* s2efD */._==0) ? new T5(0,1+_2k/* s2efh */|0,E(_2s/* s2efu */),_2t/* s2efv */,E(new T5(0,(1+_2p/* s2efn */|0)+_2w/* s2efC */|0,E(_2l/* s2efi */),_2m/* s2efj */,E(_2o/* s2efm */),E(_2u/* s2efw */))),E(new T5(0,1+_2x/* s2efD */.a|0,E(_1Y/* s2ee2 */),_1Z/* s2ee3 */,E(_2x/* s2efD */),E(_1R/* Data.Map.Base.Tip */)))) : new T5(0,1+_2k/* s2efh */|0,E(_2s/* s2efu */),_2t/* s2efv */,E(new T5(0,(1+_2p/* s2efn */|0)+_2w/* s2efC */|0,E(_2l/* s2efi */),_2m/* s2efj */,E(_2o/* s2efm */),E(_2u/* s2efw */))),E(new T5(0,1,E(_1Y/* s2ee2 */),_1Z/* s2ee3 */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */))));
            },
            _2y/* s2efW */ = E(_2u/* s2efw */);
            if(!_2y/* s2efW */._){
              return new F(function(){return _2v/* s2efB */(_2y/* s2efW */.a);});
            }else{
              return new F(function(){return _2v/* s2efB */(0);});
            }
          }else{
            return new T5(0,1+_2k/* s2efh */|0,E(_2l/* s2efi */),_2m/* s2efj */,E(_2o/* s2efm */),E(new T5(0,1+_2r/* s2eft */|0,E(_1Y/* s2ee2 */),_1Z/* s2ee3 */,E(_2q/* s2efs */),E(_1R/* Data.Map.Base.Tip */))));
          }
        }else{
          return new T5(0,3,E(_2l/* s2efi */),_2m/* s2efj */,E(_2o/* s2efm */),E(new T5(0,1,E(_1Y/* s2ee2 */),_1Z/* s2ee3 */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */))));
        }
      }else{
        var _2z/* s2eg8 */ = E(_2n/* s2efl */);
        return (_2z/* s2eg8 */._==0) ? new T5(0,3,E(_2z/* s2eg8 */.b),_2z/* s2eg8 */.c,E(new T5(0,1,E(_2l/* s2efi */),_2m/* s2efj */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */))),E(new T5(0,1,E(_1Y/* s2ee2 */),_1Z/* s2ee3 */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)))) : new T5(0,2,E(_1Y/* s2ee2 */),_1Z/* s2ee3 */,E(_2j/* s2efg */),E(_1R/* Data.Map.Base.Tip */));
      }
    }else{
      return new T5(0,1,E(_1Y/* s2ee2 */),_1Z/* s2ee3 */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
    }
  }
},
_2A/* lvl15 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Failure in Data.Map.balanceR"));
}),
_2B/* $wpoly_fail3 */ = function(_2C/* s2eiN */){
  return new F(function(){return err/* EXTERNAL */(_2A/* Data.Map.Base.lvl15 */);});
},
_2D/* lvl16 */ = new T(function(){
  return B(_2B/* Data.Map.Base.$wpoly_fail3 */(_/* EXTERNAL */));
}),
_2E/* balanceR */ = function(_2F/* s2eiO */, _2G/* s2eiP */, _2H/* s2eiQ */, _2I/* s2eiR */){
  var _2J/* s2eiS */ = E(_2H/* s2eiQ */);
  if(!_2J/* s2eiS */._){
    var _2K/* s2eiT */ = _2J/* s2eiS */.a,
    _2L/* s2eiY */ = E(_2I/* s2eiR */);
    if(!_2L/* s2eiY */._){
      var _2M/* s2eiZ */ = _2L/* s2eiY */.a,
      _2N/* s2ej0 */ = _2L/* s2eiY */.b,
      _2O/* s2ej1 */ = _2L/* s2eiY */.c;
      if(_2M/* s2eiZ */<=(imul/* EXTERNAL */(3, _2K/* s2eiT */)|0)){
        return new T5(0,(1+_2K/* s2eiT */|0)+_2M/* s2eiZ */|0,E(_2F/* s2eiO */),_2G/* s2eiP */,E(_2J/* s2eiS */),E(_2L/* s2eiY */));
      }else{
        var _2P/* s2eja */ = E(_2L/* s2eiY */.d);
        if(!_2P/* s2eja */._){
          var _2Q/* s2ejb */ = _2P/* s2eja */.a,
          _2R/* s2ejc */ = _2P/* s2eja */.b,
          _2S/* s2ejd */ = _2P/* s2eja */.c,
          _2T/* s2eje */ = _2P/* s2eja */.d,
          _2U/* s2ejg */ = E(_2L/* s2eiY */.e);
          if(!_2U/* s2ejg */._){
            var _2V/* s2ejh */ = _2U/* s2ejg */.a;
            if(_2Q/* s2ejb */>=(imul/* EXTERNAL */(2, _2V/* s2ejh */)|0)){
              var _2W/* s2ejp */ = function(_2X/* s2ejq */){
                var _2Y/* s2ejr */ = E(_2F/* s2eiO */),
                _2Z/* s2ejs */ = E(_2P/* s2eja */.e);
                return (_2Z/* s2ejs */._==0) ? new T5(0,(1+_2K/* s2eiT */|0)+_2M/* s2eiZ */|0,E(_2R/* s2ejc */),_2S/* s2ejd */,E(new T5(0,(1+_2K/* s2eiT */|0)+_2X/* s2ejq */|0,E(_2Y/* s2ejr */),_2G/* s2eiP */,E(_2J/* s2eiS */),E(_2T/* s2eje */))),E(new T5(0,(1+_2V/* s2ejh */|0)+_2Z/* s2ejs */.a|0,E(_2N/* s2ej0 */),_2O/* s2ej1 */,E(_2Z/* s2ejs */),E(_2U/* s2ejg */)))) : new T5(0,(1+_2K/* s2eiT */|0)+_2M/* s2eiZ */|0,E(_2R/* s2ejc */),_2S/* s2ejd */,E(new T5(0,(1+_2K/* s2eiT */|0)+_2X/* s2ejq */|0,E(_2Y/* s2ejr */),_2G/* s2eiP */,E(_2J/* s2eiS */),E(_2T/* s2eje */))),E(new T5(0,1+_2V/* s2ejh */|0,E(_2N/* s2ej0 */),_2O/* s2ej1 */,E(_1R/* Data.Map.Base.Tip */),E(_2U/* s2ejg */))));
              },
              _30/* s2ejN */ = E(_2T/* s2eje */);
              if(!_30/* s2ejN */._){
                return new F(function(){return _2W/* s2ejp */(_30/* s2ejN */.a);});
              }else{
                return new F(function(){return _2W/* s2ejp */(0);});
              }
            }else{
              return new T5(0,(1+_2K/* s2eiT */|0)+_2M/* s2eiZ */|0,E(_2N/* s2ej0 */),_2O/* s2ej1 */,E(new T5(0,(1+_2K/* s2eiT */|0)+_2Q/* s2ejb */|0,E(_2F/* s2eiO */),_2G/* s2eiP */,E(_2J/* s2eiS */),E(_2P/* s2eja */))),E(_2U/* s2ejg */));
            }
          }else{
            return E(_2D/* Data.Map.Base.lvl16 */);
          }
        }else{
          return E(_2D/* Data.Map.Base.lvl16 */);
        }
      }
    }else{
      return new T5(0,1+_2K/* s2eiT */|0,E(_2F/* s2eiO */),_2G/* s2eiP */,E(_2J/* s2eiS */),E(_1R/* Data.Map.Base.Tip */));
    }
  }else{
    var _31/* s2ek1 */ = E(_2I/* s2eiR */);
    if(!_31/* s2ek1 */._){
      var _32/* s2ek2 */ = _31/* s2ek1 */.a,
      _33/* s2ek3 */ = _31/* s2ek1 */.b,
      _34/* s2ek4 */ = _31/* s2ek1 */.c,
      _35/* s2ek6 */ = _31/* s2ek1 */.e,
      _36/* s2ek7 */ = E(_31/* s2ek1 */.d);
      if(!_36/* s2ek7 */._){
        var _37/* s2ek8 */ = _36/* s2ek7 */.a,
        _38/* s2ek9 */ = _36/* s2ek7 */.b,
        _39/* s2eka */ = _36/* s2ek7 */.c,
        _3a/* s2ekb */ = _36/* s2ek7 */.d,
        _3b/* s2ekd */ = E(_35/* s2ek6 */);
        if(!_3b/* s2ekd */._){
          var _3c/* s2eke */ = _3b/* s2ekd */.a;
          if(_37/* s2ek8 */>=(imul/* EXTERNAL */(2, _3c/* s2eke */)|0)){
            var _3d/* s2ekm */ = function(_3e/* s2ekn */){
              var _3f/* s2eko */ = E(_2F/* s2eiO */),
              _3g/* s2ekp */ = E(_36/* s2ek7 */.e);
              return (_3g/* s2ekp */._==0) ? new T5(0,1+_32/* s2ek2 */|0,E(_38/* s2ek9 */),_39/* s2eka */,E(new T5(0,1+_3e/* s2ekn */|0,E(_3f/* s2eko */),_2G/* s2eiP */,E(_1R/* Data.Map.Base.Tip */),E(_3a/* s2ekb */))),E(new T5(0,(1+_3c/* s2eke */|0)+_3g/* s2ekp */.a|0,E(_33/* s2ek3 */),_34/* s2ek4 */,E(_3g/* s2ekp */),E(_3b/* s2ekd */)))) : new T5(0,1+_32/* s2ek2 */|0,E(_38/* s2ek9 */),_39/* s2eka */,E(new T5(0,1+_3e/* s2ekn */|0,E(_3f/* s2eko */),_2G/* s2eiP */,E(_1R/* Data.Map.Base.Tip */),E(_3a/* s2ekb */))),E(new T5(0,1+_3c/* s2eke */|0,E(_33/* s2ek3 */),_34/* s2ek4 */,E(_1R/* Data.Map.Base.Tip */),E(_3b/* s2ekd */))));
            },
            _3h/* s2ekG */ = E(_3a/* s2ekb */);
            if(!_3h/* s2ekG */._){
              return new F(function(){return _3d/* s2ekm */(_3h/* s2ekG */.a);});
            }else{
              return new F(function(){return _3d/* s2ekm */(0);});
            }
          }else{
            return new T5(0,1+_32/* s2ek2 */|0,E(_33/* s2ek3 */),_34/* s2ek4 */,E(new T5(0,1+_37/* s2ek8 */|0,E(_2F/* s2eiO */),_2G/* s2eiP */,E(_1R/* Data.Map.Base.Tip */),E(_36/* s2ek7 */))),E(_3b/* s2ekd */));
          }
        }else{
          return new T5(0,3,E(_38/* s2ek9 */),_39/* s2eka */,E(new T5(0,1,E(_2F/* s2eiO */),_2G/* s2eiP */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */))),E(new T5(0,1,E(_33/* s2ek3 */),_34/* s2ek4 */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */))));
        }
      }else{
        var _3i/* s2ekT */ = E(_35/* s2ek6 */);
        return (_3i/* s2ekT */._==0) ? new T5(0,3,E(_33/* s2ek3 */),_34/* s2ek4 */,E(new T5(0,1,E(_2F/* s2eiO */),_2G/* s2eiP */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */))),E(_3i/* s2ekT */)) : new T5(0,2,E(_2F/* s2eiO */),_2G/* s2eiP */,E(_1R/* Data.Map.Base.Tip */),E(_31/* s2ek1 */));
      }
    }else{
      return new T5(0,1,E(_2F/* s2eiO */),_2G/* s2eiP */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
    }
  }
},
_3j/* $sinsert_$sgo10 */ = function(_3k/* shFn */, _3l/* shFo */, _3m/* shFp */){
  var _3n/* shFq */ = E(_3k/* shFn */),
  _3o/* shFr */ = E(_3m/* shFp */);
  if(!_3o/* shFr */._){
    var _3p/* shFs */ = _3o/* shFr */.a,
    _3q/* shFt */ = _3o/* shFr */.b,
    _3r/* shFu */ = _3o/* shFr */.c,
    _3s/* shFv */ = _3o/* shFr */.d,
    _3t/* shFw */ = _3o/* shFr */.e;
    switch(E(_3n/* shFq */)){
      case 0:
        switch(E(_3q/* shFt */)){
          case 0:
            return new T5(0,_3p/* shFs */,E(_1O/* LudoJS.Blue */),_3l/* shFo */,E(_3s/* shFv */),E(_3t/* shFw */));
          case 1:
            return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1P/* LudoJS.Green */, _3r/* shFu */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1O/* LudoJS.Blue */, _3l/* shFo */, _3s/* shFv */)), _3t/* shFw */);});
            break;
          case 2:
            return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1Q/* LudoJS.Red */, _3r/* shFu */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1O/* LudoJS.Blue */, _3l/* shFo */, _3s/* shFv */)), _3t/* shFw */);});
            break;
          default:
            return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1S/* LudoJS.Yellow */, _3r/* shFu */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1O/* LudoJS.Blue */, _3l/* shFo */, _3s/* shFv */)), _3t/* shFw */);});
        }
        break;
      case 1:
        switch(E(_3q/* shFt */)){
          case 0:
            return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1O/* LudoJS.Blue */, _3r/* shFu */, _3s/* shFv */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1P/* LudoJS.Green */, _3l/* shFo */, _3t/* shFw */)));});
            break;
          case 1:
            return new T5(0,_3p/* shFs */,E(_1P/* LudoJS.Green */),_3l/* shFo */,E(_3s/* shFv */),E(_3t/* shFw */));
          case 2:
            return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1Q/* LudoJS.Red */, _3r/* shFu */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1P/* LudoJS.Green */, _3l/* shFo */, _3s/* shFv */)), _3t/* shFw */);});
            break;
          default:
            return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1S/* LudoJS.Yellow */, _3r/* shFu */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1P/* LudoJS.Green */, _3l/* shFo */, _3s/* shFv */)), _3t/* shFw */);});
        }
        break;
      case 2:
        switch(E(_3q/* shFt */)){
          case 0:
            return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1O/* LudoJS.Blue */, _3r/* shFu */, _3s/* shFv */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1Q/* LudoJS.Red */, _3l/* shFo */, _3t/* shFw */)));});
            break;
          case 1:
            return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1P/* LudoJS.Green */, _3r/* shFu */, _3s/* shFv */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1Q/* LudoJS.Red */, _3l/* shFo */, _3t/* shFw */)));});
            break;
          case 2:
            return new T5(0,_3p/* shFs */,E(_1Q/* LudoJS.Red */),_3l/* shFo */,E(_3s/* shFv */),E(_3t/* shFw */));
          default:
            return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1S/* LudoJS.Yellow */, _3r/* shFu */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1Q/* LudoJS.Red */, _3l/* shFo */, _3s/* shFv */)), _3t/* shFw */);});
        }
        break;
      default:
        switch(E(_3q/* shFt */)){
          case 0:
            return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1O/* LudoJS.Blue */, _3r/* shFu */, _3s/* shFv */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1S/* LudoJS.Yellow */, _3l/* shFo */, _3t/* shFw */)));});
            break;
          case 1:
            return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1P/* LudoJS.Green */, _3r/* shFu */, _3s/* shFv */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1S/* LudoJS.Yellow */, _3l/* shFo */, _3t/* shFw */)));});
            break;
          case 2:
            return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1Q/* LudoJS.Red */, _3r/* shFu */, _3s/* shFv */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1S/* LudoJS.Yellow */, _3l/* shFo */, _3t/* shFw */)));});
            break;
          default:
            return new T5(0,_3p/* shFs */,E(_1S/* LudoJS.Yellow */),_3l/* shFo */,E(_3s/* shFv */),E(_3t/* shFw */));
        }
    }
  }else{
    return new T5(0,1,E(_3n/* shFq */),_3l/* shFo */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
  }
},
_3u/* poly_go10 */ = function(_3v/* shG0 */, _3w/* shG1 */){
  while(1){
    var _3x/* shG2 */ = E(_3w/* shG1 */);
    if(!_3x/* shG2 */._){
      return E(_3v/* shG0 */);
    }else{
      var _3y/* shG5 */ = E(_3x/* shG2 */.a),
      _3z/*  shG0 */ = B(_3j/* LudoJS.$sinsert_$sgo10 */(_3y/* shG5 */.a, _3y/* shG5 */.b, _3v/* shG0 */));
      _3v/* shG0 */ = _3z/*  shG0 */;
      _3w/* shG1 */ = _3x/* shG2 */.b;
      continue;
    }
  }
},
_3A/* $sfromList_$spoly_go10 */ = function(_3B/* shFV */, _3C/* shFW */, _3D/* shFX */, _3E/* shFY */){
  return new F(function(){return _3u/* LudoJS.poly_go10 */(B(_3j/* LudoJS.$sinsert_$sgo10 */(_3C/* shFW */, _3D/* shFX */, _3B/* shFV */)), _3E/* shFY */);});
},
_3F/* singleton */ = function(_3G/* s2e3X */, _3H/* s2e3Y */){
  return new T5(0,1,E(_3G/* s2e3X */),_3H/* s2e3Y */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
},
_3I/* insertMax */ = function(_3J/* s2eoG */, _3K/* s2eoH */, _3L/* s2eoI */){
  var _3M/* s2eoJ */ = E(_3L/* s2eoI */);
  if(!_3M/* s2eoJ */._){
    return new F(function(){return _2E/* Data.Map.Base.balanceR */(_3M/* s2eoJ */.b, _3M/* s2eoJ */.c, _3M/* s2eoJ */.d, B(_3I/* Data.Map.Base.insertMax */(_3J/* s2eoG */, _3K/* s2eoH */, _3M/* s2eoJ */.e)));});
  }else{
    return new F(function(){return _3F/* Data.Map.Base.singleton */(_3J/* s2eoG */, _3K/* s2eoH */);});
  }
},
_3N/* insertMin */ = function(_3O/* s2ehs */, _3P/* s2eht */, _3Q/* s2ehu */){
  var _3R/* s2ehv */ = E(_3Q/* s2ehu */);
  if(!_3R/* s2ehv */._){
    return new F(function(){return _1X/* Data.Map.Base.balanceL */(_3R/* s2ehv */.b, _3R/* s2ehv */.c, B(_3N/* Data.Map.Base.insertMin */(_3O/* s2ehs */, _3P/* s2eht */, _3R/* s2ehv */.d)), _3R/* s2ehv */.e);});
  }else{
    return new F(function(){return _3F/* Data.Map.Base.singleton */(_3O/* s2ehs */, _3P/* s2eht */);});
  }
},
_3S/* link_$sinsertMin */ = function(_3T/* s2ehk */, _3U/* s2ehl */, _3V/* s2ehm */, _3W/* s2ehn */, _3X/* s2eho */, _3Y/* s2ehp */, _3Z/* s2ehq */){
  return new F(function(){return _1X/* Data.Map.Base.balanceL */(_3W/* s2ehn */, _3X/* s2eho */, B(_3N/* Data.Map.Base.insertMin */(_3T/* s2ehk */, _3U/* s2ehl */, _3Y/* s2ehp */)), _3Z/* s2ehq */);});
},
_40/* link_$slink1 */ = function(_41/* s2evt */, _42/* s2evu */, _43/* s2evv */, _44/* s2evw */, _45/* s2evx */, _46/* s2evy */, _47/* s2evz */, _48/* s2evA */){
  var _49/* s2evB */ = E(_43/* s2evv */);
  if(!_49/* s2evB */._){
    var _4a/* s2evC */ = _49/* s2evB */.a,
    _4b/* s2evD */ = _49/* s2evB */.b,
    _4c/* s2evE */ = _49/* s2evB */.c,
    _4d/* s2evF */ = _49/* s2evB */.d,
    _4e/* s2evG */ = _49/* s2evB */.e;
    if((imul/* EXTERNAL */(3, _4a/* s2evC */)|0)>=_44/* s2evw */){
      if((imul/* EXTERNAL */(3, _44/* s2evw */)|0)>=_4a/* s2evC */){
        return new T5(0,(_4a/* s2evC */+_44/* s2evw */|0)+1|0,E(_41/* s2evt */),_42/* s2evu */,E(_49/* s2evB */),E(new T5(0,_44/* s2evw */,E(_45/* s2evx */),_46/* s2evy */,E(_47/* s2evz */),E(_48/* s2evA */))));
      }else{
        return new F(function(){return _2E/* Data.Map.Base.balanceR */(_4b/* s2evD */, _4c/* s2evE */, _4d/* s2evF */, B(_40/* Data.Map.Base.link_$slink1 */(_41/* s2evt */, _42/* s2evu */, _4e/* s2evG */, _44/* s2evw */, _45/* s2evx */, _46/* s2evy */, _47/* s2evz */, _48/* s2evA */)));});
      }
    }else{
      return new F(function(){return _1X/* Data.Map.Base.balanceL */(_45/* s2evx */, _46/* s2evy */, B(_4f/* Data.Map.Base.link_$slink */(_41/* s2evt */, _42/* s2evu */, _4a/* s2evC */, _4b/* s2evD */, _4c/* s2evE */, _4d/* s2evF */, _4e/* s2evG */, _47/* s2evz */)), _48/* s2evA */);});
    }
  }else{
    return new F(function(){return _3S/* Data.Map.Base.link_$sinsertMin */(_41/* s2evt */, _42/* s2evu */, _44/* s2evw */, _45/* s2evx */, _46/* s2evy */, _47/* s2evz */, _48/* s2evA */);});
  }
},
_4f/* link_$slink */ = function(_4g/* s2ev2 */, _4h/* s2ev3 */, _4i/* s2ev4 */, _4j/* s2ev5 */, _4k/* s2ev6 */, _4l/* s2ev7 */, _4m/* s2ev8 */, _4n/* s2ev9 */){
  var _4o/* s2eva */ = E(_4n/* s2ev9 */);
  if(!_4o/* s2eva */._){
    var _4p/* s2evb */ = _4o/* s2eva */.a,
    _4q/* s2evc */ = _4o/* s2eva */.b,
    _4r/* s2evd */ = _4o/* s2eva */.c,
    _4s/* s2eve */ = _4o/* s2eva */.d,
    _4t/* s2evf */ = _4o/* s2eva */.e;
    if((imul/* EXTERNAL */(3, _4i/* s2ev4 */)|0)>=_4p/* s2evb */){
      if((imul/* EXTERNAL */(3, _4p/* s2evb */)|0)>=_4i/* s2ev4 */){
        return new T5(0,(_4i/* s2ev4 */+_4p/* s2evb */|0)+1|0,E(_4g/* s2ev2 */),_4h/* s2ev3 */,E(new T5(0,_4i/* s2ev4 */,E(_4j/* s2ev5 */),_4k/* s2ev6 */,E(_4l/* s2ev7 */),E(_4m/* s2ev8 */))),E(_4o/* s2eva */));
      }else{
        return new F(function(){return _2E/* Data.Map.Base.balanceR */(_4j/* s2ev5 */, _4k/* s2ev6 */, _4l/* s2ev7 */, B(_40/* Data.Map.Base.link_$slink1 */(_4g/* s2ev2 */, _4h/* s2ev3 */, _4m/* s2ev8 */, _4p/* s2evb */, _4q/* s2evc */, _4r/* s2evd */, _4s/* s2eve */, _4t/* s2evf */)));});
      }
    }else{
      return new F(function(){return _1X/* Data.Map.Base.balanceL */(_4q/* s2evc */, _4r/* s2evd */, B(_4f/* Data.Map.Base.link_$slink */(_4g/* s2ev2 */, _4h/* s2ev3 */, _4i/* s2ev4 */, _4j/* s2ev5 */, _4k/* s2ev6 */, _4l/* s2ev7 */, _4m/* s2ev8 */, _4s/* s2eve */)), _4t/* s2evf */);});
    }
  }else{
    return new F(function(){return _3I/* Data.Map.Base.insertMax */(_4g/* s2ev2 */, _4h/* s2ev3 */, new T5(0,_4i/* s2ev4 */,E(_4j/* s2ev5 */),_4k/* s2ev6 */,E(_4l/* s2ev7 */),E(_4m/* s2ev8 */)));});
  }
},
_4u/* link */ = function(_4v/* s2evT */, _4w/* s2evU */, _4x/* s2evV */, _4y/* s2evW */){
  var _4z/* s2evX */ = E(_4x/* s2evV */);
  if(!_4z/* s2evX */._){
    var _4A/* s2evY */ = _4z/* s2evX */.a,
    _4B/* s2evZ */ = _4z/* s2evX */.b,
    _4C/* s2ew0 */ = _4z/* s2evX */.c,
    _4D/* s2ew1 */ = _4z/* s2evX */.d,
    _4E/* s2ew2 */ = _4z/* s2evX */.e,
    _4F/* s2ew3 */ = E(_4y/* s2evW */);
    if(!_4F/* s2ew3 */._){
      var _4G/* s2ew4 */ = _4F/* s2ew3 */.a,
      _4H/* s2ew5 */ = _4F/* s2ew3 */.b,
      _4I/* s2ew6 */ = _4F/* s2ew3 */.c,
      _4J/* s2ew7 */ = _4F/* s2ew3 */.d,
      _4K/* s2ew8 */ = _4F/* s2ew3 */.e;
      if((imul/* EXTERNAL */(3, _4A/* s2evY */)|0)>=_4G/* s2ew4 */){
        if((imul/* EXTERNAL */(3, _4G/* s2ew4 */)|0)>=_4A/* s2evY */){
          return new T5(0,(_4A/* s2evY */+_4G/* s2ew4 */|0)+1|0,E(_4v/* s2evT */),_4w/* s2evU */,E(_4z/* s2evX */),E(_4F/* s2ew3 */));
        }else{
          return new F(function(){return _2E/* Data.Map.Base.balanceR */(_4B/* s2evZ */, _4C/* s2ew0 */, _4D/* s2ew1 */, B(_40/* Data.Map.Base.link_$slink1 */(_4v/* s2evT */, _4w/* s2evU */, _4E/* s2ew2 */, _4G/* s2ew4 */, _4H/* s2ew5 */, _4I/* s2ew6 */, _4J/* s2ew7 */, _4K/* s2ew8 */)));});
        }
      }else{
        return new F(function(){return _1X/* Data.Map.Base.balanceL */(_4H/* s2ew5 */, _4I/* s2ew6 */, B(_4f/* Data.Map.Base.link_$slink */(_4v/* s2evT */, _4w/* s2evU */, _4A/* s2evY */, _4B/* s2evZ */, _4C/* s2ew0 */, _4D/* s2ew1 */, _4E/* s2ew2 */, _4J/* s2ew7 */)), _4K/* s2ew8 */);});
      }
    }else{
      return new F(function(){return _3I/* Data.Map.Base.insertMax */(_4v/* s2evT */, _4w/* s2evU */, _4z/* s2evX */);});
    }
  }else{
    return new F(function(){return _3N/* Data.Map.Base.insertMin */(_4v/* s2evT */, _4w/* s2evU */, _4y/* s2evW */);});
  }
},
_4L/* $s$wpoly_create */ = function(_4M/* shDo */, _4N/* shDp */, _4O/* shDq */, _4P/* shDr */){
  var _4Q/* shDs */ = E(_4M/* shDo */);
  if(_4Q/* shDs */==1){
    var _4R/* shEq */ = E(_4P/* shDr */);
    if(!_4R/* shEq */._){
      return new T3(0,new T(function(){
        return new T5(0,1,E(_4N/* shDp */),_4O/* shDq */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
      }),_4/* GHC.Types.[] */,_4/* GHC.Types.[] */);
    }else{
      var _4S/* shEw */ = E(_4R/* shEq */.a).a;
      switch(E(_4N/* shDp */)){
        case 0:
          switch(E(_4S/* shEw */)){
            case 0:
              return new T3(0,new T5(0,1,E(_1O/* LudoJS.Blue */),_4O/* shDq */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_4R/* shEq */);
            case 1:
              return new T3(0,new T5(0,1,E(_1O/* LudoJS.Blue */),_4O/* shDq */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4R/* shEq */,_4/* GHC.Types.[] */);
            case 2:
              return new T3(0,new T5(0,1,E(_1O/* LudoJS.Blue */),_4O/* shDq */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4R/* shEq */,_4/* GHC.Types.[] */);
            default:
              return new T3(0,new T5(0,1,E(_1O/* LudoJS.Blue */),_4O/* shDq */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4R/* shEq */,_4/* GHC.Types.[] */);
          }
          break;
        case 1:
          switch(E(_4S/* shEw */)){
            case 2:
              return new T3(0,new T5(0,1,E(_1P/* LudoJS.Green */),_4O/* shDq */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4R/* shEq */,_4/* GHC.Types.[] */);
            case 3:
              return new T3(0,new T5(0,1,E(_1P/* LudoJS.Green */),_4O/* shDq */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4R/* shEq */,_4/* GHC.Types.[] */);
            default:
              return new T3(0,new T5(0,1,E(_1P/* LudoJS.Green */),_4O/* shDq */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_4R/* shEq */);
          }
          break;
        case 2:
          return (E(_4S/* shEw */)==3) ? new T3(0,new T5(0,1,E(_1Q/* LudoJS.Red */),_4O/* shDq */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4R/* shEq */,_4/* GHC.Types.[] */) : new T3(0,new T5(0,1,E(_1Q/* LudoJS.Red */),_4O/* shDq */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_4R/* shEq */);
        default:
          var _4T/* shEL */ = E(_4S/* shEw */);
          return new T3(0,new T5(0,1,E(_1S/* LudoJS.Yellow */),_4O/* shDq */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_4R/* shEq */);
      }
    }
  }else{
    var _4U/* shDu */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shDs */>>1, _4N/* shDp */, _4O/* shDq */, _4P/* shDr */)),
    _4V/* shDv */ = _4U/* shDu */.a,
    _4W/* shDx */ = _4U/* shDu */.c,
    _4X/* shDy */ = E(_4U/* shDu */.b);
    if(!_4X/* shDy */._){
      return new T3(0,_4V/* shDv */,_4/* GHC.Types.[] */,_4W/* shDx */);
    }else{
      var _4Y/* shDB */ = E(_4X/* shDy */.a),
      _4Z/* shDC */ = _4Y/* shDB */.a,
      _50/* shDD */ = _4Y/* shDB */.b,
      _51/* shDE */ = E(_4X/* shDy */.b);
      if(!_51/* shDE */._){
        return new T3(0,new T(function(){
          return B(_3I/* Data.Map.Base.insertMax */(_4Z/* shDC */, _50/* shDD */, _4V/* shDv */));
        }),_4/* GHC.Types.[] */,_4W/* shDx */);
      }else{
        var _52/* shDH */ = _51/* shDE */.b,
        _53/* shDI */ = E(_51/* shDE */.a),
        _54/* shDJ */ = _53/* shDI */.a,
        _55/* shDK */ = _53/* shDI */.b;
        switch(E(_4Z/* shDC */)){
          case 0:
            switch(E(_54/* shDJ */)){
              case 0:
                return new T3(0,_4V/* shDv */,_4/* GHC.Types.[] */,_4X/* shDy */);
              case 1:
                var _56/* shDO */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shDs */>>1, _1P/* LudoJS.Green */, _55/* shDK */, _52/* shDH */));
                return new T3(0,new T(function(){
                  return B(_4u/* Data.Map.Base.link */(_1O/* LudoJS.Blue */, _50/* shDD */, _4V/* shDv */, _56/* shDO */.a));
                }),_56/* shDO */.b,_56/* shDO */.c);
              case 2:
                var _57/* shDU */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shDs */>>1, _1Q/* LudoJS.Red */, _55/* shDK */, _52/* shDH */));
                return new T3(0,new T(function(){
                  return B(_4u/* Data.Map.Base.link */(_1O/* LudoJS.Blue */, _50/* shDD */, _4V/* shDv */, _57/* shDU */.a));
                }),_57/* shDU */.b,_57/* shDU */.c);
              default:
                var _58/* shE0 */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shDs */>>1, _1S/* LudoJS.Yellow */, _55/* shDK */, _52/* shDH */));
                return new T3(0,new T(function(){
                  return B(_4u/* Data.Map.Base.link */(_1O/* LudoJS.Blue */, _50/* shDD */, _4V/* shDv */, _58/* shE0 */.a));
                }),_58/* shE0 */.b,_58/* shE0 */.c);
            }
            break;
          case 1:
            switch(E(_54/* shDJ */)){
              case 2:
                var _59/* shE7 */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shDs */>>1, _1Q/* LudoJS.Red */, _55/* shDK */, _52/* shDH */));
                return new T3(0,new T(function(){
                  return B(_4u/* Data.Map.Base.link */(_1P/* LudoJS.Green */, _50/* shDD */, _4V/* shDv */, _59/* shE7 */.a));
                }),_59/* shE7 */.b,_59/* shE7 */.c);
              case 3:
                var _5a/* shEd */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shDs */>>1, _1S/* LudoJS.Yellow */, _55/* shDK */, _52/* shDH */));
                return new T3(0,new T(function(){
                  return B(_4u/* Data.Map.Base.link */(_1P/* LudoJS.Green */, _50/* shDD */, _4V/* shDv */, _5a/* shEd */.a));
                }),_5a/* shEd */.b,_5a/* shEd */.c);
              default:
                return new T3(0,_4V/* shDv */,_4/* GHC.Types.[] */,_4X/* shDy */);
            }
            break;
          case 2:
            if(E(_54/* shDJ */)==3){
              var _5b/* shEk */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shDs */>>1, _1S/* LudoJS.Yellow */, _55/* shDK */, _52/* shDH */));
              return new T3(0,new T(function(){
                return B(_4u/* Data.Map.Base.link */(_1Q/* LudoJS.Red */, _50/* shDD */, _4V/* shDv */, _5b/* shEk */.a));
              }),_5b/* shEk */.b,_5b/* shEk */.c);
            }else{
              return new T3(0,_4V/* shDv */,_4/* GHC.Types.[] */,_4X/* shDy */);
            }
            break;
          default:
            var _5c/* shEp */ = E(_54/* shDJ */);
            return new T3(0,_4V/* shDv */,_4/* GHC.Types.[] */,_4X/* shDy */);
        }
      }
    }
  }
},
_5d/* $spoly_go10 */ = function(_5e/* shFO */, _5f/* shFP */, _5g/* shFQ */){
  var _5h/* shFR */ = E(_5f/* shFP */);
  return new F(function(){return _3u/* LudoJS.poly_go10 */(B(_3j/* LudoJS.$sinsert_$sgo10 */(_5h/* shFR */.a, _5h/* shFR */.b, _5e/* shFO */)), _5g/* shFQ */);});
},
_5i/* $wpoly_go10 */ = function(_5j/* shGR */, _5k/* shGS */, _5l/* shGT */){
  var _5m/* shGU */ = E(_5l/* shGT */);
  if(!_5m/* shGU */._){
    return E(_5k/* shGS */);
  }else{
    var _5n/* shGX */ = E(_5m/* shGU */.a),
    _5o/* shGY */ = _5n/* shGX */.a,
    _5p/* shGZ */ = _5n/* shGX */.b,
    _5q/* shH0 */ = E(_5m/* shGU */.b);
    if(!_5q/* shH0 */._){
      return new F(function(){return _3I/* Data.Map.Base.insertMax */(_5o/* shGY */, _5p/* shGZ */, _5k/* shGS */);});
    }else{
      var _5r/* shH3 */ = E(_5q/* shH0 */.a),
      _5s/* shH4 */ = _5r/* shH3 */.a,
      _5t/* shH6 */ = function(_5u/* shH7 */){
        var _5v/* shH8 */ = B(_4L/* LudoJS.$s$wpoly_create */(_5j/* shGR */, _5s/* shH4 */, _5r/* shH3 */.b, _5q/* shH0 */.b)),
        _5w/* shH9 */ = _5v/* shH8 */.a,
        _5x/* shHc */ = E(_5v/* shH8 */.c);
        if(!_5x/* shHc */._){
          return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(_5j/* shGR */<<1, B(_4u/* Data.Map.Base.link */(_5o/* shGY */, _5p/* shGZ */, _5k/* shGS */, _5w/* shH9 */)), _5v/* shH8 */.b);});
        }else{
          return new F(function(){return _5d/* LudoJS.$spoly_go10 */(B(_4u/* Data.Map.Base.link */(_5o/* shGY */, _5p/* shGZ */, _5k/* shGS */, _5w/* shH9 */)), _5x/* shHc */.a, _5x/* shHc */.b);});
        }
      };
      switch(E(_5o/* shGY */)){
        case 0:
          switch(E(_5s/* shH4 */)){
            case 0:
              return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(_5k/* shGS */, _1O/* LudoJS.Blue */, _5p/* shGZ */, _5q/* shH0 */);});
              break;
            case 1:
              return new F(function(){return _5t/* shH6 */(_/* EXTERNAL */);});
              break;
            case 2:
              return new F(function(){return _5t/* shH6 */(_/* EXTERNAL */);});
              break;
            default:
              return new F(function(){return _5t/* shH6 */(_/* EXTERNAL */);});
          }
          break;
        case 1:
          switch(E(_5s/* shH4 */)){
            case 2:
              return new F(function(){return _5t/* shH6 */(_/* EXTERNAL */);});
              break;
            case 3:
              return new F(function(){return _5t/* shH6 */(_/* EXTERNAL */);});
              break;
            default:
              return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(_5k/* shGS */, _1P/* LudoJS.Green */, _5p/* shGZ */, _5q/* shH0 */);});
          }
          break;
        case 2:
          if(E(_5s/* shH4 */)==3){
            return new F(function(){return _5t/* shH6 */(_/* EXTERNAL */);});
          }else{
            return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(_5k/* shGS */, _1Q/* LudoJS.Red */, _5p/* shGZ */, _5q/* shH0 */);});
          }
          break;
        default:
          var _5y/* shHm */ = E(_5s/* shH4 */);
          return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(_5k/* shGS */, _1S/* LudoJS.Yellow */, _5p/* shGZ */, _5q/* shH0 */);});
      }
    }
  }
},
_5z/* $sfromList */ = function(_5A/* shJc */){
  var _5B/* shJd */ = E(_5A/* shJc */);
  if(!_5B/* shJd */._){
    return new T0(1);
  }else{
    var _5C/* shJg */ = E(_5B/* shJd */.a),
    _5D/* shJh */ = _5C/* shJg */.a,
    _5E/* shJi */ = _5C/* shJg */.b,
    _5F/* shJj */ = E(_5B/* shJd */.b);
    if(!_5F/* shJj */._){
      return new T5(0,1,E(_5D/* shJh */),_5E/* shJi */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
    }else{
      var _5G/* shJm */ = _5F/* shJj */.b,
      _5H/* shJn */ = E(_5F/* shJj */.a),
      _5I/* shJo */ = _5H/* shJn */.a,
      _5J/* shJp */ = _5H/* shJn */.b;
      switch(E(_5D/* shJh */)){
        case 0:
          switch(E(_5I/* shJo */)){
            case 0:
              return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_1O/* LudoJS.Blue */),_5E/* shJi */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _1O/* LudoJS.Blue */, _5J/* shJp */, _5G/* shJm */);});
              break;
            case 1:
              return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_1O/* LudoJS.Blue */),_5E/* shJi */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5F/* shJj */);});
              break;
            case 2:
              return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_1O/* LudoJS.Blue */),_5E/* shJi */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5F/* shJj */);});
              break;
            default:
              return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_1O/* LudoJS.Blue */),_5E/* shJi */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5F/* shJj */);});
          }
          break;
        case 1:
          var _5K/* shJw */ = E(_5I/* shJo */);
          switch(_5K/* shJw */){
            case 2:
              return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_1P/* LudoJS.Green */),_5E/* shJi */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5F/* shJj */);});
              break;
            case 3:
              return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_1P/* LudoJS.Green */),_5E/* shJi */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5F/* shJj */);});
              break;
            default:
              return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_1P/* LudoJS.Green */),_5E/* shJi */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5K/* shJw */, _5J/* shJp */, _5G/* shJm */);});
          }
          break;
        case 2:
          var _5L/* shJA */ = E(_5I/* shJo */);
          if(_5L/* shJA */==3){
            return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_1Q/* LudoJS.Red */),_5E/* shJi */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5F/* shJj */);});
          }else{
            return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_1Q/* LudoJS.Red */),_5E/* shJi */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5L/* shJA */, _5J/* shJp */, _5G/* shJm */);});
          }
          break;
        default:
          return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_1S/* LudoJS.Yellow */),_5E/* shJi */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), E(_5I/* shJo */), _5J/* shJp */, _5G/* shJm */);});
      }
    }
  }
},
_5M/* $w$cshowsPrec */ = function(_5N/* si8F */, _5O/* si8G */){
  switch(E(_5N/* si8F */)){
    case 0:
      return new F(function(){return _q/* GHC.Base.++ */(_1u/* LudoJS.$fFromAnyGameState14 */, _5O/* si8G */);});
      break;
    case 1:
      return new F(function(){return _q/* GHC.Base.++ */(_1t/* LudoJS.$fFromAnyGameState13 */, _5O/* si8G */);});
      break;
    case 2:
      return new F(function(){return _q/* GHC.Base.++ */(_1s/* LudoJS.$fFromAnyGameState12 */, _5O/* si8G */);});
      break;
    default:
      return new F(function(){return _q/* GHC.Base.++ */(_1r/* LudoJS.$fFromAnyGameState11 */, _5O/* si8G */);});
  }
},
_5P/* itos */ = function(_5Q/* sf6u */, _5R/* sf6v */){
  var _5S/* sf6x */ = jsShowI/* EXTERNAL */(_5Q/* sf6u */);
  return new F(function(){return _q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(_5S/* sf6x */), _5R/* sf6v */);});
},
_5T/* shows7 */ = 41,
_5U/* shows8 */ = 40,
_5V/* $wshowSignedInt */ = function(_5W/* sf6C */, _5X/* sf6D */, _5Y/* sf6E */){
  if(_5X/* sf6D */>=0){
    return new F(function(){return _5P/* GHC.Show.itos */(_5X/* sf6D */, _5Y/* sf6E */);});
  }else{
    if(_5W/* sf6C */<=6){
      return new F(function(){return _5P/* GHC.Show.itos */(_5X/* sf6D */, _5Y/* sf6E */);});
    }else{
      return new T2(1,_5U/* GHC.Show.shows8 */,new T(function(){
        var _5Z/* sf6K */ = jsShowI/* EXTERNAL */(_5X/* sf6D */);
        return B(_q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(_5Z/* sf6K */), new T2(1,_5T/* GHC.Show.shows7 */,_5Y/* sf6E */)));
      }));
    }
  }
},
_60/* Out */ = __Z/* EXTERNAL */,
_61/* lvl14 */ = 4,
_62/* a32 */ = new T2(1,_61/* LudoJS.lvl14 */,_4/* GHC.Types.[] */),
_63/* play10 */ = 3,
_64/* a33 */ = new T2(1,_63/* LudoJS.play10 */,_62/* LudoJS.a32 */),
_65/* lvl19 */ = 2,
_66/* a34 */ = new T2(1,_65/* LudoJS.lvl19 */,_64/* LudoJS.a33 */),
_67/* lvl32 */ = new T(function(){
  return B(_1o/* Control.Exception.Base.patError */("LudoJS.hs:(113,17)-(115,114)|case"));
}),
_68/* lvl33 */ = "Active",
_69/* lvl34 */ = "Out",
_6a/* lvl7 */ = new T2(1,_1Q/* LudoJS.Red */,_4/* GHC.Types.[] */),
_6b/* lvl8 */ = new T2(1,_1S/* LudoJS.Yellow */,_6a/* LudoJS.lvl7 */),
_6c/* $fFromAnyGameState6 */ = function(_6d/* si8M */, _/* EXTERNAL */){
  var _6e/* si8O */ = E(_6d/* si8M */),
  _6f/* si8W */ = __get/* EXTERNAL */(_6e/* si8O */, toJSStr/* EXTERNAL */(B(_q/* GHC.Base.++ */(_1u/* LudoJS.$fFromAnyGameState14 */, _4/* GHC.Types.[] */)))),
  _6g/* si8Y */ = _6f/* si8W */,
  _6h/* si90 */ = function(_6i/*  sica */, _6j/*  sicb */, _/* EXTERNAL */){
    while(1){
      var _6k/*  si90 */ = B((function(_6l/* sica */, _6m/* sicb */, _/* EXTERNAL */){
        var _6n/* sicd */ = E(_6l/* sica */);
        if(!_6n/* sicd */._){
          return _6m/* sicb */;
        }else{
          var _6o/* sicf */ = _6n/* sicd */.b,
          _6p/* sicg */ = E(_6n/* sicd */.a),
          _6q/* sick */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _6p/* sicg */, _4/* GHC.Types.[] */))),
          _6r/* sico */ = __has/* EXTERNAL */(_6g/* si8Y */, _6q/* sick */);
          if(!E(_6r/* sico */)){
            var _6s/*   sicb */ = _6m/* sicb */;
            _6i/*  sica */ = _6o/* sicf */;
            _6j/*  sicb */ = _6s/*   sicb */;
            return __continue/* EXTERNAL */;
          }else{
            var _6t/* sict */ = __get/* EXTERNAL */(_6g/* si8Y */, _6q/* sick */),
            _6u/* sicw */ = E(_1N/* LudoJS.$fToAnyOption5 */),
            _6v/* sicz */ = __get/* EXTERNAL */(_6t/* sict */, _6u/* sicw */),
            _6w/* sicD */ = String/* EXTERNAL */(_6v/* sicz */),
            _6x/* sicG */ = E(_69/* LudoJS.lvl34 */),
            _6y/* sicJ */ = strEq/* EXTERNAL */(_6w/* sicD */, _6x/* sicG */);
            if(!E(_6y/* sicJ */)){
              var _6z/* sidU */ = E(_68/* LudoJS.lvl33 */),
              _6A/* sidX */ = strEq/* EXTERNAL */(_6w/* sicD */, _6z/* sidU */);
              if(!E(_6A/* sidX */)){
                return E(_67/* LudoJS.lvl32 */);
              }else{
                var _6B/* sie1 */ = E(_1M/* LudoJS.$fToAnyOption1 */),
                _6C/* sie4 */ = __get/* EXTERNAL */(_6t/* sict */, _6B/* sie1 */),
                _6D/* sie7 */ = function(_6E/*  sie8 */, _6F/*  sie9 */, _/* EXTERNAL */){
                  while(1){
                    var _6G/*  sie7 */ = B((function(_6H/* sie8 */, _6I/* sie9 */, _/* EXTERNAL */){
                      var _6J/* sieb */ = E(_6H/* sie8 */);
                      if(!_6J/* sieb */._){
                        return _6I/* sie9 */;
                      }else{
                        var _6K/* sied */ = _6J/* sieb */.b,
                        _6L/* siee */ = E(_6J/* sieb */.a),
                        _6M/* siei */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _6L/* siee */, _4/* GHC.Types.[] */))),
                        _6N/* siem */ = __has/* EXTERNAL */(_6g/* si8Y */, _6M/* siei */);
                        if(!E(_6N/* siem */)){
                          var _6O/*   sie9 */ = _6I/* sie9 */;
                          _6E/*  sie8 */ = _6K/* sied */;
                          _6F/*  sie9 */ = _6O/*   sie9 */;
                          return __continue/* EXTERNAL */;
                        }else{
                          var _6P/* sier */ = __get/* EXTERNAL */(_6g/* si8Y */, _6M/* siei */),
                          _6Q/* siev */ = __get/* EXTERNAL */(_6P/* sier */, _6u/* sicw */),
                          _6R/* siez */ = String/* EXTERNAL */(_6Q/* siev */),
                          _6S/* sieD */ = strEq/* EXTERNAL */(_6R/* siez */, _6x/* sicG */);
                          if(!E(_6S/* sieD */)){
                            var _6T/* sieL */ = strEq/* EXTERNAL */(_6R/* siez */, _6z/* sidU */);
                            if(!E(_6T/* sieL */)){
                              return E(_67/* LudoJS.lvl32 */);
                            }else{
                              var _6U/* sieQ */ = __get/* EXTERNAL */(_6P/* sier */, _6B/* sie1 */),
                              _6V/* sif5 */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_6I/* sie9 */, new T2(1,new T2(0,_6L/* siee */,new T1(1,new T(function(){
                                  var _6W/* sieU */ = Number/* EXTERNAL */(_6U/* sieQ */);
                                  return jsTrunc/* EXTERNAL */(_6W/* sieU */);
                                }))),_4/* GHC.Types.[] */)));
                              });
                              _6E/*  sie8 */ = _6K/* sied */;
                              _6F/*  sie9 */ = _6V/* sif5 */;
                              return __continue/* EXTERNAL */;
                            }
                          }else{
                            _6E/*  sie8 */ = _6K/* sied */;
                            _6F/*  sie9 */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_6I/* sie9 */, new T2(1,new T2(0,_6L/* siee */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                            });
                            return __continue/* EXTERNAL */;
                          }
                        }
                      }
                    })(_6E/*  sie8 */, _6F/*  sie9 */, _/* EXTERNAL */));
                    if(_6G/*  sie7 */!=__continue/* EXTERNAL */){
                      return _6G/*  sie7 */;
                    }
                  }
                },
                _6X/* sifj */ = new T(function(){
                  return B(_q/* GHC.Base.++ */(_6m/* sicb */, new T2(1,new T2(0,_6p/* sicg */,new T1(1,new T(function(){
                    var _6Y/* sif8 */ = Number/* EXTERNAL */(_6C/* sie4 */);
                    return jsTrunc/* EXTERNAL */(_6Y/* sif8 */);
                  }))),_4/* GHC.Types.[] */)));
                });
                return new F(function(){return _6D/* sie7 */(_6o/* sicf */, _6X/* sifj */, _/* EXTERNAL */);});
              }
            }else{
              var _6Z/* sicN */ = function(_70/*  sicO */, _71/*  sicP */, _/* EXTERNAL */){
                while(1){
                  var _72/*  sicN */ = B((function(_73/* sicO */, _74/* sicP */, _/* EXTERNAL */){
                    var _75/* sicR */ = E(_73/* sicO */);
                    if(!_75/* sicR */._){
                      return _74/* sicP */;
                    }else{
                      var _76/* sicT */ = _75/* sicR */.b,
                      _77/* sicU */ = E(_75/* sicR */.a),
                      _78/* sicY */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _77/* sicU */, _4/* GHC.Types.[] */))),
                      _79/* sid2 */ = __has/* EXTERNAL */(_6g/* si8Y */, _78/* sicY */);
                      if(!E(_79/* sid2 */)){
                        var _7a/*   sicP */ = _74/* sicP */;
                        _70/*  sicO */ = _76/* sicT */;
                        _71/*  sicP */ = _7a/*   sicP */;
                        return __continue/* EXTERNAL */;
                      }else{
                        var _7b/* sid7 */ = __get/* EXTERNAL */(_6g/* si8Y */, _78/* sicY */),
                        _7c/* sidb */ = __get/* EXTERNAL */(_7b/* sid7 */, _6u/* sicw */),
                        _7d/* sidf */ = String/* EXTERNAL */(_7c/* sidb */),
                        _7e/* sidj */ = strEq/* EXTERNAL */(_7d/* sidf */, _6x/* sicG */);
                        if(!E(_7e/* sidj */)){
                          var _7f/* sidt */ = strEq/* EXTERNAL */(_7d/* sidf */, E(_68/* LudoJS.lvl33 */));
                          if(!E(_7f/* sidt */)){
                            return E(_67/* LudoJS.lvl32 */);
                          }else{
                            var _7g/* sidA */ = __get/* EXTERNAL */(_7b/* sid7 */, E(_1M/* LudoJS.$fToAnyOption1 */)),
                            _7h/* sidP */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_74/* sicP */, new T2(1,new T2(0,_77/* sicU */,new T1(1,new T(function(){
                                var _7i/* sidE */ = Number/* EXTERNAL */(_7g/* sidA */);
                                return jsTrunc/* EXTERNAL */(_7i/* sidE */);
                              }))),_4/* GHC.Types.[] */)));
                            });
                            _70/*  sicO */ = _76/* sicT */;
                            _71/*  sicP */ = _7h/* sidP */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          _70/*  sicO */ = _76/* sicT */;
                          _71/*  sicP */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_74/* sicP */, new T2(1,new T2(0,_77/* sicU */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                          });
                          return __continue/* EXTERNAL */;
                        }
                      }
                    }
                  })(_70/*  sicO */, _71/*  sicP */, _/* EXTERNAL */));
                  if(_72/*  sicN */!=__continue/* EXTERNAL */){
                    return _72/*  sicN */;
                  }
                }
              };
              return new F(function(){return _6Z/* sicN */(_6o/* sicf */, new T(function(){
                return B(_q/* GHC.Base.++ */(_6m/* sicb */, new T2(1,new T2(0,_6p/* sicg */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
              }), _/* EXTERNAL */);});
            }
          }
        }
      })(_6i/*  sica */, _6j/*  sicb */, _/* EXTERNAL */));
      if(_6k/*  si90 */!=__continue/* EXTERNAL */){
        return _6k/*  si90 */;
      }
    }
  },
  _7j/* si8Z */ = function(_7k/* si91 */, _7l/* si92 */, _7m/* si93 */, _/* EXTERNAL */){
    var _7n/* si97 */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _7k/* si91 */, _4/* GHC.Types.[] */))),
    _7o/* si9b */ = __has/* EXTERNAL */(_6g/* si8Y */, _7n/* si97 */);
    if(!E(_7o/* si9b */)){
      return new F(function(){return _6h/* si90 */(_7l/* si92 */, _7m/* si93 */, _/* EXTERNAL */);});
    }else{
      var _7p/* si9g */ = __get/* EXTERNAL */(_6g/* si8Y */, _7n/* si97 */),
      _7q/* si9j */ = E(_1N/* LudoJS.$fToAnyOption5 */),
      _7r/* si9m */ = __get/* EXTERNAL */(_7p/* si9g */, _7q/* si9j */),
      _7s/* si9q */ = String/* EXTERNAL */(_7r/* si9m */),
      _7t/* si9t */ = E(_69/* LudoJS.lvl34 */),
      _7u/* si9w */ = strEq/* EXTERNAL */(_7s/* si9q */, _7t/* si9t */);
      if(!E(_7u/* si9w */)){
        var _7v/* siaI */ = E(_68/* LudoJS.lvl33 */),
        _7w/* siaL */ = strEq/* EXTERNAL */(_7s/* si9q */, _7v/* siaI */);
        if(!E(_7w/* siaL */)){
          return E(_67/* LudoJS.lvl32 */);
        }else{
          var _7x/* siaP */ = E(_1M/* LudoJS.$fToAnyOption1 */),
          _7y/* siaS */ = __get/* EXTERNAL */(_7p/* si9g */, _7x/* siaP */),
          _7z/* siaV */ = function(_7A/*  siaW */, _7B/*  siaX */, _/* EXTERNAL */){
            while(1){
              var _7C/*  siaV */ = B((function(_7D/* siaW */, _7E/* siaX */, _/* EXTERNAL */){
                var _7F/* siaZ */ = E(_7D/* siaW */);
                if(!_7F/* siaZ */._){
                  return _7E/* siaX */;
                }else{
                  var _7G/* sib1 */ = _7F/* siaZ */.b,
                  _7H/* sib2 */ = E(_7F/* siaZ */.a),
                  _7I/* sib6 */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _7H/* sib2 */, _4/* GHC.Types.[] */))),
                  _7J/* siba */ = __has/* EXTERNAL */(_6g/* si8Y */, _7I/* sib6 */);
                  if(!E(_7J/* siba */)){
                    var _7K/*   siaX */ = _7E/* siaX */;
                    _7A/*  siaW */ = _7G/* sib1 */;
                    _7B/*  siaX */ = _7K/*   siaX */;
                    return __continue/* EXTERNAL */;
                  }else{
                    var _7L/* sibf */ = __get/* EXTERNAL */(_6g/* si8Y */, _7I/* sib6 */),
                    _7M/* sibj */ = __get/* EXTERNAL */(_7L/* sibf */, _7q/* si9j */),
                    _7N/* sibn */ = String/* EXTERNAL */(_7M/* sibj */),
                    _7O/* sibr */ = strEq/* EXTERNAL */(_7N/* sibn */, _7t/* si9t */);
                    if(!E(_7O/* sibr */)){
                      var _7P/* sibz */ = strEq/* EXTERNAL */(_7N/* sibn */, _7v/* siaI */);
                      if(!E(_7P/* sibz */)){
                        return E(_67/* LudoJS.lvl32 */);
                      }else{
                        var _7Q/* sibE */ = __get/* EXTERNAL */(_7L/* sibf */, _7x/* siaP */),
                        _7R/* sibT */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_7E/* siaX */, new T2(1,new T2(0,_7H/* sib2 */,new T1(1,new T(function(){
                            var _7S/* sibI */ = Number/* EXTERNAL */(_7Q/* sibE */);
                            return jsTrunc/* EXTERNAL */(_7S/* sibI */);
                          }))),_4/* GHC.Types.[] */)));
                        });
                        _7A/*  siaW */ = _7G/* sib1 */;
                        _7B/*  siaX */ = _7R/* sibT */;
                        return __continue/* EXTERNAL */;
                      }
                    }else{
                      _7A/*  siaW */ = _7G/* sib1 */;
                      _7B/*  siaX */ = new T(function(){
                        return B(_q/* GHC.Base.++ */(_7E/* siaX */, new T2(1,new T2(0,_7H/* sib2 */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                      });
                      return __continue/* EXTERNAL */;
                    }
                  }
                }
              })(_7A/*  siaW */, _7B/*  siaX */, _/* EXTERNAL */));
              if(_7C/*  siaV */!=__continue/* EXTERNAL */){
                return _7C/*  siaV */;
              }
            }
          },
          _7T/* sic8 */ = new T(function(){
            return B(_q/* GHC.Base.++ */(_7m/* si93 */, new T2(1,new T2(0,_7k/* si91 */,new T1(1,new T(function(){
              var _7U/* sibX */ = Number/* EXTERNAL */(_7y/* siaS */);
              return jsTrunc/* EXTERNAL */(_7U/* sibX */);
            }))),_4/* GHC.Types.[] */)));
          });
          return new F(function(){return _7z/* siaV */(_7l/* si92 */, _7T/* sic8 */, _/* EXTERNAL */);});
        }
      }else{
        var _7V/* si9A */ = function(_7W/*  si9B */, _7X/*  si9C */, _/* EXTERNAL */){
          while(1){
            var _7Y/*  si9A */ = B((function(_7Z/* si9B */, _80/* si9C */, _/* EXTERNAL */){
              var _81/* si9E */ = E(_7Z/* si9B */);
              if(!_81/* si9E */._){
                return _80/* si9C */;
              }else{
                var _82/* si9G */ = _81/* si9E */.b,
                _83/* si9H */ = E(_81/* si9E */.a),
                _84/* si9L */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _83/* si9H */, _4/* GHC.Types.[] */))),
                _85/* si9P */ = __has/* EXTERNAL */(_6g/* si8Y */, _84/* si9L */);
                if(!E(_85/* si9P */)){
                  var _86/*   si9C */ = _80/* si9C */;
                  _7W/*  si9B */ = _82/* si9G */;
                  _7X/*  si9C */ = _86/*   si9C */;
                  return __continue/* EXTERNAL */;
                }else{
                  var _87/* si9U */ = __get/* EXTERNAL */(_6g/* si8Y */, _84/* si9L */),
                  _88/* si9Y */ = __get/* EXTERNAL */(_87/* si9U */, _7q/* si9j */),
                  _89/* sia2 */ = String/* EXTERNAL */(_88/* si9Y */),
                  _8a/* sia6 */ = strEq/* EXTERNAL */(_89/* sia2 */, _7t/* si9t */);
                  if(!E(_8a/* sia6 */)){
                    var _8b/* siag */ = strEq/* EXTERNAL */(_89/* sia2 */, E(_68/* LudoJS.lvl33 */));
                    if(!E(_8b/* siag */)){
                      return E(_67/* LudoJS.lvl32 */);
                    }else{
                      var _8c/* sian */ = __get/* EXTERNAL */(_87/* si9U */, E(_1M/* LudoJS.$fToAnyOption1 */)),
                      _8d/* siaC */ = new T(function(){
                        return B(_q/* GHC.Base.++ */(_80/* si9C */, new T2(1,new T2(0,_83/* si9H */,new T1(1,new T(function(){
                          var _8e/* siar */ = Number/* EXTERNAL */(_8c/* sian */);
                          return jsTrunc/* EXTERNAL */(_8e/* siar */);
                        }))),_4/* GHC.Types.[] */)));
                      });
                      _7W/*  si9B */ = _82/* si9G */;
                      _7X/*  si9C */ = _8d/* siaC */;
                      return __continue/* EXTERNAL */;
                    }
                  }else{
                    _7W/*  si9B */ = _82/* si9G */;
                    _7X/*  si9C */ = new T(function(){
                      return B(_q/* GHC.Base.++ */(_80/* si9C */, new T2(1,new T2(0,_83/* si9H */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                    });
                    return __continue/* EXTERNAL */;
                  }
                }
              }
            })(_7W/*  si9B */, _7X/*  si9C */, _/* EXTERNAL */));
            if(_7Y/*  si9A */!=__continue/* EXTERNAL */){
              return _7Y/*  si9A */;
            }
          }
        };
        return new F(function(){return _7V/* si9A */(_7l/* si92 */, new T(function(){
          return B(_q/* GHC.Base.++ */(_7m/* si93 */, new T2(1,new T2(0,_7k/* si91 */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
        }), _/* EXTERNAL */);});
      }
    }
  },
  _8f/* sifl */ = B(_7j/* si8Z */(1, _66/* LudoJS.a34 */, _4/* GHC.Types.[] */, _/* EXTERNAL */)),
  _8g/* sifp */ = function(_8h/* sim6 */, _/* EXTERNAL */){
    var _8i/* sim8 */ = E(_8h/* sim6 */);
    if(!_8i/* sim8 */._){
      return _4/* GHC.Types.[] */;
    }else{
      var _8j/* sim9 */ = _8i/* sim8 */.a,
      _8k/* simh */ = __get/* EXTERNAL */(_6e/* si8O */, toJSStr/* EXTERNAL */(B(_5M/* LudoJS.$w$cshowsPrec */(_8j/* sim9 */, _4/* GHC.Types.[] */)))),
      _8l/* simj */ = _8k/* simh */,
      _8m/* siml */ = function(_8n/*  sipv */, _8o/*  sipw */, _/* EXTERNAL */){
        while(1){
          var _8p/*  siml */ = B((function(_8q/* sipv */, _8r/* sipw */, _/* EXTERNAL */){
            var _8s/* sipy */ = E(_8q/* sipv */);
            if(!_8s/* sipy */._){
              return _8r/* sipw */;
            }else{
              var _8t/* sipA */ = _8s/* sipy */.b,
              _8u/* sipB */ = E(_8s/* sipy */.a),
              _8v/* sipF */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _8u/* sipB */, _4/* GHC.Types.[] */))),
              _8w/* sipJ */ = __has/* EXTERNAL */(_8l/* simj */, _8v/* sipF */);
              if(!E(_8w/* sipJ */)){
                var _8x/*   sipw */ = _8r/* sipw */;
                _8n/*  sipv */ = _8t/* sipA */;
                _8o/*  sipw */ = _8x/*   sipw */;
                return __continue/* EXTERNAL */;
              }else{
                var _8y/* sipO */ = __get/* EXTERNAL */(_8l/* simj */, _8v/* sipF */),
                _8z/* sipR */ = E(_1N/* LudoJS.$fToAnyOption5 */),
                _8A/* sipU */ = __get/* EXTERNAL */(_8y/* sipO */, _8z/* sipR */),
                _8B/* sipY */ = String/* EXTERNAL */(_8A/* sipU */),
                _8C/* siq1 */ = E(_69/* LudoJS.lvl34 */),
                _8D/* siq4 */ = strEq/* EXTERNAL */(_8B/* sipY */, _8C/* siq1 */);
                if(!E(_8D/* siq4 */)){
                  var _8E/* sirf */ = E(_68/* LudoJS.lvl33 */),
                  _8F/* siri */ = strEq/* EXTERNAL */(_8B/* sipY */, _8E/* sirf */);
                  if(!E(_8F/* siri */)){
                    return E(_67/* LudoJS.lvl32 */);
                  }else{
                    var _8G/* sirm */ = E(_1M/* LudoJS.$fToAnyOption1 */),
                    _8H/* sirp */ = __get/* EXTERNAL */(_8y/* sipO */, _8G/* sirm */),
                    _8I/* sirs */ = function(_8J/*  sirt */, _8K/*  siru */, _/* EXTERNAL */){
                      while(1){
                        var _8L/*  sirs */ = B((function(_8M/* sirt */, _8N/* siru */, _/* EXTERNAL */){
                          var _8O/* sirw */ = E(_8M/* sirt */);
                          if(!_8O/* sirw */._){
                            return _8N/* siru */;
                          }else{
                            var _8P/* siry */ = _8O/* sirw */.b,
                            _8Q/* sirz */ = E(_8O/* sirw */.a),
                            _8R/* sirD */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _8Q/* sirz */, _4/* GHC.Types.[] */))),
                            _8S/* sirH */ = __has/* EXTERNAL */(_8l/* simj */, _8R/* sirD */);
                            if(!E(_8S/* sirH */)){
                              var _8T/*   siru */ = _8N/* siru */;
                              _8J/*  sirt */ = _8P/* siry */;
                              _8K/*  siru */ = _8T/*   siru */;
                              return __continue/* EXTERNAL */;
                            }else{
                              var _8U/* sirM */ = __get/* EXTERNAL */(_8l/* simj */, _8R/* sirD */),
                              _8V/* sirQ */ = __get/* EXTERNAL */(_8U/* sirM */, _8z/* sipR */),
                              _8W/* sirU */ = String/* EXTERNAL */(_8V/* sirQ */),
                              _8X/* sirY */ = strEq/* EXTERNAL */(_8W/* sirU */, _8C/* siq1 */);
                              if(!E(_8X/* sirY */)){
                                var _8Y/* sis6 */ = strEq/* EXTERNAL */(_8W/* sirU */, _8E/* sirf */);
                                if(!E(_8Y/* sis6 */)){
                                  return E(_67/* LudoJS.lvl32 */);
                                }else{
                                  var _8Z/* sisb */ = __get/* EXTERNAL */(_8U/* sirM */, _8G/* sirm */),
                                  _90/* sisq */ = new T(function(){
                                    return B(_q/* GHC.Base.++ */(_8N/* siru */, new T2(1,new T2(0,_8Q/* sirz */,new T1(1,new T(function(){
                                      var _91/* sisf */ = Number/* EXTERNAL */(_8Z/* sisb */);
                                      return jsTrunc/* EXTERNAL */(_91/* sisf */);
                                    }))),_4/* GHC.Types.[] */)));
                                  });
                                  _8J/*  sirt */ = _8P/* siry */;
                                  _8K/*  siru */ = _90/* sisq */;
                                  return __continue/* EXTERNAL */;
                                }
                              }else{
                                _8J/*  sirt */ = _8P/* siry */;
                                _8K/*  siru */ = new T(function(){
                                  return B(_q/* GHC.Base.++ */(_8N/* siru */, new T2(1,new T2(0,_8Q/* sirz */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                                });
                                return __continue/* EXTERNAL */;
                              }
                            }
                          }
                        })(_8J/*  sirt */, _8K/*  siru */, _/* EXTERNAL */));
                        if(_8L/*  sirs */!=__continue/* EXTERNAL */){
                          return _8L/*  sirs */;
                        }
                      }
                    },
                    _92/* sisE */ = new T(function(){
                      return B(_q/* GHC.Base.++ */(_8r/* sipw */, new T2(1,new T2(0,_8u/* sipB */,new T1(1,new T(function(){
                        var _93/* sist */ = Number/* EXTERNAL */(_8H/* sirp */);
                        return jsTrunc/* EXTERNAL */(_93/* sist */);
                      }))),_4/* GHC.Types.[] */)));
                    });
                    return new F(function(){return _8I/* sirs */(_8t/* sipA */, _92/* sisE */, _/* EXTERNAL */);});
                  }
                }else{
                  var _94/* siq8 */ = function(_95/*  siq9 */, _96/*  siqa */, _/* EXTERNAL */){
                    while(1){
                      var _97/*  siq8 */ = B((function(_98/* siq9 */, _99/* siqa */, _/* EXTERNAL */){
                        var _9a/* siqc */ = E(_98/* siq9 */);
                        if(!_9a/* siqc */._){
                          return _99/* siqa */;
                        }else{
                          var _9b/* siqe */ = _9a/* siqc */.b,
                          _9c/* siqf */ = E(_9a/* siqc */.a),
                          _9d/* siqj */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _9c/* siqf */, _4/* GHC.Types.[] */))),
                          _9e/* siqn */ = __has/* EXTERNAL */(_8l/* simj */, _9d/* siqj */);
                          if(!E(_9e/* siqn */)){
                            var _9f/*   siqa */ = _99/* siqa */;
                            _95/*  siq9 */ = _9b/* siqe */;
                            _96/*  siqa */ = _9f/*   siqa */;
                            return __continue/* EXTERNAL */;
                          }else{
                            var _9g/* siqs */ = __get/* EXTERNAL */(_8l/* simj */, _9d/* siqj */),
                            _9h/* siqw */ = __get/* EXTERNAL */(_9g/* siqs */, _8z/* sipR */),
                            _9i/* siqA */ = String/* EXTERNAL */(_9h/* siqw */),
                            _9j/* siqE */ = strEq/* EXTERNAL */(_9i/* siqA */, _8C/* siq1 */);
                            if(!E(_9j/* siqE */)){
                              var _9k/* siqO */ = strEq/* EXTERNAL */(_9i/* siqA */, E(_68/* LudoJS.lvl33 */));
                              if(!E(_9k/* siqO */)){
                                return E(_67/* LudoJS.lvl32 */);
                              }else{
                                var _9l/* siqV */ = __get/* EXTERNAL */(_9g/* siqs */, E(_1M/* LudoJS.$fToAnyOption1 */)),
                                _9m/* sira */ = new T(function(){
                                  return B(_q/* GHC.Base.++ */(_99/* siqa */, new T2(1,new T2(0,_9c/* siqf */,new T1(1,new T(function(){
                                    var _9n/* siqZ */ = Number/* EXTERNAL */(_9l/* siqV */);
                                    return jsTrunc/* EXTERNAL */(_9n/* siqZ */);
                                  }))),_4/* GHC.Types.[] */)));
                                });
                                _95/*  siq9 */ = _9b/* siqe */;
                                _96/*  siqa */ = _9m/* sira */;
                                return __continue/* EXTERNAL */;
                              }
                            }else{
                              _95/*  siq9 */ = _9b/* siqe */;
                              _96/*  siqa */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_99/* siqa */, new T2(1,new T2(0,_9c/* siqf */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                              });
                              return __continue/* EXTERNAL */;
                            }
                          }
                        }
                      })(_95/*  siq9 */, _96/*  siqa */, _/* EXTERNAL */));
                      if(_97/*  siq8 */!=__continue/* EXTERNAL */){
                        return _97/*  siq8 */;
                      }
                    }
                  };
                  return new F(function(){return _94/* siq8 */(_8t/* sipA */, new T(function(){
                    return B(_q/* GHC.Base.++ */(_8r/* sipw */, new T2(1,new T2(0,_8u/* sipB */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                  }), _/* EXTERNAL */);});
                }
              }
            }
          })(_8n/*  sipv */, _8o/*  sipw */, _/* EXTERNAL */));
          if(_8p/*  siml */!=__continue/* EXTERNAL */){
            return _8p/*  siml */;
          }
        }
      },
      _9o/* simk */ = function(_9p/* simm */, _9q/* simn */, _9r/* simo */, _/* EXTERNAL */){
        var _9s/* sims */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _9p/* simm */, _4/* GHC.Types.[] */))),
        _9t/* simw */ = __has/* EXTERNAL */(_8l/* simj */, _9s/* sims */);
        if(!E(_9t/* simw */)){
          return new F(function(){return _8m/* siml */(_9q/* simn */, _9r/* simo */, _/* EXTERNAL */);});
        }else{
          var _9u/* simB */ = __get/* EXTERNAL */(_8l/* simj */, _9s/* sims */),
          _9v/* simE */ = E(_1N/* LudoJS.$fToAnyOption5 */),
          _9w/* simH */ = __get/* EXTERNAL */(_9u/* simB */, _9v/* simE */),
          _9x/* simL */ = String/* EXTERNAL */(_9w/* simH */),
          _9y/* simO */ = E(_69/* LudoJS.lvl34 */),
          _9z/* simR */ = strEq/* EXTERNAL */(_9x/* simL */, _9y/* simO */);
          if(!E(_9z/* simR */)){
            var _9A/* sio3 */ = E(_68/* LudoJS.lvl33 */),
            _9B/* sio6 */ = strEq/* EXTERNAL */(_9x/* simL */, _9A/* sio3 */);
            if(!E(_9B/* sio6 */)){
              return E(_67/* LudoJS.lvl32 */);
            }else{
              var _9C/* sioa */ = E(_1M/* LudoJS.$fToAnyOption1 */),
              _9D/* siod */ = __get/* EXTERNAL */(_9u/* simB */, _9C/* sioa */),
              _9E/* siog */ = function(_9F/*  sioh */, _9G/*  sioi */, _/* EXTERNAL */){
                while(1){
                  var _9H/*  siog */ = B((function(_9I/* sioh */, _9J/* sioi */, _/* EXTERNAL */){
                    var _9K/* siok */ = E(_9I/* sioh */);
                    if(!_9K/* siok */._){
                      return _9J/* sioi */;
                    }else{
                      var _9L/* siom */ = _9K/* siok */.b,
                      _9M/* sion */ = E(_9K/* siok */.a),
                      _9N/* sior */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _9M/* sion */, _4/* GHC.Types.[] */))),
                      _9O/* siov */ = __has/* EXTERNAL */(_8l/* simj */, _9N/* sior */);
                      if(!E(_9O/* siov */)){
                        var _9P/*   sioi */ = _9J/* sioi */;
                        _9F/*  sioh */ = _9L/* siom */;
                        _9G/*  sioi */ = _9P/*   sioi */;
                        return __continue/* EXTERNAL */;
                      }else{
                        var _9Q/* sioA */ = __get/* EXTERNAL */(_8l/* simj */, _9N/* sior */),
                        _9R/* sioE */ = __get/* EXTERNAL */(_9Q/* sioA */, _9v/* simE */),
                        _9S/* sioI */ = String/* EXTERNAL */(_9R/* sioE */),
                        _9T/* sioM */ = strEq/* EXTERNAL */(_9S/* sioI */, _9y/* simO */);
                        if(!E(_9T/* sioM */)){
                          var _9U/* sioU */ = strEq/* EXTERNAL */(_9S/* sioI */, _9A/* sio3 */);
                          if(!E(_9U/* sioU */)){
                            return E(_67/* LudoJS.lvl32 */);
                          }else{
                            var _9V/* sioZ */ = __get/* EXTERNAL */(_9Q/* sioA */, _9C/* sioa */),
                            _9W/* sipe */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_9J/* sioi */, new T2(1,new T2(0,_9M/* sion */,new T1(1,new T(function(){
                                var _9X/* sip3 */ = Number/* EXTERNAL */(_9V/* sioZ */);
                                return jsTrunc/* EXTERNAL */(_9X/* sip3 */);
                              }))),_4/* GHC.Types.[] */)));
                            });
                            _9F/*  sioh */ = _9L/* siom */;
                            _9G/*  sioi */ = _9W/* sipe */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          _9F/*  sioh */ = _9L/* siom */;
                          _9G/*  sioi */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_9J/* sioi */, new T2(1,new T2(0,_9M/* sion */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                          });
                          return __continue/* EXTERNAL */;
                        }
                      }
                    }
                  })(_9F/*  sioh */, _9G/*  sioi */, _/* EXTERNAL */));
                  if(_9H/*  siog */!=__continue/* EXTERNAL */){
                    return _9H/*  siog */;
                  }
                }
              },
              _9Y/* sipt */ = new T(function(){
                return B(_q/* GHC.Base.++ */(_9r/* simo */, new T2(1,new T2(0,_9p/* simm */,new T1(1,new T(function(){
                  var _9Z/* sipi */ = Number/* EXTERNAL */(_9D/* siod */);
                  return jsTrunc/* EXTERNAL */(_9Z/* sipi */);
                }))),_4/* GHC.Types.[] */)));
              });
              return new F(function(){return _9E/* siog */(_9q/* simn */, _9Y/* sipt */, _/* EXTERNAL */);});
            }
          }else{
            var _a0/* simV */ = function(_a1/*  simW */, _a2/*  simX */, _/* EXTERNAL */){
              while(1){
                var _a3/*  simV */ = B((function(_a4/* simW */, _a5/* simX */, _/* EXTERNAL */){
                  var _a6/* simZ */ = E(_a4/* simW */);
                  if(!_a6/* simZ */._){
                    return _a5/* simX */;
                  }else{
                    var _a7/* sin1 */ = _a6/* simZ */.b,
                    _a8/* sin2 */ = E(_a6/* simZ */.a),
                    _a9/* sin6 */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _a8/* sin2 */, _4/* GHC.Types.[] */))),
                    _aa/* sina */ = __has/* EXTERNAL */(_8l/* simj */, _a9/* sin6 */);
                    if(!E(_aa/* sina */)){
                      var _ab/*   simX */ = _a5/* simX */;
                      _a1/*  simW */ = _a7/* sin1 */;
                      _a2/*  simX */ = _ab/*   simX */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _ac/* sinf */ = __get/* EXTERNAL */(_8l/* simj */, _a9/* sin6 */),
                      _ad/* sinj */ = __get/* EXTERNAL */(_ac/* sinf */, _9v/* simE */),
                      _ae/* sinn */ = String/* EXTERNAL */(_ad/* sinj */),
                      _af/* sinr */ = strEq/* EXTERNAL */(_ae/* sinn */, _9y/* simO */);
                      if(!E(_af/* sinr */)){
                        var _ag/* sinB */ = strEq/* EXTERNAL */(_ae/* sinn */, E(_68/* LudoJS.lvl33 */));
                        if(!E(_ag/* sinB */)){
                          return E(_67/* LudoJS.lvl32 */);
                        }else{
                          var _ah/* sinI */ = __get/* EXTERNAL */(_ac/* sinf */, E(_1M/* LudoJS.$fToAnyOption1 */)),
                          _ai/* sinX */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_a5/* simX */, new T2(1,new T2(0,_a8/* sin2 */,new T1(1,new T(function(){
                              var _aj/* sinM */ = Number/* EXTERNAL */(_ah/* sinI */);
                              return jsTrunc/* EXTERNAL */(_aj/* sinM */);
                            }))),_4/* GHC.Types.[] */)));
                          });
                          _a1/*  simW */ = _a7/* sin1 */;
                          _a2/*  simX */ = _ai/* sinX */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        _a1/*  simW */ = _a7/* sin1 */;
                        _a2/*  simX */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_a5/* simX */, new T2(1,new T2(0,_a8/* sin2 */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                        });
                        return __continue/* EXTERNAL */;
                      }
                    }
                  }
                })(_a1/*  simW */, _a2/*  simX */, _/* EXTERNAL */));
                if(_a3/*  simV */!=__continue/* EXTERNAL */){
                  return _a3/*  simV */;
                }
              }
            };
            return new F(function(){return _a0/* simV */(_9q/* simn */, new T(function(){
              return B(_q/* GHC.Base.++ */(_9r/* simo */, new T2(1,new T2(0,_9p/* simm */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
            }), _/* EXTERNAL */);});
          }
        }
      },
      _ak/* sisG */ = B(_9o/* simk */(1, _66/* LudoJS.a34 */, _4/* GHC.Types.[] */, _/* EXTERNAL */)),
      _al/* sisJ */ = B(_8g/* sifp */(_8i/* sim8 */.b, _/* EXTERNAL */));
      return new T2(1,new T2(0,_8j/* sim9 */,_ak/* sisG */),_al/* sisJ */);
    }
  },
  _am/* sifo */ = function(_an/* sifq */, _ao/* sifr */, _/* EXTERNAL */){
    var _ap/* sifz */ = __get/* EXTERNAL */(_6e/* si8O */, toJSStr/* EXTERNAL */(B(_5M/* LudoJS.$w$cshowsPrec */(_an/* sifq */, _4/* GHC.Types.[] */)))),
    _aq/* sifB */ = _ap/* sifz */,
    _ar/* sifD */ = function(_as/*  siiN */, _at/*  siiO */, _/* EXTERNAL */){
      while(1){
        var _au/*  sifD */ = B((function(_av/* siiN */, _aw/* siiO */, _/* EXTERNAL */){
          var _ax/* siiQ */ = E(_av/* siiN */);
          if(!_ax/* siiQ */._){
            return _aw/* siiO */;
          }else{
            var _ay/* siiS */ = _ax/* siiQ */.b,
            _az/* siiT */ = E(_ax/* siiQ */.a),
            _aA/* siiX */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _az/* siiT */, _4/* GHC.Types.[] */))),
            _aB/* sij1 */ = __has/* EXTERNAL */(_aq/* sifB */, _aA/* siiX */);
            if(!E(_aB/* sij1 */)){
              var _aC/*   siiO */ = _aw/* siiO */;
              _as/*  siiN */ = _ay/* siiS */;
              _at/*  siiO */ = _aC/*   siiO */;
              return __continue/* EXTERNAL */;
            }else{
              var _aD/* sij6 */ = __get/* EXTERNAL */(_aq/* sifB */, _aA/* siiX */),
              _aE/* sij9 */ = E(_1N/* LudoJS.$fToAnyOption5 */),
              _aF/* sijc */ = __get/* EXTERNAL */(_aD/* sij6 */, _aE/* sij9 */),
              _aG/* sijg */ = String/* EXTERNAL */(_aF/* sijc */),
              _aH/* sijj */ = E(_69/* LudoJS.lvl34 */),
              _aI/* sijm */ = strEq/* EXTERNAL */(_aG/* sijg */, _aH/* sijj */);
              if(!E(_aI/* sijm */)){
                var _aJ/* sikx */ = E(_68/* LudoJS.lvl33 */),
                _aK/* sikA */ = strEq/* EXTERNAL */(_aG/* sijg */, _aJ/* sikx */);
                if(!E(_aK/* sikA */)){
                  return E(_67/* LudoJS.lvl32 */);
                }else{
                  var _aL/* sikE */ = E(_1M/* LudoJS.$fToAnyOption1 */),
                  _aM/* sikH */ = __get/* EXTERNAL */(_aD/* sij6 */, _aL/* sikE */),
                  _aN/* sikK */ = function(_aO/*  sikL */, _aP/*  sikM */, _/* EXTERNAL */){
                    while(1){
                      var _aQ/*  sikK */ = B((function(_aR/* sikL */, _aS/* sikM */, _/* EXTERNAL */){
                        var _aT/* sikO */ = E(_aR/* sikL */);
                        if(!_aT/* sikO */._){
                          return _aS/* sikM */;
                        }else{
                          var _aU/* sikQ */ = _aT/* sikO */.b,
                          _aV/* sikR */ = E(_aT/* sikO */.a),
                          _aW/* sikV */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _aV/* sikR */, _4/* GHC.Types.[] */))),
                          _aX/* sikZ */ = __has/* EXTERNAL */(_aq/* sifB */, _aW/* sikV */);
                          if(!E(_aX/* sikZ */)){
                            var _aY/*   sikM */ = _aS/* sikM */;
                            _aO/*  sikL */ = _aU/* sikQ */;
                            _aP/*  sikM */ = _aY/*   sikM */;
                            return __continue/* EXTERNAL */;
                          }else{
                            var _aZ/* sil4 */ = __get/* EXTERNAL */(_aq/* sifB */, _aW/* sikV */),
                            _b0/* sil8 */ = __get/* EXTERNAL */(_aZ/* sil4 */, _aE/* sij9 */),
                            _b1/* silc */ = String/* EXTERNAL */(_b0/* sil8 */),
                            _b2/* silg */ = strEq/* EXTERNAL */(_b1/* silc */, _aH/* sijj */);
                            if(!E(_b2/* silg */)){
                              var _b3/* silo */ = strEq/* EXTERNAL */(_b1/* silc */, _aJ/* sikx */);
                              if(!E(_b3/* silo */)){
                                return E(_67/* LudoJS.lvl32 */);
                              }else{
                                var _b4/* silt */ = __get/* EXTERNAL */(_aZ/* sil4 */, _aL/* sikE */),
                                _b5/* silI */ = new T(function(){
                                  return B(_q/* GHC.Base.++ */(_aS/* sikM */, new T2(1,new T2(0,_aV/* sikR */,new T1(1,new T(function(){
                                    var _b6/* silx */ = Number/* EXTERNAL */(_b4/* silt */);
                                    return jsTrunc/* EXTERNAL */(_b6/* silx */);
                                  }))),_4/* GHC.Types.[] */)));
                                });
                                _aO/*  sikL */ = _aU/* sikQ */;
                                _aP/*  sikM */ = _b5/* silI */;
                                return __continue/* EXTERNAL */;
                              }
                            }else{
                              _aO/*  sikL */ = _aU/* sikQ */;
                              _aP/*  sikM */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_aS/* sikM */, new T2(1,new T2(0,_aV/* sikR */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                              });
                              return __continue/* EXTERNAL */;
                            }
                          }
                        }
                      })(_aO/*  sikL */, _aP/*  sikM */, _/* EXTERNAL */));
                      if(_aQ/*  sikK */!=__continue/* EXTERNAL */){
                        return _aQ/*  sikK */;
                      }
                    }
                  },
                  _b7/* silW */ = new T(function(){
                    return B(_q/* GHC.Base.++ */(_aw/* siiO */, new T2(1,new T2(0,_az/* siiT */,new T1(1,new T(function(){
                      var _b8/* silL */ = Number/* EXTERNAL */(_aM/* sikH */);
                      return jsTrunc/* EXTERNAL */(_b8/* silL */);
                    }))),_4/* GHC.Types.[] */)));
                  });
                  return new F(function(){return _aN/* sikK */(_ay/* siiS */, _b7/* silW */, _/* EXTERNAL */);});
                }
              }else{
                var _b9/* sijq */ = function(_ba/*  sijr */, _bb/*  sijs */, _/* EXTERNAL */){
                  while(1){
                    var _bc/*  sijq */ = B((function(_bd/* sijr */, _be/* sijs */, _/* EXTERNAL */){
                      var _bf/* siju */ = E(_bd/* sijr */);
                      if(!_bf/* siju */._){
                        return _be/* sijs */;
                      }else{
                        var _bg/* sijw */ = _bf/* siju */.b,
                        _bh/* sijx */ = E(_bf/* siju */.a),
                        _bi/* sijB */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _bh/* sijx */, _4/* GHC.Types.[] */))),
                        _bj/* sijF */ = __has/* EXTERNAL */(_aq/* sifB */, _bi/* sijB */);
                        if(!E(_bj/* sijF */)){
                          var _bk/*   sijs */ = _be/* sijs */;
                          _ba/*  sijr */ = _bg/* sijw */;
                          _bb/*  sijs */ = _bk/*   sijs */;
                          return __continue/* EXTERNAL */;
                        }else{
                          var _bl/* sijK */ = __get/* EXTERNAL */(_aq/* sifB */, _bi/* sijB */),
                          _bm/* sijO */ = __get/* EXTERNAL */(_bl/* sijK */, _aE/* sij9 */),
                          _bn/* sijS */ = String/* EXTERNAL */(_bm/* sijO */),
                          _bo/* sijW */ = strEq/* EXTERNAL */(_bn/* sijS */, _aH/* sijj */);
                          if(!E(_bo/* sijW */)){
                            var _bp/* sik6 */ = strEq/* EXTERNAL */(_bn/* sijS */, E(_68/* LudoJS.lvl33 */));
                            if(!E(_bp/* sik6 */)){
                              return E(_67/* LudoJS.lvl32 */);
                            }else{
                              var _bq/* sikd */ = __get/* EXTERNAL */(_bl/* sijK */, E(_1M/* LudoJS.$fToAnyOption1 */)),
                              _br/* siks */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_be/* sijs */, new T2(1,new T2(0,_bh/* sijx */,new T1(1,new T(function(){
                                  var _bs/* sikh */ = Number/* EXTERNAL */(_bq/* sikd */);
                                  return jsTrunc/* EXTERNAL */(_bs/* sikh */);
                                }))),_4/* GHC.Types.[] */)));
                              });
                              _ba/*  sijr */ = _bg/* sijw */;
                              _bb/*  sijs */ = _br/* siks */;
                              return __continue/* EXTERNAL */;
                            }
                          }else{
                            _ba/*  sijr */ = _bg/* sijw */;
                            _bb/*  sijs */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_be/* sijs */, new T2(1,new T2(0,_bh/* sijx */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                            });
                            return __continue/* EXTERNAL */;
                          }
                        }
                      }
                    })(_ba/*  sijr */, _bb/*  sijs */, _/* EXTERNAL */));
                    if(_bc/*  sijq */!=__continue/* EXTERNAL */){
                      return _bc/*  sijq */;
                    }
                  }
                };
                return new F(function(){return _b9/* sijq */(_ay/* siiS */, new T(function(){
                  return B(_q/* GHC.Base.++ */(_aw/* siiO */, new T2(1,new T2(0,_az/* siiT */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                }), _/* EXTERNAL */);});
              }
            }
          }
        })(_as/*  siiN */, _at/*  siiO */, _/* EXTERNAL */));
        if(_au/*  sifD */!=__continue/* EXTERNAL */){
          return _au/*  sifD */;
        }
      }
    },
    _bt/* sifC */ = function(_bu/* sifE */, _bv/* sifF */, _bw/* sifG */, _/* EXTERNAL */){
      var _bx/* sifK */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _bu/* sifE */, _4/* GHC.Types.[] */))),
      _by/* sifO */ = __has/* EXTERNAL */(_aq/* sifB */, _bx/* sifK */);
      if(!E(_by/* sifO */)){
        return new F(function(){return _ar/* sifD */(_bv/* sifF */, _bw/* sifG */, _/* EXTERNAL */);});
      }else{
        var _bz/* sifT */ = __get/* EXTERNAL */(_aq/* sifB */, _bx/* sifK */),
        _bA/* sifW */ = E(_1N/* LudoJS.$fToAnyOption5 */),
        _bB/* sifZ */ = __get/* EXTERNAL */(_bz/* sifT */, _bA/* sifW */),
        _bC/* sig3 */ = String/* EXTERNAL */(_bB/* sifZ */),
        _bD/* sig6 */ = E(_69/* LudoJS.lvl34 */),
        _bE/* sig9 */ = strEq/* EXTERNAL */(_bC/* sig3 */, _bD/* sig6 */);
        if(!E(_bE/* sig9 */)){
          var _bF/* sihl */ = E(_68/* LudoJS.lvl33 */),
          _bG/* siho */ = strEq/* EXTERNAL */(_bC/* sig3 */, _bF/* sihl */);
          if(!E(_bG/* siho */)){
            return E(_67/* LudoJS.lvl32 */);
          }else{
            var _bH/* sihs */ = E(_1M/* LudoJS.$fToAnyOption1 */),
            _bI/* sihv */ = __get/* EXTERNAL */(_bz/* sifT */, _bH/* sihs */),
            _bJ/* sihy */ = function(_bK/*  sihz */, _bL/*  sihA */, _/* EXTERNAL */){
              while(1){
                var _bM/*  sihy */ = B((function(_bN/* sihz */, _bO/* sihA */, _/* EXTERNAL */){
                  var _bP/* sihC */ = E(_bN/* sihz */);
                  if(!_bP/* sihC */._){
                    return _bO/* sihA */;
                  }else{
                    var _bQ/* sihE */ = _bP/* sihC */.b,
                    _bR/* sihF */ = E(_bP/* sihC */.a),
                    _bS/* sihJ */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _bR/* sihF */, _4/* GHC.Types.[] */))),
                    _bT/* sihN */ = __has/* EXTERNAL */(_aq/* sifB */, _bS/* sihJ */);
                    if(!E(_bT/* sihN */)){
                      var _bU/*   sihA */ = _bO/* sihA */;
                      _bK/*  sihz */ = _bQ/* sihE */;
                      _bL/*  sihA */ = _bU/*   sihA */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _bV/* sihS */ = __get/* EXTERNAL */(_aq/* sifB */, _bS/* sihJ */),
                      _bW/* sihW */ = __get/* EXTERNAL */(_bV/* sihS */, _bA/* sifW */),
                      _bX/* sii0 */ = String/* EXTERNAL */(_bW/* sihW */),
                      _bY/* sii4 */ = strEq/* EXTERNAL */(_bX/* sii0 */, _bD/* sig6 */);
                      if(!E(_bY/* sii4 */)){
                        var _bZ/* siic */ = strEq/* EXTERNAL */(_bX/* sii0 */, _bF/* sihl */);
                        if(!E(_bZ/* siic */)){
                          return E(_67/* LudoJS.lvl32 */);
                        }else{
                          var _c0/* siih */ = __get/* EXTERNAL */(_bV/* sihS */, _bH/* sihs */),
                          _c1/* siiw */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_bO/* sihA */, new T2(1,new T2(0,_bR/* sihF */,new T1(1,new T(function(){
                              var _c2/* siil */ = Number/* EXTERNAL */(_c0/* siih */);
                              return jsTrunc/* EXTERNAL */(_c2/* siil */);
                            }))),_4/* GHC.Types.[] */)));
                          });
                          _bK/*  sihz */ = _bQ/* sihE */;
                          _bL/*  sihA */ = _c1/* siiw */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        _bK/*  sihz */ = _bQ/* sihE */;
                        _bL/*  sihA */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_bO/* sihA */, new T2(1,new T2(0,_bR/* sihF */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                        });
                        return __continue/* EXTERNAL */;
                      }
                    }
                  }
                })(_bK/*  sihz */, _bL/*  sihA */, _/* EXTERNAL */));
                if(_bM/*  sihy */!=__continue/* EXTERNAL */){
                  return _bM/*  sihy */;
                }
              }
            },
            _c3/* siiL */ = new T(function(){
              return B(_q/* GHC.Base.++ */(_bw/* sifG */, new T2(1,new T2(0,_bu/* sifE */,new T1(1,new T(function(){
                var _c4/* siiA */ = Number/* EXTERNAL */(_bI/* sihv */);
                return jsTrunc/* EXTERNAL */(_c4/* siiA */);
              }))),_4/* GHC.Types.[] */)));
            });
            return new F(function(){return _bJ/* sihy */(_bv/* sifF */, _c3/* siiL */, _/* EXTERNAL */);});
          }
        }else{
          var _c5/* sigd */ = function(_c6/*  sige */, _c7/*  sigf */, _/* EXTERNAL */){
            while(1){
              var _c8/*  sigd */ = B((function(_c9/* sige */, _ca/* sigf */, _/* EXTERNAL */){
                var _cb/* sigh */ = E(_c9/* sige */);
                if(!_cb/* sigh */._){
                  return _ca/* sigf */;
                }else{
                  var _cc/* sigj */ = _cb/* sigh */.b,
                  _cd/* sigk */ = E(_cb/* sigh */.a),
                  _ce/* sigo */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _cd/* sigk */, _4/* GHC.Types.[] */))),
                  _cf/* sigs */ = __has/* EXTERNAL */(_aq/* sifB */, _ce/* sigo */);
                  if(!E(_cf/* sigs */)){
                    var _cg/*   sigf */ = _ca/* sigf */;
                    _c6/*  sige */ = _cc/* sigj */;
                    _c7/*  sigf */ = _cg/*   sigf */;
                    return __continue/* EXTERNAL */;
                  }else{
                    var _ch/* sigx */ = __get/* EXTERNAL */(_aq/* sifB */, _ce/* sigo */),
                    _ci/* sigB */ = __get/* EXTERNAL */(_ch/* sigx */, _bA/* sifW */),
                    _cj/* sigF */ = String/* EXTERNAL */(_ci/* sigB */),
                    _ck/* sigJ */ = strEq/* EXTERNAL */(_cj/* sigF */, _bD/* sig6 */);
                    if(!E(_ck/* sigJ */)){
                      var _cl/* sigT */ = strEq/* EXTERNAL */(_cj/* sigF */, E(_68/* LudoJS.lvl33 */));
                      if(!E(_cl/* sigT */)){
                        return E(_67/* LudoJS.lvl32 */);
                      }else{
                        var _cm/* sih0 */ = __get/* EXTERNAL */(_ch/* sigx */, E(_1M/* LudoJS.$fToAnyOption1 */)),
                        _cn/* sihf */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_ca/* sigf */, new T2(1,new T2(0,_cd/* sigk */,new T1(1,new T(function(){
                            var _co/* sih4 */ = Number/* EXTERNAL */(_cm/* sih0 */);
                            return jsTrunc/* EXTERNAL */(_co/* sih4 */);
                          }))),_4/* GHC.Types.[] */)));
                        });
                        _c6/*  sige */ = _cc/* sigj */;
                        _c7/*  sigf */ = _cn/* sihf */;
                        return __continue/* EXTERNAL */;
                      }
                    }else{
                      _c6/*  sige */ = _cc/* sigj */;
                      _c7/*  sigf */ = new T(function(){
                        return B(_q/* GHC.Base.++ */(_ca/* sigf */, new T2(1,new T2(0,_cd/* sigk */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                      });
                      return __continue/* EXTERNAL */;
                    }
                  }
                }
              })(_c6/*  sige */, _c7/*  sigf */, _/* EXTERNAL */));
              if(_c8/*  sigd */!=__continue/* EXTERNAL */){
                return _c8/*  sigd */;
              }
            }
          };
          return new F(function(){return _c5/* sigd */(_bv/* sifF */, new T(function(){
            return B(_q/* GHC.Base.++ */(_bw/* sifG */, new T2(1,new T2(0,_bu/* sifE */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
          }), _/* EXTERNAL */);});
        }
      }
    },
    _cp/* silY */ = B(_bt/* sifC */(1, _66/* LudoJS.a34 */, _4/* GHC.Types.[] */, _/* EXTERNAL */)),
    _cq/* sim1 */ = B(_8g/* sifp */(_ao/* sifr */, _/* EXTERNAL */));
    return new T2(1,new T2(0,_an/* sifq */,_cp/* silY */),_cq/* sim1 */);
  },
  _cr/* sisO */ = B(_am/* sifo */(_1P/* LudoJS.Green */, _6b/* LudoJS.lvl8 */, _/* EXTERNAL */));
  return new T(function(){
    return B(_5z/* LudoJS.$sfromList */(new T2(1,new T2(0,_1O/* LudoJS.Blue */,_8f/* sifl */),_cr/* sisO */)));
  });
},
_cs/* $fFromAnyGameState7 */ = "pieces",
_ct/* $fFromAnyGameState8 */ = "numRolls",
_cu/* $fToAnyGameState11 */ = "rollNumber",
_cv/* $fToAnyGameState6 */ = "pieceIndex",
_cw/* GameFinished */ = new T0(3),
_cx/* lvl27 */ = new T(function(){
  return B(_1o/* Control.Exception.Base.patError */("LudoJS.hs:(91,5)-(95,45)|case"));
}),
_cy/* lvl28 */ = "GameFinished",
_cz/* lvl29 */ = "SelectField",
_cA/* lvl30 */ = "SelectPiece",
_cB/* lvl31 */ = "Roll",
_cC/* $wa2 */ = function(_cD/* shHq */, _/* EXTERNAL */){
  var _cE/* shHv */ = __get/* EXTERNAL */(_cD/* shHq */, E(_1F/* LudoJS.$fFromAnyGameState16 */)),
  _cF/* shHz */ = String/* EXTERNAL */(_cE/* shHv */),
  _cG/* shHF */ = strEq/* EXTERNAL */(_cF/* shHz */, E(_cB/* LudoJS.lvl31 */));
  if(!E(_cG/* shHF */)){
    var _cH/* shI4 */ = strEq/* EXTERNAL */(_cF/* shHz */, E(_cA/* LudoJS.lvl30 */));
    if(!E(_cH/* shI4 */)){
      var _cI/* shIr */ = strEq/* EXTERNAL */(_cF/* shHz */, E(_cz/* LudoJS.lvl29 */));
      if(!E(_cI/* shIr */)){
        var _cJ/* shJ3 */ = strEq/* EXTERNAL */(_cF/* shHz */, E(_cy/* LudoJS.lvl28 */));
        return (E(_cJ/* shJ3 */)==0) ? E(_cx/* LudoJS.lvl27 */) : _cw/* LudoJS.GameFinished */;
      }else{
        var _cK/* shIy */ = __get/* EXTERNAL */(_cD/* shHq */, E(_cu/* LudoJS.$fToAnyGameState11 */)),
        _cL/* shIE */ = __get/* EXTERNAL */(_cD/* shHq */, E(_cv/* LudoJS.$fToAnyGameState6 */));
        return new T2(2,new T(function(){
          var _cM/* shII */ = Number/* EXTERNAL */(_cK/* shIy */);
          return jsTrunc/* EXTERNAL */(_cM/* shII */);
        }),new T(function(){
          var _cN/* shIR */ = Number/* EXTERNAL */(_cL/* shIE */);
          return jsTrunc/* EXTERNAL */(_cN/* shIR */);
        }));
      }
    }else{
      var _cO/* shIb */ = __get/* EXTERNAL */(_cD/* shHq */, E(_cu/* LudoJS.$fToAnyGameState11 */));
      return new T1(1,new T(function(){
        var _cP/* shIf */ = Number/* EXTERNAL */(_cO/* shIb */);
        return jsTrunc/* EXTERNAL */(_cP/* shIf */);
      }));
    }
  }else{
    var _cQ/* shHM */ = __get/* EXTERNAL */(_cD/* shHq */, E(_cu/* LudoJS.$fToAnyGameState11 */));
    return new T1(0,new T(function(){
      var _cR/* shHQ */ = Number/* EXTERNAL */(_cQ/* shHM */),
      _cS/* shHU */ = jsTrunc/* EXTERNAL */(_cR/* shHQ */),
      _cT/* shHX */ = E(_cS/* shHU */);
      if(_cT/* shHX */==( -1)){
        return __Z/* EXTERNAL */;
      }else{
        return new T1(1,_cT/* shHX */);
      }
    }));
  }
},
_cU/* $wa1 */ = function(_cV/* sith */, _/* EXTERNAL */){
  var _cW/* sitm */ = __get/* EXTERNAL */(_cV/* sith */, E(_1F/* LudoJS.$fFromAnyGameState16 */)),
  _cX/* sitp */ = B(_cC/* LudoJS.$wa2 */(_cW/* sitm */, _/* EXTERNAL */)),
  _cY/* sitv */ = __get/* EXTERNAL */(_cV/* sith */, E(_1E/* LudoJS.$fFromAnyGameState15 */)),
  _cZ/* sitz */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_cY/* sitv */, _/* EXTERNAL */)),
  _d0/* sitF */ = __get/* EXTERNAL */(_cV/* sith */, E(_ct/* LudoJS.$fFromAnyGameState8 */)),
  _d1/* sitL */ = __get/* EXTERNAL */(_cV/* sith */, E(_cs/* LudoJS.$fFromAnyGameState7 */)),
  _d2/* sitP */ = B(_6c/* LudoJS.$fFromAnyGameState6 */(_d1/* sitL */, _/* EXTERNAL */)),
  _d3/* sitV */ = __get/* EXTERNAL */(_cV/* sith */, E(_1L/* LudoJS.$fFromAnyGameState5 */)),
  _d4/* sitZ */ = __arr2lst/* EXTERNAL */(0, _d3/* sitV */),
  _d5/* siu3 */ = B(_1G/* LudoJS.$fFromAnyGameState4 */(_d4/* sitZ */, _/* EXTERNAL */));
  return new T5(0,_cX/* sitp */,_cZ/* sitz */,new T(function(){
    var _d6/* siu7 */ = Number/* EXTERNAL */(_d0/* sitF */);
    return jsTrunc/* EXTERNAL */(_d6/* siu7 */);
  }),_d2/* sitP */,_d5/* siu3 */);
},
_d7/* $fShowStage2 */ = 0,
_d8/* lvl25 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Map.!: given key is not an element in the map"));
}),
_d9/* lvl26 */ = new T(function(){
  return B(err/* EXTERNAL */(_d8/* LudoJS.lvl25 */));
}),
_da/* $s!1 */ = function(_db/* shDb */, _dc/* shDc */){
  while(1){
    var _dd/* shDd */ = E(_dc/* shDc */);
    if(!_dd/* shDd */._){
      var _de/* shDf */ = _dd/* shDd */.b,
      _df/* shDg */ = _dd/* shDd */.c,
      _dg/* shDh */ = _dd/* shDd */.d,
      _dh/* shDi */ = _dd/* shDd */.e;
      switch(E(_db/* shDb */)){
        case 0:
          switch(E(_de/* shDf */)){
            case 0:
              return E(_df/* shDg */);
            case 1:
              _db/* shDb */ = _1O/* LudoJS.Blue */;
              _dc/* shDc */ = _dg/* shDh */;
              continue;
            case 2:
              _db/* shDb */ = _1O/* LudoJS.Blue */;
              _dc/* shDc */ = _dg/* shDh */;
              continue;
            default:
              _db/* shDb */ = _1O/* LudoJS.Blue */;
              _dc/* shDc */ = _dg/* shDh */;
              continue;
          }
          break;
        case 1:
          switch(E(_de/* shDf */)){
            case 0:
              _db/* shDb */ = _1P/* LudoJS.Green */;
              _dc/* shDc */ = _dh/* shDi */;
              continue;
            case 1:
              return E(_df/* shDg */);
            case 2:
              _db/* shDb */ = _1P/* LudoJS.Green */;
              _dc/* shDc */ = _dg/* shDh */;
              continue;
            default:
              _db/* shDb */ = _1P/* LudoJS.Green */;
              _dc/* shDc */ = _dg/* shDh */;
              continue;
          }
          break;
        case 2:
          switch(E(_de/* shDf */)){
            case 2:
              return E(_df/* shDg */);
            case 3:
              _db/* shDb */ = _1Q/* LudoJS.Red */;
              _dc/* shDc */ = _dg/* shDh */;
              continue;
            default:
              _db/* shDb */ = _1Q/* LudoJS.Red */;
              _dc/* shDc */ = _dh/* shDi */;
              continue;
          }
          break;
        default:
          if(E(_de/* shDf */)==3){
            return E(_df/* shDg */);
          }else{
            _db/* shDb */ = _1S/* LudoJS.Yellow */;
            _dc/* shDc */ = _dh/* shDi */;
            continue;
          }
      }
    }else{
      return E(_d9/* LudoJS.lvl26 */);
    }
  }
},
_di/* numPiecesAt2 */ = 1,
_dj/* $wa6 */ = function(_dk/* si30 */, _dl/* si31 */, _dm/* si32 */, _/* EXTERNAL */){
  var _dn/* si34 */ = function(_/* EXTERNAL */){
    return new T(function(){
      var _do/* si3c */ = function(_dp/* si3d */){
        while(1){
          var _dq/* si3e */ = E(_dp/* si3d */);
          if(!_dq/* si3e */._){
            return 0;
          }else{
            var _dr/* si3g */ = _dq/* si3e */.b,
            _ds/* si3k */ = E(E(_dq/* si3e */.a).b);
            if(!_ds/* si3k */._){
              _dp/* si3d */ = _dr/* si3g */;
              continue;
            }else{
              if(_dm/* si32 */!=E(_ds/* si3k */.a)){
                _dp/* si3d */ = _dr/* si3g */;
                continue;
              }else{
                return B(_do/* si3c */(_dr/* si3g */))+1|0;
              }
            }
          }
        }
      };
      return B(_do/* si3c */(B(_da/* LudoJS.$s!1 */(_dl/* si31 */, E(_dk/* si30 */).d))));
    });
  };
  if(_dm/* si32 */>( -1)){
    return new F(function(){return _dn/* si34 */(_/* EXTERNAL */);});
  }else{
    if(_dm/* si32 */<( -4)){
      return new F(function(){return _dn/* si34 */(_/* EXTERNAL */);});
    }else{
      return new T(function(){
        var _dt/* si3F */ = function(_du/* si3G */){
          while(1){
            var _dv/* si3H */ = E(_du/* si3G */);
            if(!_dv/* si3H */._){
              return false;
            }else{
              var _dw/* si3J */ = _dv/* si3H */.b,
              _dx/* si3K */ = E(_dv/* si3H */.a);
              if(!E(_dx/* si3K */.b)._){
                if((_dm/* si32 */+5|0)!=E(_dx/* si3K */.a)){
                  _du/* si3G */ = _dw/* si3J */;
                  continue;
                }else{
                  return true;
                }
              }else{
                _du/* si3G */ = _dw/* si3J */;
                continue;
              }
            }
          }
        };
        if(!B(_dt/* si3F */(B(_da/* LudoJS.$s!1 */(_dl/* si31 */, E(_dk/* si30 */).d))))){
          return E(_d7/* LudoJS.$fShowStage2 */);
        }else{
          return E(_di/* LudoJS.numPiecesAt2 */);
        }
      });
    }
  }
},
_dy/* elemById_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function(id){return document.getElementById(id);})");
}),
_dz/* f2 */ = new T(function(){
  return eval/* EXTERNAL */("(function(s,f){Haste[s] = f;})");
}),
_dA/* lvl2 */ = function(_/* EXTERNAL */){
  return new F(function(){return __jsNull/* EXTERNAL */();});
},
_dB/* unsafeDupablePerformIO */ = function(_dC/* s2YSa */){
  var _dD/* s2YSb */ = B(A1(_dC/* s2YSa */,_/* EXTERNAL */));
  return E(_dD/* s2YSb */);
},
_dE/* nullValue */ = new T(function(){
  return B(_dB/* GHC.IO.unsafeDupablePerformIO */(_dA/* Haste.Prim.Any.lvl2 */));
}),
_dF/* jsNull */ = new T(function(){
  return E(_dE/* Haste.Prim.Any.nullValue */);
}),
_dG/* maxInt */ = 2147483647,
_dH/* minInt */ =  -2147483648,
_dI/* $fRandomGenSMGen1 */ = new T2(0,_dH/* GHC.Base.minInt */,_dG/* GHC.Base.maxInt */),
_dJ/* $fRandomGenSMGen0_$cgenRange */ = function(_dK/* sixI */){
  return E(_dI/* System.Random.Internal.$fRandomGenSMGen1 */);
},
_dL/* $w$c+ */ = function(_dM/* s1RvK */, _dN/* s1RvL */){
  var _dO/* s1RvN */ = hs_word64ToInt64/* EXTERNAL */(_dN/* s1RvL */),
  _dP/* s1RvR */ = hs_word64ToInt64/* EXTERNAL */(_dM/* s1RvK */),
  _dQ/* s1RvV */ = hs_plusInt64/* EXTERNAL */(_dP/* s1RvR */, _dO/* s1RvN */),
  _dR/* s1RvZ */ = hs_int64ToWord64/* EXTERNAL */(_dQ/* s1RvV */);
  return E(_dR/* s1RvZ */);
},
_dS/* $w$cshiftR */ = function(_dT/* s1Ryx */, _dU/* s1Ryy */){
  if(_dU/* s1Ryy */<64){
    var _dV/* s1RyC */ = hs_uncheckedShiftRL64/* EXTERNAL */(_dT/* s1Ryx */, _dU/* s1Ryy */);
    return E(_dV/* s1RyC */);
  }else{
    var _dW/* s1RyG */ = hs_wordToWord64/* EXTERNAL */(0);
    return E(_dW/* s1RyG */);
  }
},
_dX/* $w$c* */ = function(_dY/* s1RuW */, _dZ/* s1RuX */){
  var _e0/* s1RuZ */ = hs_word64ToInt64/* EXTERNAL */(_dZ/* s1RuX */),
  _e1/* s1Rv3 */ = hs_word64ToInt64/* EXTERNAL */(_dY/* s1RuW */),
  _e2/* s1Rv7 */ = hs_timesInt64/* EXTERNAL */(_e1/* s1Rv3 */, _e0/* s1RuZ */),
  _e3/* s1Rvb */ = hs_int64ToWord64/* EXTERNAL */(_e2/* s1Rv7 */);
  return E(_e3/* s1Rvb */);
},
_e4/* $wmix64 */ = function(_e5/* saYW */){
  var _e6/* saYZ */ = hs_xor64/* EXTERNAL */(_e5/* saYW */, B(_dS/* GHC.Word.$w$cshiftR */(_e5/* saYW */, 33))),
  _e7/* saZ2 */ = B(_dX/* GHC.Word.$w$c* */(_e6/* saYZ */, new Long/* EXTERNAL */(3981806797, 4283543511, true))),
  _e8/* saZ5 */ = hs_xor64/* EXTERNAL */(_e7/* saZ2 */, B(_dS/* GHC.Word.$w$cshiftR */(_e7/* saZ2 */, 33))),
  _e9/* saZ8 */ = B(_dX/* GHC.Word.$w$c* */(_e8/* saZ5 */, new Long/* EXTERNAL */(444984403, 3301882366, true))),
  _ea/* saZb */ = hs_xor64/* EXTERNAL */(_e9/* saZ8 */, B(_dS/* GHC.Word.$w$cshiftR */(_e9/* saZ8 */, 33)));
  return E(_ea/* saZb */);
},
_eb/* () */ = 0,
_ec/* mix64 */ = function(_ed/* saZe */){
  return new F(function(){return _e4/* System.Random.SplitMix.$wmix64 */(E(_ed/* saZe */));});
},
_ee/* $fRandomGenSMGen0_$cgenShortByteString */ = function(_ef/* sixJ */, _eg/* sixK */){
  var _eh/* siz8 */ = function(_/* EXTERNAL */){
    var _ei/* sixN */ = E(_ef/* sixJ */),
    _ej/* sixP */ = function(_ek/* sixQ */){
      var _el/* sixR */ = newByteArr/* EXTERNAL */(_ek/* sixQ */),
      _em/* sixV */ = function(_en/* sixW */, _eo/* sixX */, _ep/* sixY */, _/* EXTERNAL */){
        while(1){
          if(_en/* sixW */>=quot/* EXTERNAL */(_ek/* sixQ */, 8)){
            return new T2(0,_eo/* sixX */,_ep/* sixY */);
          }else{
            var _eq/* siy3 */ = E(_ep/* sixY */),
            _er/* siy5 */ = _eq/* siy3 */.b,
            _es/* siy6 */ = E(_eo/* sixX */),
            _et/* siy8 */ = B(_dL/* GHC.Word.$w$c+ */(_eq/* siy3 */.a, _er/* siy5 */)),
            _/* EXTERNAL */ = writeOffAddr64/* EXTERNAL */(_es/* siy6 */, 0, B(_e4/* System.Random.SplitMix.$wmix64 */(_et/* siy8 */))),
            _eu/*  sixW */ = _en/* sixW */+1|0;
            _en/* sixW */ = _eu/*  sixW */;
            _eo/* sixX */ = plusAddr/* EXTERNAL */(_es/* siy6 */, 8);
            _ep/* sixY */ = new T2(0,_et/* siy8 */,_er/* siy5 */);
            _/* EXTERNAL */ = 0;
            continue;
          }
        }
      },
      _ev/* siyh */ = B(_em/* sixV */(0, _el/* sixR */, _eg/* sixK */, 0)),
      _ew/* siyk */ = E(_ev/* siyh */),
      _ex/* siym */ = _ew/* siyk */.b,
      _ey/* siyn */ = _ek/* sixQ */%8;
      if(_ey/* siyn */<=0){
        return new T2(0,_el/* sixR */,_ex/* siym */);
      }else{
        var _ez/* siyv */ = E(_ex/* siym */),
        _eA/* siyx */ = _ez/* siyv */.b,
        _eB/* siyy */ = new T(function(){
          return B(_dL/* GHC.Word.$w$c+ */(_ez/* siyv */.a, _eA/* siyx */));
        }),
        _eC/* siyA */ = function(_eD/*  siyB */, _eE/*  siyC */, _/* EXTERNAL */){
          while(1){
            var _eF/*  siyA */ = B((function(_eG/* siyB */, _eH/* siyC */, _/* EXTERNAL */){
              if(_eH/* siyC */>=_ey/* siyn */){
                return _eb/* GHC.Tuple.() */;
              }else{
                var _eI/* siyI */ = E(_eG/* siyB */),
                _eJ/* siyL */ = hs_word64ToWord/* EXTERNAL */(_eI/* siyI */),
                _/* EXTERNAL */ = writeOffAddr/* EXTERNAL */("w8", 1, plusAddr/* EXTERNAL */(E(_ew/* siyk */.a), _eH/* siyC */), 0, _eJ/* siyL */&255),
                _eK/*   siyC */ = _eH/* siyC */+1|0;
                _eD/*  siyB */ = new T(function(){
                  return B(_dS/* GHC.Word.$w$cshiftR */(_eI/* siyI */, 8));
                },1);
                _eE/*  siyC */ = _eK/*   siyC */;
                _/* EXTERNAL */ = 0;
                return __continue/* EXTERNAL */;
              }
            })(_eD/*  siyB */, _eE/*  siyC */, _/* EXTERNAL */));
            if(_eF/*  siyA */!=__continue/* EXTERNAL */){
              return _eF/*  siyA */;
            }
          }
        },
        _eL/* siyV */ = B(_eC/* siyA */(new T(function(){
          return B(_ec/* System.Random.SplitMix.mix64 */(_eB/* siyy */));
        },1), 0, 0));
        return new T2(0,_el/* sixR */,new T(function(){
          return new T2(0,E(_eB/* siyy */),_eA/* siyx */);
        }));
      }
    };
    if(0>_ei/* sixN */){
      return new F(function(){return _ej/* sixP */(0);});
    }else{
      return new F(function(){return _ej/* sixP */(_ei/* sixN */);});
    }
  };
  return new F(function(){return _dB/* GHC.IO.unsafeDupablePerformIO */(_eh/* siz8 */);});
},
_eM/* $wnextWord32 */ = function(_eN/* sb0P */){
  var _eO/* sb0Q */ = new T(function(){
    var _eP/* sb0R */ = E(_eN/* sb0P */),
    _eQ/* sb0T */ = _eP/* sb0R */.b,
    _eR/* sb0U */ = new T(function(){
      return B(_dL/* GHC.Word.$w$c+ */(_eP/* sb0R */.a, _eQ/* sb0T */));
    });
    return new T2(0,new T(function(){
      return B(_ec/* System.Random.SplitMix.mix64 */(_eR/* sb0U */));
    }),new T(function(){
      return new T2(0,E(_eR/* sb0U */),_eQ/* sb0T */);
    }));
  });
  return new T2(0,new T(function(){
    return hs_word64ToWord/* EXTERNAL */(E(E(_eO/* sb0Q */).a));
  }),new T(function(){
    return E(E(_eO/* sb0Q */).b);
  }));
},
_eS/* $fRandomGenSMGen0_$cgenWord16 */ = function(_eT/* siA0 */){
  var _eU/* siA1 */ = new T(function(){
    var _eV/* siA2 */ = B(_eM/* System.Random.SplitMix.$wnextWord32 */(_eT/* siA0 */));
    return new T2(0,_eV/* siA2 */.a,_eV/* siA2 */.b);
  });
  return new T2(0,new T(function(){
    return E(E(_eU/* siA1 */).a)&65535;
  }),new T(function(){
    return E(E(_eU/* siA1 */).b);
  }));
},
_eW/* $fExceptionArithException_ww2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("base"));
}),
_eX/* $fExceptionArithException_ww4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("GHC.Exception"));
}),
_eY/* $fExceptionArithException_ww5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("ArithException"));
}),
_eZ/* $fExceptionArithException_wild */ = new T5(0,new Long/* EXTERNAL */(4194982440, 719304104, true),new Long/* EXTERNAL */(3110813675, 1843557400, true),_eW/* GHC.Exception.$fExceptionArithException_ww2 */,_eX/* GHC.Exception.$fExceptionArithException_ww4 */,_eY/* GHC.Exception.$fExceptionArithException_ww5 */),
_f0/* $fExceptionArithException8 */ = new T5(0,new Long/* EXTERNAL */(4194982440, 719304104, true),new Long/* EXTERNAL */(3110813675, 1843557400, true),_eZ/* GHC.Exception.$fExceptionArithException_wild */,_4/* GHC.Types.[] */,_4/* GHC.Types.[] */),
_f1/* $fExceptionArithException7 */ = function(_f2/* s2VcG */){
  return E(_f0/* GHC.Exception.$fExceptionArithException8 */);
},
_f3/* $fExceptionArithException_$cfromException */ = function(_f4/* s2VcH */){
  var _f5/* s2VcI */ = E(_f4/* s2VcH */);
  return new F(function(){return _a/* Data.Typeable.cast */(B(_8/* GHC.Exception.$p1Exception */(_f5/* s2VcI */.a)), _f1/* GHC.Exception.$fExceptionArithException7 */, _f5/* s2VcI */.b);});
},
_f6/* $fExceptionArithException1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Ratio has zero denominator"));
}),
_f7/* $fExceptionArithException2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("denormal"));
}),
_f8/* $fExceptionArithException3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("divide by zero"));
}),
_f9/* $fExceptionArithException4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("loss of precision"));
}),
_fa/* $fExceptionArithException5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("arithmetic underflow"));
}),
_fb/* $fExceptionArithException6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("arithmetic overflow"));
}),
_fc/* $w$cshowsPrec */ = function(_fd/* s2VaQ */, _fe/* s2VaR */){
  switch(E(_fd/* s2VaQ */)){
    case 0:
      return new F(function(){return _q/* GHC.Base.++ */(_fb/* GHC.Exception.$fExceptionArithException6 */, _fe/* s2VaR */);});
      break;
    case 1:
      return new F(function(){return _q/* GHC.Base.++ */(_fa/* GHC.Exception.$fExceptionArithException5 */, _fe/* s2VaR */);});
      break;
    case 2:
      return new F(function(){return _q/* GHC.Base.++ */(_f9/* GHC.Exception.$fExceptionArithException4 */, _fe/* s2VaR */);});
      break;
    case 3:
      return new F(function(){return _q/* GHC.Base.++ */(_f8/* GHC.Exception.$fExceptionArithException3 */, _fe/* s2VaR */);});
      break;
    case 4:
      return new F(function(){return _q/* GHC.Base.++ */(_f7/* GHC.Exception.$fExceptionArithException2 */, _fe/* s2VaR */);});
      break;
    default:
      return new F(function(){return _q/* GHC.Base.++ */(_f6/* GHC.Exception.$fExceptionArithException1 */, _fe/* s2VaR */);});
  }
},
_ff/* $fExceptionArithException_$cshow */ = function(_fg/* s2VaY */){
  return new F(function(){return _fc/* GHC.Exception.$w$cshowsPrec */(_fg/* s2VaY */, _4/* GHC.Types.[] */);});
},
_fh/* $fExceptionArithException_$cshowsPrec */ = function(_fi/* s2VaT */, _fj/* s2VaU */, _fk/* s2VaV */){
  return new F(function(){return _fc/* GHC.Exception.$w$cshowsPrec */(_fj/* s2VaU */, _fk/* s2VaV */);});
},
_fl/* $fShowArithException_$cshowList */ = function(_fm/* s2VaW */, _fn/* s2VaX */){
  return new F(function(){return _A/* GHC.Show.showList__ */(_fc/* GHC.Exception.$w$cshowsPrec */, _fm/* s2VaW */, _fn/* s2VaX */);});
},
_fo/* $fShowArithException */ = new T3(0,_fh/* GHC.Exception.$fExceptionArithException_$cshowsPrec */,_ff/* GHC.Exception.$fExceptionArithException_$cshow */,_fl/* GHC.Exception.$fShowArithException_$cshowList */),
_fp/* $fExceptionArithException */ = new T(function(){
  return new T5(0,_f1/* GHC.Exception.$fExceptionArithException7 */,_fo/* GHC.Exception.$fShowArithException */,_fq/* GHC.Exception.$fExceptionArithException_$ctoException */,_f3/* GHC.Exception.$fExceptionArithException_$cfromException */,_ff/* GHC.Exception.$fExceptionArithException_$cshow */);
}),
_fq/* $fExceptionArithException_$ctoException */ = function(_11/* B1 */){
  return new T2(0,_fp/* GHC.Exception.$fExceptionArithException */,_11/* B1 */);
},
_fr/* DivideByZero */ = 3,
_fs/* divZeroException */ = new T(function(){
  return B(_fq/* GHC.Exception.$fExceptionArithException_$ctoException */(_fr/* GHC.Exception.DivideByZero */));
}),
_ft/* divZeroError */ = new T(function(){
  return die/* EXTERNAL */(_fs/* GHC.Exception.divZeroException */);
}),
_fu/* $w$sunbiasedWordMult32Exclusive1 */ = function(_fv/* sj9N */){
  var _fw/* sj9O */ = new T(function(){
    return hs_wordToWord64/* EXTERNAL */(E(_fv/* sj9N */));
  }),
  _fx/* sj9V */ = new T(function(){
    var _fy/* sj9Y */ = E(_fv/* sj9N */);
    if(!_fy/* sj9Y */){
      return E(_ft/* GHC.Real.divZeroError */);
    }else{
      return ( -(_fy/* sj9Y */&4294967295)>>>0)%_fy/* sj9Y */;
    }
  }),
  _fz/* sja3 */ = function(_fA/*  sja4 */){
    while(1){
      var _fB/*  sja3 */ = B((function(_fC/* sja4 */){
        var _fD/* sja5 */ = B(_eM/* System.Random.SplitMix.$wnextWord32 */(_fC/* sja4 */)),
        _fE/* sja7 */ = _fD/* sja5 */.b,
        _fF/* sjab */ = hs_wordToWord64/* EXTERNAL */(E(_fD/* sja5 */.a)),
        _fG/* sjag */ = B(_dX/* GHC.Word.$w$c* */(_fF/* sjab */, E(_fw/* sj9O */))),
        _fH/* sjai */ = hs_word64ToWord/* EXTERNAL */(_fG/* sjag */);
        if(_fH/* sjai */<E(_fx/* sj9V */)){
          _fA/*  sja4 */ = _fE/* sja7 */;
          return __continue/* EXTERNAL */;
        }else{
          return new T2(0,new T(function(){
            return hs_word64ToWord/* EXTERNAL */(B(_dS/* GHC.Word.$w$cshiftR */(_fG/* sjag */, 32)));
          }),_fE/* sja7 */);
        }
      })(_fA/*  sja4 */));
      if(_fB/*  sja3 */!=__continue/* EXTERNAL */){
        return _fB/*  sja3 */;
      }
    }
  };
  return function(_fI/* sjaw */){
    var _fJ/* sjax */ = B(_fz/* sja3 */(_fI/* sjaw */));
    return new T2(0,_fJ/* sjax */.a,_fJ/* sjax */.b);
  };
},
_fK/* nextWord32 */ = function(_fL/* sb1e */){
  var _fM/* sb1f */ = B(_eM/* System.Random.SplitMix.$wnextWord32 */(_fL/* sb1e */));
  return new T2(0,_fM/* sb1f */.a,_fM/* sb1f */.b);
},
_fN/* $w$cgenWord32R1 */ = function(_fO/* sjaA */, _fP/* sjaB */){
  var _fQ/* sjaC */ = E(_fO/* sjaA */);
  if(_fQ/* sjaC */==4294967295){
    return new F(function(){return _fK/* System.Random.SplitMix.nextWord32 */(_fP/* sjaB */);});
  }else{
    return new F(function(){return A2(_fu/* System.Random.Internal.$w$sunbiasedWordMult32Exclusive1 */,_fQ/* sjaC */+1>>>0, _fP/* sjaB */);});
  }
},
_fR/* $fRandomGenSMGen0_$cgenWord32R */ = function(_fS/* sjaF */, _fT/* sjaG */){
  return new F(function(){return _fN/* System.Random.Internal.$w$cgenWord32R1 */(E(_fS/* sjaF */), _fT/* sjaG */);});
},
_fU/* $fBitsWord64_$czeroBits */ = new T(function(){
  var _fV/* s1RAQ */ = hs_uncheckedShiftL64/* EXTERNAL */(new Long/* EXTERNAL */(1, 0, true), 0),
  _fW/* s1RAU */ = hs_not64/* EXTERNAL */(_fV/* s1RAQ */);
  return hs_and64/* EXTERNAL */(_fV/* s1RAQ */, _fW/* s1RAU */);
}),
_fX/* $w$cgenWord64R1 */ = function(_fY/* siz9 */, _fZ/* siza */, _g0/* sizb */){
  var _g1/* sizf */ = hs_not64/* EXTERNAL */(E(_fU/* GHC.Word.$fBitsWord64_$czeroBits */)),
  _g2/* sizi */ = E(_fY/* siz9 */),
  _g3/* sizl */ = hs_or64/* EXTERNAL */(_g2/* sizi */, new Long/* EXTERNAL */(1, 0, true)),
  _g4/* sizo */ = die/* EXTERNAL */("Unsupported PrimOp: clz64#")&4294967295,
  _g5/* sizq */ = function(_g6/* sizr */){
    var _g7/* sizs */ = function(_g8/* sizt */, _g9/* sizu */){
      while(1){
        var _ga/* sizv */ = B(_dL/* GHC.Word.$w$c+ */(_g8/* sizt */, _g9/* sizu */)),
        _gb/* sizy */ = hs_and64/* EXTERNAL */(B(_e4/* System.Random.SplitMix.$wmix64 */(_ga/* sizv */)), _g6/* sizr */),
        _gc/* sizC */ = hs_gtWord64/* EXTERNAL */(_gb/* sizy */, _g2/* sizi */);
        if(!_gc/* sizC */){
          return new T2(0,_gb/* sizy */,new T2(0,_ga/* sizv */,_g9/* sizu */));
        }else{
          _g8/* sizt */ = _ga/* sizv */;
          continue;
        }
      }
    };
    return new F(function(){return _g7/* sizs */(_fZ/* siza */, _g0/* sizb */);});
  };
  if(_g4/* sizo */<64){
    var _gd/* sizL */ = hs_uncheckedShiftRL64/* EXTERNAL */(_g1/* sizf */, _g4/* sizo */);
    return new F(function(){return _g5/* sizq */(_gd/* sizL */);});
  }else{
    var _ge/* sizP */ = hs_wordToWord64/* EXTERNAL */(0);
    return new F(function(){return _g5/* sizq */(_ge/* sizP */);});
  }
},
_gf/* $fRandomGenSMGen0_$cgenWord64R */ = function(_gg/* sizS */, _gh/* sizT */){
  var _gi/* sizU */ = E(_gh/* sizT */),
  _gj/* sizX */ = B(_fX/* System.Random.Internal.$w$cgenWord64R1 */(_gg/* sizS */, _gi/* sizU */.a, _gi/* sizU */.b));
  return new T2(0,_gj/* sizX */.a,_gj/* sizX */.b);
},
_gk/* $fRandomGenSMGen0_$cgenWord8 */ = function(_gl/* siAg */){
  var _gm/* siAh */ = new T(function(){
    var _gn/* siAi */ = B(_eM/* System.Random.SplitMix.$wnextWord32 */(_gl/* siAg */));
    return new T2(0,_gn/* siAi */.a,_gn/* siAi */.b);
  });
  return new T2(0,new T(function(){
    return E(E(_gm/* siAh */).a)&255;
  }),new T(function(){
    return E(E(_gm/* siAh */).b);
  }));
},
_go/* $wnextInt */ = function(_gp/* sb1V */, _gq/* sb1W */){
  var _gr/* sb1X */ = new T(function(){
    return B(_dL/* GHC.Word.$w$c+ */(_gp/* sb1V */, _gq/* sb1W */));
  });
  return new T2(0,new T(function(){
    var _gs/* sb23 */ = hs_word64ToWord/* EXTERNAL */(B(_e4/* System.Random.SplitMix.$wmix64 */(E(_gr/* sb1X */))));
    return _gs/* sb23 */&4294967295;
  }),new T(function(){
    return new T2(0,E(_gr/* sb1X */),_gq/* sb1W */);
  }));
},
_gt/* nextInt */ = function(_gu/* sb2b */){
  var _gv/* sb2c */ = E(_gu/* sb2b */),
  _gw/* sb2f */ = B(_go/* System.Random.SplitMix.$wnextInt */(_gv/* sb2c */.a, _gv/* sb2c */.b));
  return new T2(0,_gw/* sb2f */.a,_gw/* sb2f */.b);
},
_gx/* nextWord64 */ = function(_gy/* sb0F */){
  var _gz/* sb0G */ = E(_gy/* sb0F */),
  _gA/* sb0I */ = _gz/* sb0G */.b,
  _gB/* sb0J */ = new T(function(){
    return B(_dL/* GHC.Word.$w$c+ */(_gz/* sb0G */.a, _gA/* sb0I */));
  });
  return new T2(0,new T(function(){
    return B(_ec/* System.Random.SplitMix.mix64 */(_gB/* sb0J */));
  }),new T(function(){
    return new T2(0,E(_gB/* sb0J */),_gA/* sb0I */);
  }));
},
_gC/* $wmixGamma */ = function(_gD/* saYn */){
  var _gE/* saYq */ = hs_xor64/* EXTERNAL */(_gD/* saYn */, B(_dS/* GHC.Word.$w$cshiftR */(_gD/* saYn */, 30))),
  _gF/* saYt */ = B(_dX/* GHC.Word.$w$c* */(_gE/* saYq */, new Long/* EXTERNAL */(484763065, 3210233709, true))),
  _gG/* saYw */ = hs_xor64/* EXTERNAL */(_gF/* saYt */, B(_dS/* GHC.Word.$w$cshiftR */(_gF/* saYt */, 27))),
  _gH/* saYz */ = B(_dX/* GHC.Word.$w$c* */(_gG/* saYw */, new Long/* EXTERNAL */(321982955, 2496678331, true))),
  _gI/* saYC */ = hs_xor64/* EXTERNAL */(_gH/* saYz */, B(_dS/* GHC.Word.$w$cshiftR */(_gH/* saYz */, 31))),
  _gJ/* saYG */ = hs_or64/* EXTERNAL */(_gI/* saYC */, new Long/* EXTERNAL */(1, 0, true)),
  _gK/* saYL */ = hs_xor64/* EXTERNAL */(_gJ/* saYG */, B(_dS/* GHC.Word.$w$cshiftR */(_gJ/* saYG */, 1)));
  if((popCnt64/* EXTERNAL */(_gK/* saYL */)&4294967295)<24){
    var _gL/* saYT */ = hs_xor64/* EXTERNAL */(_gJ/* saYG */, new Long/* EXTERNAL */(2863311530, 2863311530, true));
    return E(_gL/* saYT */);
  }else{
    return E(_gJ/* saYG */);
  }
},
_gM/* $wsplitSMGen */ = function(_gN/* saZP */, _gO/* saZQ */){
  var _gP/* saZR */ = new T(function(){
    return B(_dL/* GHC.Word.$w$c+ */(_gN/* saZP */, _gO/* saZQ */));
  }),
  _gQ/* saZT */ = new T(function(){
    return B(_dL/* GHC.Word.$w$c+ */(E(_gP/* saZR */), _gO/* saZQ */));
  });
  return new T2(0,new T(function(){
    return new T2(0,E(_gQ/* saZT */),_gO/* saZQ */);
  }),new T(function(){
    return new T2(0,B(_e4/* System.Random.SplitMix.$wmix64 */(E(_gP/* saZR */))),B(_gC/* System.Random.SplitMix.$wmixGamma */(E(_gQ/* saZT */))));
  }));
},
_gR/* splitSMGen */ = function(_gS/* sb07 */){
  var _gT/* sb08 */ = E(_gS/* sb07 */),
  _gU/* sb0b */ = B(_gM/* System.Random.SplitMix.$wsplitSMGen */(_gT/* sb08 */.a, _gT/* sb08 */.b));
  return new T2(0,_gU/* sb0b */.a,_gU/* sb0b */.b);
},
_gV/* $fRandomGenStdGen */ = {_:0,a:_gt/* System.Random.SplitMix.nextInt */,b:_gk/* System.Random.Internal.$fRandomGenSMGen0_$cgenWord8 */,c:_eS/* System.Random.Internal.$fRandomGenSMGen0_$cgenWord16 */,d:_fK/* System.Random.SplitMix.nextWord32 */,e:_gx/* System.Random.SplitMix.nextWord64 */,f:_fR/* System.Random.Internal.$fRandomGenSMGen0_$cgenWord32R */,g:_gf/* System.Random.Internal.$fRandomGenSMGen0_$cgenWord64R */,h:_ee/* System.Random.Internal.$fRandomGenSMGen0_$cgenShortByteString */,i:_dJ/* System.Random.Internal.$fRandomGenSMGen0_$cgenRange */,j:_gR/* System.Random.SplitMix.splitSMGen */},
_gW/* genWord32 */ = function(_gX/* sif0 */){
  return E(E(_gX/* sif0 */).d);
},
_gY/* $w$crandomR12 */ = function(_gZ/* szhd */, _h0/* szhe */, _h1/* szhf */, _h2/* szhg */){
  if(_h0/* szhe */!=_h1/* szhf */){
    var _h3/* szhj */ = function(_h4/* szhk */, _h5/* szhl */){
      var _h6/* szhm */ = E(_h5/* szhl */),
      _h7/* szho */ = __clz/* EXTERNAL */(32, (_h6/* szhm */|1)>>>0)&4294967295,
      _h8/* szhr */ = function(_h9/* szhs */){
        var _ha/* szht */ = function(_hb/* szhu */){
          while(1){
            var _hc/* szhv */ = B(A2(_gW/* System.Random.Internal.genWord32 */,_gZ/* szhd */, _hb/* szhu */)),
            _hd/* szhx */ = _hc/* szhv */.b,
            _he/* szhA */ = (E(_hc/* szhv */.a)&_h9/* szhs */)>>>0;
            if(_he/* szhA */<=_h6/* szhm */){
              return new T2(0,_he/* szhA */,_hd/* szhx */);
            }else{
              _hb/* szhu */ = _hd/* szhx */;
              continue;
            }
          }
        },
        _hf/* szhE */ = B(_ha/* szht */(_h2/* szhg */));
        return new T2(0,new T(function(){
          return E(_h4/* szhk */)+(E(_hf/* szhE */.a)&4294967295)|0;
        }),_hf/* szhE */.b);
      };
      if(_h7/* szho */<32){
        return new F(function(){return _h8/* szhr */(4294967295>>>_h7/* szho */);});
      }else{
        return new F(function(){return _h8/* szhr */(0);});
      }
    };
    if(_h0/* szhe */<=_h1/* szhf */){
      return new F(function(){return _h3/* szhj */(_h0/* szhe */, (_h1/* szhf */>>>0)-(_h0/* szhe */>>>0)>>>0);});
    }else{
      return new F(function(){return _h3/* szhj */(_h1/* szhf */, (_h0/* szhe */>>>0)-(_h1/* szhf */>>>0)>>>0);});
    }
  }else{
    return new T2(0,_h1/* szhf */,_h2/* szhg */);
  }
},
_hg/* lvl4 */ = function(_hh/* stFM */){
  var _hi/* stFN */ = B(_gY/* System.Random.$w$crandomR12 */(_gV/* System.Random.Internal.$fRandomGenStdGen */, 1, 4, _hh/* stFM */));
  return new T2(0,E(_hi/* stFN */.b),_hi/* stFN */.a);
},
_hj/* lvl24 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Integer must be in the range [1, 4]"));
}),
_hk/* numToColor1 */ = new T(function(){
  return B(err/* EXTERNAL */(_hj/* LudoJS.lvl24 */));
}),
_hl/* $fEventMouseEvent2 */ = "deltaZ",
_hm/* $fEventMouseEvent3 */ = "deltaY",
_hn/* $fEventMouseEvent4 */ = "deltaX",
_ho/* lvl2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */(")"));
}),
_hp/* lvl3 */ = new T(function(){
  return B(_5V/* GHC.Show.$wshowSignedInt */(0, 2, _ho/* Haste.Events.MouseEvents.lvl2 */));
}),
_hq/* lvl4 */ = new T(function(){
  return B(unAppCStr/* EXTERNAL */(") is outside of enumeration\'s range (0,", _hp/* Haste.Events.MouseEvents.lvl3 */));
}),
_hr/* $fEnumMouseButton1 */ = function(_hs/* sr4R */){
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("toEnum{MouseButton}: tag (", new T(function(){
    return B(_5V/* GHC.Show.$wshowSignedInt */(0, _hs/* sr4R */, _hq/* Haste.Events.MouseEvents.lvl4 */));
  }))));});
},
_ht/* $fEventMouseEvent5 */ = function(_hu/* sr7q */, _/* EXTERNAL */){
  return new T(function(){
    var _hv/* sr7v */ = Number/* EXTERNAL */(E(_hu/* sr7q */)),
    _hw/* sr7z */ = jsTrunc/* EXTERNAL */(_hv/* sr7v */);
    if(_hw/* sr7z */<0){
      return B(_hr/* Haste.Events.MouseEvents.$fEnumMouseButton1 */(_hw/* sr7z */));
    }else{
      if(_hw/* sr7z */>2){
        return B(_hr/* Haste.Events.MouseEvents.$fEnumMouseButton1 */(_hw/* sr7z */));
      }else{
        return _hw/* sr7z */;
      }
    }
  });
},
_hx/* $fEventMouseEvent7 */ = 0,
_hy/* $fEventMouseEvent6 */ = new T3(0,_hx/* Haste.Events.MouseEvents.$fEventMouseEvent7 */,_hx/* Haste.Events.MouseEvents.$fEventMouseEvent7 */,_hx/* Haste.Events.MouseEvents.$fEventMouseEvent7 */),
_hz/* $fEventMouseEvent8 */ = "button",
_hA/* $fEventMouseEvent_f1 */ = new T(function(){
  return eval/* EXTERNAL */("jsGetMouseCoords");
}),
_hB/* $fFromAnyInt2 */ = function(_hC/* sc5b */, _/* EXTERNAL */){
  var _hD/* sc5d */ = E(_hC/* sc5b */);
  if(!_hD/* sc5d */._){
    return _4/* GHC.Types.[] */;
  }else{
    var _hE/* sc5g */ = B(_hB/* Haste.Prim.Any.$fFromAnyInt2 */(_hD/* sc5d */.b, _/* EXTERNAL */));
    return new T2(1,new T(function(){
      var _hF/* sc5m */ = Number/* EXTERNAL */(E(_hD/* sc5d */.a));
      return jsTrunc/* EXTERNAL */(_hF/* sc5m */);
    }),_hE/* sc5g */);
  }
},
_hG/* $wa22 */ = function(_hH/* sc5v */, _/* EXTERNAL */){
  var _hI/* sc5y */ = __arr2lst/* EXTERNAL */(0, _hH/* sc5v */);
  return new F(function(){return _hB/* Haste.Prim.Any.$fFromAnyInt2 */(_hI/* sc5y */, _/* EXTERNAL */);});
},
_hJ/* $fFromAnyInt1 */ = function(_hK/* sc5C */, _/* EXTERNAL */){
  return new F(function(){return _hG/* Haste.Prim.Any.$wa22 */(E(_hK/* sc5C */), _/* EXTERNAL */);});
},
_hL/* $fFromAnyInt3 */ = function(_hM/* sbVc */, _/* EXTERNAL */){
  return new T(function(){
    var _hN/* sbVh */ = Number/* EXTERNAL */(E(_hM/* sbVc */));
    return jsTrunc/* EXTERNAL */(_hN/* sbVh */);
  });
},
_hO/* $fFromAnyInt */ = new T2(0,_hL/* Haste.Prim.Any.$fFromAnyInt3 */,_hJ/* Haste.Prim.Any.$fFromAnyInt1 */),
_hP/* $fFromAny(,)2 */ = function(_hQ/* schQ */, _/* EXTERNAL */){
  var _hR/* schS */ = E(_hQ/* schQ */);
  if(!_hR/* schS */._){
    return _4/* GHC.Types.[] */;
  }else{
    var _hS/* schV */ = B(_hP/* Haste.Prim.Any.$fFromAny(,)2 */(_hR/* schS */.b, _/* EXTERNAL */));
    return new T2(1,_hR/* schS */.a,_hS/* schV */);
  }
},
_hT/* $fExceptionAllocationLimitExceeded_ww2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("base"));
}),
_hU/* $fExceptionAllocationLimitExceeded_ww4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("GHC.IO.Exception"));
}),
_hV/* $fExceptionIOException_ww4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("IOException"));
}),
_hW/* $fExceptionIOException_wild */ = new T5(0,new Long/* EXTERNAL */(4053623282, 1685460941, true),new Long/* EXTERNAL */(3693590983, 2507416641, true),_hT/* GHC.IO.Exception.$fExceptionAllocationLimitExceeded_ww2 */,_hU/* GHC.IO.Exception.$fExceptionAllocationLimitExceeded_ww4 */,_hV/* GHC.IO.Exception.$fExceptionIOException_ww4 */),
_hX/* $fExceptionIOException4 */ = new T5(0,new Long/* EXTERNAL */(4053623282, 1685460941, true),new Long/* EXTERNAL */(3693590983, 2507416641, true),_hW/* GHC.IO.Exception.$fExceptionIOException_wild */,_4/* GHC.Types.[] */,_4/* GHC.Types.[] */),
_hY/* $fExceptionIOException3 */ = function(_hZ/* s3MW8 */){
  return E(_hX/* GHC.IO.Exception.$fExceptionIOException4 */);
},
_i0/* $fExceptionIOException_$cfromException */ = function(_i1/* s3N1e */){
  var _i2/* s3N1f */ = E(_i1/* s3N1e */);
  return new F(function(){return _a/* Data.Typeable.cast */(B(_8/* GHC.Exception.$p1Exception */(_i2/* s3N1f */.a)), _hY/* GHC.IO.Exception.$fExceptionIOException3 */, _i2/* s3N1f */.b);});
},
_i3/* $fExceptionArrayException2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */(": "));
}),
_i4/* $fExceptionIOException1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */(")"));
}),
_i5/* $fExceptionIOException2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */(" ("));
}),
_i6/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */("interrupted"));
}),
_i7/* lvl1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("resource vanished"));
}),
_i8/* lvl10 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("unsatisified constraints"));
}),
_i9/* lvl11 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("user error"));
}),
_ia/* lvl12 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("permission denied"));
}),
_ib/* lvl13 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("illegal operation"));
}),
_ic/* lvl14 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("end of file"));
}),
_id/* lvl15 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("resource exhausted"));
}),
_ie/* lvl16 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("resource busy"));
}),
_if/* lvl17 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("does not exist"));
}),
_ig/* lvl18 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("already exists"));
}),
_ih/* lvl2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("timeout"));
}),
_ii/* lvl3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("unsupported operation"));
}),
_ij/* lvl4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("hardware fault"));
}),
_ik/* lvl5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("inappropriate type"));
}),
_il/* lvl6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("invalid argument"));
}),
_im/* lvl7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("failed"));
}),
_in/* lvl8 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("protocol error"));
}),
_io/* lvl9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("system error"));
}),
_ip/* $w$cshowsPrec3 */ = function(_iq/* s3N03 */, _ir/* s3N04 */){
  switch(E(_iq/* s3N03 */)){
    case 0:
      return new F(function(){return _q/* GHC.Base.++ */(_ig/* GHC.IO.Exception.lvl18 */, _ir/* s3N04 */);});
      break;
    case 1:
      return new F(function(){return _q/* GHC.Base.++ */(_if/* GHC.IO.Exception.lvl17 */, _ir/* s3N04 */);});
      break;
    case 2:
      return new F(function(){return _q/* GHC.Base.++ */(_ie/* GHC.IO.Exception.lvl16 */, _ir/* s3N04 */);});
      break;
    case 3:
      return new F(function(){return _q/* GHC.Base.++ */(_id/* GHC.IO.Exception.lvl15 */, _ir/* s3N04 */);});
      break;
    case 4:
      return new F(function(){return _q/* GHC.Base.++ */(_ic/* GHC.IO.Exception.lvl14 */, _ir/* s3N04 */);});
      break;
    case 5:
      return new F(function(){return _q/* GHC.Base.++ */(_ib/* GHC.IO.Exception.lvl13 */, _ir/* s3N04 */);});
      break;
    case 6:
      return new F(function(){return _q/* GHC.Base.++ */(_ia/* GHC.IO.Exception.lvl12 */, _ir/* s3N04 */);});
      break;
    case 7:
      return new F(function(){return _q/* GHC.Base.++ */(_i9/* GHC.IO.Exception.lvl11 */, _ir/* s3N04 */);});
      break;
    case 8:
      return new F(function(){return _q/* GHC.Base.++ */(_i8/* GHC.IO.Exception.lvl10 */, _ir/* s3N04 */);});
      break;
    case 9:
      return new F(function(){return _q/* GHC.Base.++ */(_io/* GHC.IO.Exception.lvl9 */, _ir/* s3N04 */);});
      break;
    case 10:
      return new F(function(){return _q/* GHC.Base.++ */(_in/* GHC.IO.Exception.lvl8 */, _ir/* s3N04 */);});
      break;
    case 11:
      return new F(function(){return _q/* GHC.Base.++ */(_im/* GHC.IO.Exception.lvl7 */, _ir/* s3N04 */);});
      break;
    case 12:
      return new F(function(){return _q/* GHC.Base.++ */(_il/* GHC.IO.Exception.lvl6 */, _ir/* s3N04 */);});
      break;
    case 13:
      return new F(function(){return _q/* GHC.Base.++ */(_ik/* GHC.IO.Exception.lvl5 */, _ir/* s3N04 */);});
      break;
    case 14:
      return new F(function(){return _q/* GHC.Base.++ */(_ij/* GHC.IO.Exception.lvl4 */, _ir/* s3N04 */);});
      break;
    case 15:
      return new F(function(){return _q/* GHC.Base.++ */(_ii/* GHC.IO.Exception.lvl3 */, _ir/* s3N04 */);});
      break;
    case 16:
      return new F(function(){return _q/* GHC.Base.++ */(_ih/* GHC.IO.Exception.lvl2 */, _ir/* s3N04 */);});
      break;
    case 17:
      return new F(function(){return _q/* GHC.Base.++ */(_i7/* GHC.IO.Exception.lvl1 */, _ir/* s3N04 */);});
      break;
    default:
      return new F(function(){return _q/* GHC.Base.++ */(_i6/* GHC.IO.Exception.lvl */, _ir/* s3N04 */);});
  }
},
_is/* showHandle1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("}"));
}),
_it/* showHandle2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("{handle: "));
}),
_iu/* $w$cshowsPrec2 */ = function(_iv/* s3N0c */, _iw/* s3N0d */, _ix/* s3N0e */, _iy/* s3N0f */, _iz/* s3N0g */, _iA/* s3N0h */){
  var _iB/* s3N0i */ = new T(function(){
    var _iC/* s3N0j */ = new T(function(){
      var _iD/* s3N0p */ = new T(function(){
        var _iE/* s3N0k */ = E(_iy/* s3N0f */);
        if(!_iE/* s3N0k */._){
          return E(_iA/* s3N0h */);
        }else{
          var _iF/* s3N0o */ = new T(function(){
            return B(_q/* GHC.Base.++ */(_iE/* s3N0k */, new T(function(){
              return B(_q/* GHC.Base.++ */(_i4/* GHC.IO.Exception.$fExceptionIOException1 */, _iA/* s3N0h */));
            },1)));
          },1);
          return B(_q/* GHC.Base.++ */(_i5/* GHC.IO.Exception.$fExceptionIOException2 */, _iF/* s3N0o */));
        }
      },1);
      return B(_ip/* GHC.IO.Exception.$w$cshowsPrec3 */(_iw/* s3N0d */, _iD/* s3N0p */));
    }),
    _iG/* s3N0q */ = E(_ix/* s3N0e */);
    if(!_iG/* s3N0q */._){
      return E(_iC/* s3N0j */);
    }else{
      return B(_q/* GHC.Base.++ */(_iG/* s3N0q */, new T(function(){
        return B(_q/* GHC.Base.++ */(_i3/* GHC.IO.Exception.$fExceptionArrayException2 */, _iC/* s3N0j */));
      },1)));
    }
  }),
  _iH/* s3N0u */ = E(_iz/* s3N0g */);
  if(!_iH/* s3N0u */._){
    var _iI/* s3N0v */ = E(_iv/* s3N0c */);
    if(!_iI/* s3N0v */._){
      return E(_iB/* s3N0i */);
    }else{
      var _iJ/* s3N0x */ = E(_iI/* s3N0v */.a);
      if(!_iJ/* s3N0x */._){
        var _iK/* s3N0C */ = new T(function(){
          var _iL/* s3N0B */ = new T(function(){
            return B(_q/* GHC.Base.++ */(_is/* GHC.IO.Handle.Types.showHandle1 */, new T(function(){
              return B(_q/* GHC.Base.++ */(_i3/* GHC.IO.Exception.$fExceptionArrayException2 */, _iB/* s3N0i */));
            },1)));
          },1);
          return B(_q/* GHC.Base.++ */(_iJ/* s3N0x */.a, _iL/* s3N0B */));
        },1);
        return new F(function(){return _q/* GHC.Base.++ */(_it/* GHC.IO.Handle.Types.showHandle2 */, _iK/* s3N0C */);});
      }else{
        var _iM/* s3N0I */ = new T(function(){
          var _iN/* s3N0H */ = new T(function(){
            return B(_q/* GHC.Base.++ */(_is/* GHC.IO.Handle.Types.showHandle1 */, new T(function(){
              return B(_q/* GHC.Base.++ */(_i3/* GHC.IO.Exception.$fExceptionArrayException2 */, _iB/* s3N0i */));
            },1)));
          },1);
          return B(_q/* GHC.Base.++ */(_iJ/* s3N0x */.a, _iN/* s3N0H */));
        },1);
        return new F(function(){return _q/* GHC.Base.++ */(_it/* GHC.IO.Handle.Types.showHandle2 */, _iM/* s3N0I */);});
      }
    }
  }else{
    return new F(function(){return _q/* GHC.Base.++ */(_iH/* s3N0u */.a, new T(function(){
      return B(_q/* GHC.Base.++ */(_i3/* GHC.IO.Exception.$fExceptionArrayException2 */, _iB/* s3N0i */));
    },1));});
  }
},
_iO/* $fExceptionIOException_$cshow */ = function(_iP/* s3N16 */){
  var _iQ/* s3N17 */ = E(_iP/* s3N16 */);
  return new F(function(){return _iu/* GHC.IO.Exception.$w$cshowsPrec2 */(_iQ/* s3N17 */.a, _iQ/* s3N17 */.b, _iQ/* s3N17 */.c, _iQ/* s3N17 */.d, _iQ/* s3N17 */.f, _4/* GHC.Types.[] */);});
},
_iR/* $fExceptionIOException_$cshowsPrec */ = function(_iS/* s3N0L */, _iT/* s3N0M */, _iU/* s3N0N */){
  var _iV/* s3N0O */ = E(_iT/* s3N0M */);
  return new F(function(){return _iu/* GHC.IO.Exception.$w$cshowsPrec2 */(_iV/* s3N0O */.a, _iV/* s3N0O */.b, _iV/* s3N0O */.c, _iV/* s3N0O */.d, _iV/* s3N0O */.f, _iU/* s3N0N */);});
},
_iW/* $s$dmshow9 */ = function(_iX/* s3N0V */, _iY/* s3N0W */){
  var _iZ/* s3N0X */ = E(_iX/* s3N0V */);
  return new F(function(){return _iu/* GHC.IO.Exception.$w$cshowsPrec2 */(_iZ/* s3N0X */.a, _iZ/* s3N0X */.b, _iZ/* s3N0X */.c, _iZ/* s3N0X */.d, _iZ/* s3N0X */.f, _iY/* s3N0W */);});
},
_j0/* $fShowIOException_$cshowList */ = function(_j1/* s3N14 */, _j2/* s3N15 */){
  return new F(function(){return _A/* GHC.Show.showList__ */(_iW/* GHC.IO.Exception.$s$dmshow9 */, _j1/* s3N14 */, _j2/* s3N15 */);});
},
_j3/* $fShowIOException */ = new T3(0,_iR/* GHC.IO.Exception.$fExceptionIOException_$cshowsPrec */,_iO/* GHC.IO.Exception.$fExceptionIOException_$cshow */,_j0/* GHC.IO.Exception.$fShowIOException_$cshowList */),
_j4/* $fExceptionIOException */ = new T(function(){
  return new T5(0,_hY/* GHC.IO.Exception.$fExceptionIOException3 */,_j3/* GHC.IO.Exception.$fShowIOException */,_j5/* GHC.IO.Exception.$fExceptionIOException_$ctoException */,_i0/* GHC.IO.Exception.$fExceptionIOException_$cfromException */,_iO/* GHC.IO.Exception.$fExceptionIOException_$cshow */);
}),
_j5/* $fExceptionIOException_$ctoException */ = function(_j6/* B1 */){
  return new T2(0,_j4/* GHC.IO.Exception.$fExceptionIOException */,_j6/* B1 */);
},
_j7/* Nothing */ = __Z/* EXTERNAL */,
_j8/* UserError */ = 7,
_j9/* lvl25 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Pattern match failure in do expression at src\\Haste\\Prim\\Any.hs:268:5-9"));
}),
_ja/* lvl26 */ = new T6(0,_j7/* GHC.Base.Nothing */,_j8/* GHC.IO.Exception.UserError */,_4/* GHC.Types.[] */,_j9/* Haste.Prim.Any.lvl25 */,_j7/* GHC.Base.Nothing */,_j7/* GHC.Base.Nothing */),
_jb/* lvl27 */ = new T(function(){
  return B(_j5/* GHC.IO.Exception.$fExceptionIOException_$ctoException */(_ja/* Haste.Prim.Any.lvl26 */));
}),
_jc/* $wa3 */ = function(_/* EXTERNAL */){
  return new F(function(){return die/* EXTERNAL */(_jb/* Haste.Prim.Any.lvl27 */);});
},
_jd/* fromAny */ = function(_je/* sbDR */){
  return E(E(_je/* sbDR */).a);
},
_jf/* $wa2 */ = function(_jg/* schZ */, _jh/* sci0 */, _ji/* sci1 */, _/* EXTERNAL */){
  var _jj/* sci4 */ = __arr2lst/* EXTERNAL */(0, _ji/* sci1 */),
  _jk/* sci8 */ = B(_hP/* Haste.Prim.Any.$fFromAny(,)2 */(_jj/* sci4 */, _/* EXTERNAL */)),
  _jl/* scib */ = E(_jk/* sci8 */);
  if(!_jl/* scib */._){
    return new F(function(){return _jc/* Haste.Prim.Any.$wa3 */(_/* EXTERNAL */);});
  }else{
    var _jm/* scie */ = E(_jl/* scib */.b);
    if(!_jm/* scie */._){
      return new F(function(){return _jc/* Haste.Prim.Any.$wa3 */(_/* EXTERNAL */);});
    }else{
      if(!E(_jm/* scie */.b)._){
        var _jn/* scii */ = B(A3(_jd/* Haste.Prim.Any.fromAny */,_jg/* schZ */, _jl/* scib */.a, _/* EXTERNAL */)),
        _jo/* scil */ = B(A3(_jd/* Haste.Prim.Any.fromAny */,_jh/* sci0 */, _jm/* scie */.a, _/* EXTERNAL */));
        return new T2(0,_jn/* scii */,_jo/* scil */);
      }else{
        return new F(function(){return _jc/* Haste.Prim.Any.$wa3 */(_/* EXTERNAL */);});
      }
    }
  }
},
_jp/* $wa */ = function(_jq/* sr8b */, _jr/* sr8c */, _/* EXTERNAL */){
  if(E(_jq/* sr8b */)==7){
    var _js/* sr90 */ = __app1/* EXTERNAL */(E(_hA/* Haste.Events.MouseEvents.$fEventMouseEvent_f1 */), _jr/* sr8c */),
    _jt/* sr93 */ = B(_jf/* Haste.Prim.Any.$wa2 */(_hO/* Haste.Prim.Any.$fFromAnyInt */, _hO/* Haste.Prim.Any.$fFromAnyInt */, _js/* sr90 */, _/* EXTERNAL */)),
    _ju/* sr99 */ = __get/* EXTERNAL */(_jr/* sr8c */, E(_hn/* Haste.Events.MouseEvents.$fEventMouseEvent4 */)),
    _jv/* sr9f */ = __get/* EXTERNAL */(_jr/* sr8c */, E(_hm/* Haste.Events.MouseEvents.$fEventMouseEvent3 */)),
    _jw/* sr9l */ = __get/* EXTERNAL */(_jr/* sr8c */, E(_hl/* Haste.Events.MouseEvents.$fEventMouseEvent2 */));
    return new T(function(){
      return new T3(0,E(_jt/* sr93 */),E(_j7/* GHC.Base.Nothing */),E(new T3(0,_ju/* sr99 */,_jv/* sr9f */,_jw/* sr9l */)));
    });
  }else{
    var _jx/* sr8i */ = __app1/* EXTERNAL */(E(_hA/* Haste.Events.MouseEvents.$fEventMouseEvent_f1 */), _jr/* sr8c */),
    _jy/* sr8l */ = B(_jf/* Haste.Prim.Any.$wa2 */(_hO/* Haste.Prim.Any.$fFromAnyInt */, _hO/* Haste.Prim.Any.$fFromAnyInt */, _jx/* sr8i */, _/* EXTERNAL */)),
    _jz/* sr8r */ = __get/* EXTERNAL */(_jr/* sr8c */, E(_hz/* Haste.Events.MouseEvents.$fEventMouseEvent8 */)),
    _jA/* sr8x */ = __eq/* EXTERNAL */(_jz/* sr8r */, E(_dF/* Haste.Prim.Any.jsNull */));
    if(!E(_jA/* sr8x */)){
      var _jB/* sr8G */ = __isUndef/* EXTERNAL */(_jz/* sr8r */);
      if(!E(_jB/* sr8G */)){
        var _jC/* sr8P */ = B(_ht/* Haste.Events.MouseEvents.$fEventMouseEvent5 */(_jz/* sr8r */, _/* EXTERNAL */));
        return new T(function(){
          return new T3(0,E(_jy/* sr8l */),E(new T1(1,_jC/* sr8P */)),E(_hy/* Haste.Events.MouseEvents.$fEventMouseEvent6 */));
        });
      }else{
        return new T(function(){
          return new T3(0,E(_jy/* sr8l */),E(_j7/* GHC.Base.Nothing */),E(_hy/* Haste.Events.MouseEvents.$fEventMouseEvent6 */));
        });
      }
    }else{
      return new T(function(){
        return new T3(0,E(_jy/* sr8l */),E(_j7/* GHC.Base.Nothing */),E(_hy/* Haste.Events.MouseEvents.$fEventMouseEvent6 */));
      });
    }
  }
},
_jD/* $fEventMouseEvent1 */ = function(_jE/* sr9w */, _jF/* sr9x */, _/* EXTERNAL */){
  return new F(function(){return _jp/* Haste.Events.MouseEvents.$wa */(_jE/* sr9w */, E(_jF/* sr9x */), _/* EXTERNAL */);});
},
_jG/* $fEventMouseEvent10 */ = "mouseout",
_jH/* $fEventMouseEvent11 */ = "mouseover",
_jI/* $fEventMouseEvent12 */ = "mousemove",
_jJ/* $fEventMouseEvent13 */ = "mouseup",
_jK/* $fEventMouseEvent14 */ = "mousedown",
_jL/* $fEventMouseEvent15 */ = "dblclick",
_jM/* $fEventMouseEvent16 */ = "click",
_jN/* $fEventMouseEvent9 */ = "wheel",
_jO/* $fEventMouseEvent_$ceventName */ = function(_jP/* sr5I */){
  switch(E(_jP/* sr5I */)){
    case 0:
      return E(_jM/* Haste.Events.MouseEvents.$fEventMouseEvent16 */);
    case 1:
      return E(_jL/* Haste.Events.MouseEvents.$fEventMouseEvent15 */);
    case 2:
      return E(_jK/* Haste.Events.MouseEvents.$fEventMouseEvent14 */);
    case 3:
      return E(_jJ/* Haste.Events.MouseEvents.$fEventMouseEvent13 */);
    case 4:
      return E(_jI/* Haste.Events.MouseEvents.$fEventMouseEvent12 */);
    case 5:
      return E(_jH/* Haste.Events.MouseEvents.$fEventMouseEvent11 */);
    case 6:
      return E(_jG/* Haste.Events.MouseEvents.$fEventMouseEvent10 */);
    default:
      return E(_jN/* Haste.Events.MouseEvents.$fEventMouseEvent9 */);
  }
},
_jQ/* $fEventMouseEvent */ = new T2(0,_jO/* Haste.Events.MouseEvents.$fEventMouseEvent_$ceventName */,_jD/* Haste.Events.MouseEvents.$fEventMouseEvent1 */),
_jR/* $fEventSourceElem1 */ = function(_jS/* smB0 */){
  return E(_jS/* smB0 */);
},
_jT/* $fApplicativeIO1 */ = function(_jU/* s3he */, _jV/* s3hf */, _/* EXTERNAL */){
  var _jW/* s3hh */ = B(A1(_jU/* s3he */,_/* EXTERNAL */)),
  _jX/* s3hk */ = B(A1(_jV/* s3hf */,_/* EXTERNAL */));
  return _jW/* s3hh */;
},
_jY/* $fApplicativeIO2 */ = function(_jZ/* s3gs */, _k0/* s3gt */, _/* EXTERNAL */){
  var _k1/* s3gv */ = B(A1(_jZ/* s3gs */,_/* EXTERNAL */)),
  _k2/* s3gy */ = B(A1(_k0/* s3gt */,_/* EXTERNAL */));
  return new T(function(){
    return B(A1(_k1/* s3gv */,_k2/* s3gy */));
  });
},
_k3/* $fFunctorIO1 */ = function(_k4/* s3gX */, _k5/* s3gY */, _/* EXTERNAL */){
  var _k6/* s3h0 */ = B(A1(_k5/* s3gY */,_/* EXTERNAL */));
  return _k4/* s3gX */;
},
_k7/* $fFunctorIO2 */ = function(_k8/* s3gQ */, _k9/* s3gR */, _/* EXTERNAL */){
  var _ka/* s3gT */ = B(A1(_k9/* s3gR */,_/* EXTERNAL */));
  return new T(function(){
    return B(A1(_k8/* s3gQ */,_ka/* s3gT */));
  });
},
_kb/* $fFunctorIO */ = new T2(0,_k7/* GHC.Base.$fFunctorIO2 */,_k3/* GHC.Base.$fFunctorIO1 */),
_kc/* returnIO1 */ = function(_kd/* s3g5 */, _/* EXTERNAL */){
  return _kd/* s3g5 */;
},
_ke/* thenIO1 */ = function(_kf/* s3fZ */, _kg/* s3g0 */, _/* EXTERNAL */){
  var _kh/* s3g2 */ = B(A1(_kf/* s3fZ */,_/* EXTERNAL */));
  return new F(function(){return A1(_kg/* s3g0 */,_/* EXTERNAL */);});
},
_ki/* $fApplicativeIO */ = new T5(0,_kb/* GHC.Base.$fFunctorIO */,_kc/* GHC.Base.returnIO1 */,_jY/* GHC.Base.$fApplicativeIO2 */,_ke/* GHC.Base.thenIO1 */,_jT/* GHC.Base.$fApplicativeIO1 */),
_kj/* $fxExceptionIOException */ = new T(function(){
  return E(_j4/* GHC.IO.Exception.$fExceptionIOException */);
}),
_kk/* userError */ = function(_kl/* s3N8k */){
  return new T6(0,_j7/* GHC.Base.Nothing */,_j8/* GHC.IO.Exception.UserError */,_4/* GHC.Types.[] */,_kl/* s3N8k */,_j7/* GHC.Base.Nothing */,_j7/* GHC.Base.Nothing */);
},
_km/* failIO1 */ = function(_kn/* s2YSE */, _/* EXTERNAL */){
  var _ko/* s2YSH */ = new T(function(){
    return B(A2(_U/* GHC.Exception.toException */,_kj/* GHC.IO.Exception.$fxExceptionIOException */, new T(function(){
      return B(A1(_kk/* GHC.IO.Exception.userError */,_kn/* s2YSE */));
    })));
  });
  return new F(function(){return die/* EXTERNAL */(_ko/* s2YSH */);});
},
_kp/* failIO */ = function(_kq/* B2 */, _/* EXTERNAL */){
  return new F(function(){return _km/* GHC.IO.failIO1 */(_kq/* B2 */, _/* EXTERNAL */);});
},
_kr/* $fMonadIO_$cfail */ = function(_ks/* s3ei */){
  return new F(function(){return A1(_kp/* GHC.IO.failIO */,_ks/* s3ei */);});
},
_kt/* bindIO1 */ = function(_ku/* s3g7 */, _kv/* s3g8 */, _/* EXTERNAL */){
  var _kw/* s3ga */ = B(A1(_ku/* s3g7 */,_/* EXTERNAL */));
  return new F(function(){return A2(_kv/* s3g8 */,_kw/* s3ga */, _/* EXTERNAL */);});
},
_kx/* $fMonadIO */ = new T5(0,_ki/* GHC.Base.$fApplicativeIO */,_kt/* GHC.Base.bindIO1 */,_ke/* GHC.Base.thenIO1 */,_kc/* GHC.Base.returnIO1 */,_kr/* GHC.Base.$fMonadIO_$cfail */),
_ky/* id */ = function(_kz/* s3aG */){
  return E(_kz/* s3aG */);
},
_kA/* $fMonadIOIO */ = new T2(0,_kx/* GHC.Base.$fMonadIO */,_ky/* GHC.Base.id */),
_kB/* $fMonadEventIO */ = new T2(0,_kA/* Control.Monad.IO.Class.$fMonadIOIO */,_kc/* GHC.Base.returnIO1 */),
_kC/* $w$ctoAny1 */ = function(_kD/* siuJ */){
  switch(E(_kD/* siuJ */)){
    case 0:
      return E(toJSStr/* EXTERNAL */(E(_1u/* LudoJS.$fFromAnyGameState14 */)));
    case 1:
      return E(toJSStr/* EXTERNAL */(E(_1t/* LudoJS.$fFromAnyGameState13 */)));
    case 2:
      return E(toJSStr/* EXTERNAL */(E(_1s/* LudoJS.$fFromAnyGameState12 */)));
    default:
      return E(toJSStr/* EXTERNAL */(E(_1r/* LudoJS.$fFromAnyGameState11 */)));
  }
},
_kE/* $fToAnyGameState_$ctoAny1 */ = function(_kF/* siv5 */){
  return new F(function(){return _kC/* LudoJS.$w$ctoAny1 */(_kF/* siv5 */);});
},
_kG/* $fToAnyGameState4 */ = "GameFinished",
_kH/* $fToAnyGameState5 */ = "stage",
_kI/* $fToAnyGameState3 */ = new T2(0,_kH/* LudoJS.$fToAnyGameState5 */,_kG/* LudoJS.$fToAnyGameState4 */),
_kJ/* $fToAnyGameState2 */ = new T2(1,_kI/* LudoJS.$fToAnyGameState3 */,_4/* GHC.Types.[] */),
_kK/* $wtoObject */ = function(_kL/* sbWw */){
  var _kM/* sbWy */ = __new/* EXTERNAL */(),
  _kN/* sbWA */ = _kM/* sbWy */,
  _kO/* sbWB */ = function(_kP/* sbWC */, _/* EXTERNAL */){
    while(1){
      var _kQ/* sbWE */ = E(_kP/* sbWC */);
      if(!_kQ/* sbWE */._){
        return _eb/* GHC.Tuple.() */;
      }else{
        var _kR/* sbWH */ = E(_kQ/* sbWE */.a),
        _kS/* sbWP */ = __set/* EXTERNAL */(_kN/* sbWA */, E(_kR/* sbWH */.a), E(_kR/* sbWH */.b));
        _kP/* sbWC */ = _kQ/* sbWE */.b;
        continue;
      }
    }
  },
  _kT/* sbWR */ = B(_kO/* sbWB */(_kL/* sbWw */, _/* EXTERNAL */));
  return E(_kN/* sbWA */);
},
_kU/* $fToAnyGameState1 */ = new T(function(){
  return B(_kK/* Haste.Prim.Any.$wtoObject */(_kJ/* LudoJS.$fToAnyGameState2 */));
}),
_kV/* $fToAnyGameState13 */ = "Roll",
_kW/* $fToAnyGameState12 */ = new T2(0,_1F/* LudoJS.$fFromAnyGameState16 */,_kV/* LudoJS.$fToAnyGameState13 */),
_kX/* $fToAnyGameState18 */ =  -1,
_kY/* $fToAnyGameState19 */ = "rollNumber",
_kZ/* $fToAnyGameState17 */ = new T2(0,_kY/* LudoJS.$fToAnyGameState19 */,_kX/* LudoJS.$fToAnyGameState18 */),
_l0/* $fToAnyGameState16 */ = new T2(1,_kZ/* LudoJS.$fToAnyGameState17 */,_4/* GHC.Types.[] */),
_l1/* $fToAnyGameState21 */ = "stage",
_l2/* $fToAnyGameState20 */ = new T2(0,_l1/* LudoJS.$fToAnyGameState21 */,_kV/* LudoJS.$fToAnyGameState13 */),
_l3/* $fToAnyGameState15 */ = new T2(1,_l2/* LudoJS.$fToAnyGameState20 */,_l0/* LudoJS.$fToAnyGameState16 */),
_l4/* $fToAnyGameState14 */ = new T(function(){
  return B(_kK/* Haste.Prim.Any.$wtoObject */(_l3/* LudoJS.$fToAnyGameState15 */));
}),
_l5/* $fToAnyGameState8 */ = "SelectField",
_l6/* $fToAnyGameState7 */ = new T2(0,_1F/* LudoJS.$fFromAnyGameState16 */,_l5/* LudoJS.$fToAnyGameState8 */),
_l7/* $fToAnyGameState10 */ = "SelectPiece",
_l8/* $fToAnyGameState9 */ = new T2(0,_1F/* LudoJS.$fFromAnyGameState16 */,_l7/* LudoJS.$fToAnyGameState10 */),
_l9/* $fToAnyGameState_$ctoAny2 */ = function(_la/* si54 */){
  var _lb/* si55 */ = E(_la/* si54 */);
  switch(_lb/* si55 */._){
    case 0:
      var _lc/* si57 */ = E(_lb/* si55 */.a);
      if(!_lc/* si57 */._){
        return E(_l4/* LudoJS.$fToAnyGameState14 */);
      }else{
        return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,_kW/* LudoJS.$fToAnyGameState12 */,new T2(1,new T2(0,_cu/* LudoJS.$fToAnyGameState11 */,_lc/* si57 */.a),_4/* GHC.Types.[] */)));});
      }
      break;
    case 1:
      return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,_l8/* LudoJS.$fToAnyGameState9 */,new T2(1,new T2(0,_cu/* LudoJS.$fToAnyGameState11 */,_lb/* si55 */.a),_4/* GHC.Types.[] */)));});
      break;
    case 2:
      return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,_l6/* LudoJS.$fToAnyGameState7 */,new T2(1,new T2(0,_cu/* LudoJS.$fToAnyGameState11 */,_lb/* si55 */.a),new T2(1,new T2(0,_cv/* LudoJS.$fToAnyGameState6 */,_lb/* si55 */.b),_4/* GHC.Types.[] */))));});
      break;
    default:
      return E(_kU/* LudoJS.$fToAnyGameState1 */);
  }
},
_ld/* $fToAnyPiece2 */ = "Active",
_le/* $fToAnyPiece1 */ = new T2(0,_1N/* LudoJS.$fToAnyOption5 */,_ld/* LudoJS.$fToAnyPiece2 */),
_lf/* $fToAnyPiece6 */ = "Out",
_lg/* $fToAnyPiece7 */ = "piece",
_lh/* $fToAnyPiece5 */ = new T2(0,_lg/* LudoJS.$fToAnyPiece7 */,_lf/* LudoJS.$fToAnyPiece6 */),
_li/* $fToAnyPiece4 */ = new T2(1,_lh/* LudoJS.$fToAnyPiece5 */,_4/* GHC.Types.[] */),
_lj/* $fToAnyPiece3 */ = new T(function(){
  return B(_kK/* Haste.Prim.Any.$wtoObject */(_li/* LudoJS.$fToAnyPiece4 */));
}),
_lk/* $fToAnyPiece_$ctoAny */ = function(_ll/* si4Q */){
  var _lm/* si4R */ = E(_ll/* si4Q */);
  if(!_lm/* si4R */._){
    return E(_lj/* LudoJS.$fToAnyPiece3 */);
  }else{
    return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,_le/* LudoJS.$fToAnyPiece1 */,new T2(1,new T2(0,_1M/* LudoJS.$fToAnyOption1 */,_lm/* si4R */.a),_4/* GHC.Types.[] */)));});
  }
},
_ln/* go1 */ = function(_lo/* sive */){
  var _lp/* sivf */ = E(_lo/* sive */);
  if(!_lp/* sivf */._){
    return __Z/* EXTERNAL */;
  }else{
    var _lq/* sivi */ = E(_lp/* sivf */.a);
    return new T2(1,new T2(0,new T(function(){
      return toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, E(_lq/* sivi */.a), _4/* GHC.Types.[] */)));
    }),new T(function(){
      return B(_lk/* LudoJS.$fToAnyPiece_$ctoAny */(_lq/* sivi */.b));
    })),new T(function(){
      return B(_ln/* LudoJS.go1 */(_lp/* sivf */.b));
    }));
  }
},
_lr/* lvl37 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_1r/* LudoJS.$fFromAnyGameState11 */, _4/* GHC.Types.[] */));
}),
_ls/* lvl38 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_1s/* LudoJS.$fFromAnyGameState12 */, _4/* GHC.Types.[] */));
}),
_lt/* lvl39 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_1t/* LudoJS.$fFromAnyGameState13 */, _4/* GHC.Types.[] */));
}),
_lu/* lvl40 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_1u/* LudoJS.$fFromAnyGameState14 */, _4/* GHC.Types.[] */));
}),
_lv/* $fToAnyGameState_go10 */ = function(_lw/*  sivu */, _lx/*  sivv */){
  while(1){
    var _ly/*  $fToAnyGameState_go10 */ = B((function(_lz/* sivu */, _lA/* sivv */){
      var _lB/* sivw */ = E(_lA/* sivv */);
      if(!_lB/* sivw */._){
        var _lC/* sivS */ = new T(function(){
          return B(_kK/* Haste.Prim.Any.$wtoObject */(new T(function(){
            return B(_ln/* LudoJS.go1 */(_lB/* sivw */.c));
          },1)));
        });
        _lw/*  sivu */ = new T2(1,new T2(0,new T(function(){
          switch(E(_lB/* sivw */.b)){
            case 0:
              return toJSStr/* EXTERNAL */(E(_lu/* LudoJS.lvl40 */));
              break;
            case 1:
              return toJSStr/* EXTERNAL */(E(_lt/* LudoJS.lvl39 */));
              break;
            case 2:
              return toJSStr/* EXTERNAL */(E(_ls/* LudoJS.lvl38 */));
              break;
            default:
              return toJSStr/* EXTERNAL */(E(_lr/* LudoJS.lvl37 */));
          }
        }),_lC/* sivS */),new T(function(){
          return B(_lv/* LudoJS.$fToAnyGameState_go10 */(_lz/* sivu */, _lB/* sivw */.e));
        }));
        _lx/*  sivv */ = _lB/* sivw */.d;
        return __continue/* EXTERNAL */;
      }else{
        return E(_lz/* sivu */);
      }
    })(_lw/*  sivu */, _lx/*  sivv */));
    if(_ly/*  $fToAnyGameState_go10 */!=__continue/* EXTERNAL */){
      return _ly/*  $fToAnyGameState_go10 */;
    }
  }
},
_lD/* map */ = function(_lE/* s3hr */, _lF/* s3hs */){
  var _lG/* s3ht */ = E(_lF/* s3hs */);
  return (_lG/* s3ht */._==0) ? __Z/* EXTERNAL */ : new T2(1,new T(function(){
    return B(A1(_lE/* s3hr */,_lG/* s3ht */.a));
  }),new T(function(){
    return B(_lD/* GHC.Base.map */(_lE/* s3hr */, _lG/* s3ht */.b));
  }));
},
_lH/* $w$ctoAny */ = function(_lI/* siw6 */){
  var _lJ/* siwQ */ = new T(function(){
    return B(_kK/* Haste.Prim.Any.$wtoObject */(new T(function(){
      return B(_lv/* LudoJS.$fToAnyGameState_go10 */(_4/* GHC.Types.[] */, E(_lI/* siw6 */).d));
    },1)));
  });
  return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,new T2(0,_1F/* LudoJS.$fFromAnyGameState16 */,new T(function(){
    return B(_l9/* LudoJS.$fToAnyGameState_$ctoAny2 */(E(_lI/* siw6 */).a));
  })),new T2(1,new T2(0,_1E/* LudoJS.$fFromAnyGameState15 */,new T(function(){
    switch(E(E(_lI/* siw6 */).b)){
      case 0:
        return toJSStr/* EXTERNAL */(E(_1u/* LudoJS.$fFromAnyGameState14 */));
        break;
      case 1:
        return toJSStr/* EXTERNAL */(E(_1t/* LudoJS.$fFromAnyGameState13 */));
        break;
      case 2:
        return toJSStr/* EXTERNAL */(E(_1s/* LudoJS.$fFromAnyGameState12 */));
        break;
      default:
        return toJSStr/* EXTERNAL */(E(_1r/* LudoJS.$fFromAnyGameState11 */));
    }
  })),new T2(1,new T2(0,_ct/* LudoJS.$fFromAnyGameState8 */,new T(function(){
    return E(E(_lI/* siw6 */).c);
  })),new T2(1,new T2(0,_cs/* LudoJS.$fFromAnyGameState7 */,_lJ/* siwQ */),new T2(1,new T2(0,_1L/* LudoJS.$fFromAnyGameState5 */,new T(function(){
    return __lst2arr/* EXTERNAL */(B(_lD/* GHC.Base.map */(_kE/* LudoJS.$fToAnyGameState_$ctoAny1 */, E(_lI/* siw6 */).e)));
  })),_4/* GHC.Types.[] */))))));});
},
_lK/* Click */ = 0,
_lL/* $p1MonadEvent */ = function(_lM/* smAR */){
  return E(E(_lM/* smAR */).a);
},
_lN/* $p1MonadIO */ = function(_lO/* suB */){
  return E(E(_lO/* suB */).a);
},
_lP/* >>= */ = function(_lQ/* s34T */){
  return E(E(_lQ/* s34T */).b);
},
_lR/* eventData */ = function(_lS/* smAN */){
  return E(E(_lS/* smAN */).b);
},
_lT/* eventName */ = function(_lU/* smAJ */){
  return E(E(_lU/* smAJ */).a);
},
_lV/* lvl3 */ = function(_/* EXTERNAL */){
  return new F(function(){return nMV/* EXTERNAL */(_j7/* GHC.Base.Nothing */);});
},
_lW/* evtRef */ = new T(function(){
  return B(_dB/* GHC.IO.unsafeDupablePerformIO */(_lV/* Haste.Events.Core.lvl3 */));
}),
_lX/* liftIO */ = function(_lY/* suF */){
  return E(E(_lY/* suF */).b);
},
_lZ/* mkHandler */ = function(_m0/* smAV */){
  return E(E(_m0/* smAV */).b);
},
_m1/* onEvent_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function(e,name,f){e.addEventListener(name,f,false);return [f];})");
}),
_m2/* return */ = function(_m3/* s357 */){
  return E(E(_m3/* s357 */).d);
},
_m4/* onEvent */ = function(_m5/* smBs */, _m6/* smBt */, _m7/* smBu */, _m8/* smBv */, _m9/* smBw */, _ma/* smBx */){
  var _mb/* smBy */ = B(_lL/* Haste.Events.Core.$p1MonadEvent */(_m5/* smBs */)),
  _mc/* smBz */ = B(_lN/* Control.Monad.IO.Class.$p1MonadIO */(_mb/* smBy */)),
  _md/* smBA */ = new T(function(){
    return B(_lX/* Control.Monad.IO.Class.liftIO */(_mb/* smBy */));
  }),
  _me/* smBB */ = new T(function(){
    return B(_m2/* GHC.Base.return */(_mc/* smBz */));
  }),
  _mf/* smBC */ = new T(function(){
    return B(A1(_m6/* smBt */,_m8/* smBv */));
  }),
  _mg/* smBD */ = new T(function(){
    return B(A2(_lT/* Haste.Events.Core.eventName */,_m7/* smBu */, _m9/* smBw */));
  }),
  _mh/* smBE */ = function(_mi/* smBF */){
    return new F(function(){return A1(_me/* smBB */,new T3(0,_mg/* smBD */,_mf/* smBC */,_mi/* smBF */));});
  },
  _mj/* smCl */ = function(_mk/* smBS */){
    var _ml/* smCk */ = new T(function(){
      var _mm/* smCj */ = new T(function(){
        var _mn/* smC7 */ = __createJSFunc/* EXTERNAL */(2, function(_mo/* smBY */, _/* EXTERNAL */){
          var _mp/* smC0 */ = B(A2(E(_mk/* smBS */),_mo/* smBY */, _/* EXTERNAL */));
          return _dF/* Haste.Prim.Any.jsNull */;
        }),
        _mq/* smC9 */ = _mn/* smC7 */;
        return function(_/* EXTERNAL */){
          return new F(function(){return __app3/* EXTERNAL */(E(_m1/* Haste.Events.Core.onEvent_f1 */), E(_mf/* smBC */), E(_mg/* smBD */), _mq/* smC9 */);});
        };
      });
      return B(A1(_md/* smBA */,_mm/* smCj */));
    });
    return new F(function(){return A3(_lP/* GHC.Base.>>= */,_mc/* smBz */, _ml/* smCk */, _mh/* smBE */);});
  },
  _mr/* smBR */ = new T(function(){
    var _ms/* smBH */ = new T(function(){
      return B(_lX/* Control.Monad.IO.Class.liftIO */(_mb/* smBy */));
    }),
    _mt/* smBQ */ = function(_mu/* smBI */){
      var _mv/* smBP */ = new T(function(){
        return B(A1(_ms/* smBH */,function(_/* EXTERNAL */){
          var _/* EXTERNAL */ = wMV/* EXTERNAL */(E(_lW/* Haste.Events.Core.evtRef */), new T1(1,_mu/* smBI */));
          return new F(function(){return A(_lR/* Haste.Events.Core.eventData */,[_m7/* smBu */, _m9/* smBw */, _mu/* smBI */, _/* EXTERNAL */]);});
        }));
      });
      return new F(function(){return A3(_lP/* GHC.Base.>>= */,_mc/* smBz */, _mv/* smBP */, _ma/* smBx */);});
    };
    return B(A2(_lZ/* Haste.Events.Core.mkHandler */,_m5/* smBs */, _mt/* smBQ */));
  });
  return new F(function(){return A3(_lP/* GHC.Base.>>= */,_mc/* smBz */, _mr/* smBR */, _mj/* smCl */);});
},
_mw/* play11 */ = new T1(0,_j7/* GHC.Base.Nothing */),
_mx/* eqInt */ = function(_my/* scEd */, _mz/* scEe */){
  return E(_my/* scEd */)==E(_mz/* scEe */);
},
_mA/* $fEqOption_$c== */ = function(_mB/* si8i */, _mC/* si8j */){
  var _mD/* si8k */ = E(_mB/* si8i */);
  if(!_mD/* si8k */._){
    var _mE/* si8m */ = E(_mC/* si8j */);
    if(!_mE/* si8m */._){
      return new F(function(){return _mx/* GHC.Classes.eqInt */(_mD/* si8k */.a, _mE/* si8m */.a);});
    }else{
      return false;
    }
  }else{
    var _mF/* si8s */ = E(_mC/* si8j */);
    if(!_mF/* si8s */._){
      return false;
    }else{
      if(E(_mD/* si8k */.a)!=E(_mF/* si8s */.a)){
        return false;
      }else{
        return new F(function(){return _mx/* GHC.Classes.eqInt */(_mD/* si8k */.b, _mF/* si8s */.b);});
      }
    }
  }
},
_mG/* $fEqOption_$c/= */ = function(_mH/* si8C */, _mI/* si8D */){
  return (!B(_mA/* LudoJS.$fEqOption_$c== */(_mH/* si8C */, _mI/* si8D */))) ? true : false;
},
_mJ/* $fEqOption */ = new T2(0,_mA/* LudoJS.$fEqOption_$c== */,_mG/* LudoJS.$fEqOption_$c/= */),
_mK/* $fFromAny()4 */ = function(_/* EXTERNAL */){
  return _eb/* GHC.Tuple.() */;
},
_mL/* $wlenAcc */ = function(_mM/* sbMs */, _mN/* sbMt */){
  while(1){
    var _mO/* sbMu */ = E(_mM/* sbMs */);
    if(!_mO/* sbMu */._){
      return E(_mN/* sbMt */);
    }else{
      var _mP/*  sbMt */ = _mN/* sbMt */+1|0;
      _mM/* sbMs */ = _mO/* sbMu */.b;
      _mN/* sbMt */ = _mP/*  sbMt */;
      continue;
    }
  }
},
_mQ/* $sa */ = function(_mR/*  siDO */, _mS/*  siDP */, _mT/*  siDQ */, _mU/*  siDR */, _mV/*  siDS */, _mW/*  siDT */, _mX/*  siDU */, _/* EXTERNAL */){
  while(1){
    var _mY/*  $sa */ = B((function(_mZ/* siDO */, _n0/* siDP */, _n1/* siDQ */, _n2/* siDR */, _n3/* siDS */, _n4/* siDT */, _n5/* siDU */, _/* EXTERNAL */){
      var _n6/* siDW */ = E(_mZ/* siDO */);
      if(!_n6/* siDW */._){
        return new T2(0,_n0/* siDP */,new T5(0,_n1/* siDQ */,_n2/* siDR */,_n3/* siDS */,_n4/* siDT */,_n5/* siDU */));
      }else{
        var _n7/* siDZ */ = _n6/* siDW */.a,
        _n8/*   siDQ */ = _n1/* siDQ */,
        _n9/*   siDR */ = _n2/* siDR */,
        _na/*   siDS */ = _n3/* siDS */,
        _nb/*   siDT */ = _n4/* siDT */,
        _nc/*   siDU */ = _n5/* siDU */;
        _mR/*  siDO */ = _n6/* siDW */.b;
        _mS/*  siDP */ = new T(function(){
          if(!B(_mL/* GHC.List.$wlenAcc */(B(_da/* LudoJS.$s!1 */(_n7/* siDZ */, _n4/* siDT */)), 0))){
            return E(_n0/* siDP */);
          }else{
            return B(_q/* GHC.Base.++ */(_n0/* siDP */, new T2(1,_n7/* siDZ */,_4/* GHC.Types.[] */)));
          }
        });
        _mT/*  siDQ */ = _n8/*   siDQ */;
        _mU/*  siDR */ = _n9/*   siDR */;
        _mV/*  siDS */ = _na/*   siDS */;
        _mW/*  siDT */ = _nb/*   siDT */;
        _mX/*  siDU */ = _nc/*   siDU */;
        return __continue/* EXTERNAL */;
      }
    })(_mR/*  siDO */, _mS/*  siDP */, _mT/*  siDQ */, _mU/*  siDR */, _mV/*  siDS */, _mW/*  siDT */, _mX/*  siDU */, _/* EXTERNAL */));
    if(_mY/*  $sa */!=__continue/* EXTERNAL */){
      return _mY/*  $sa */;
    }
  }
},
_nd/* $sa1 */ = function(_ne/* siE5 */, _nf/* siE6 */, _ng/* siE7 */, _nh/* siE8 */, _ni/* siE9 */, _nj/* siEa */, _nk/* siEb */, _nl/* siEc */, _/* EXTERNAL */){
  return new F(function(){return _mQ/* LudoJS.$sa */(_nf/* siE6 */, new T(function(){
    if(!B(_mL/* GHC.List.$wlenAcc */(B(_da/* LudoJS.$s!1 */(_ne/* siE5 */, _nk/* siEb */)), 0))){
      return E(_ng/* siE7 */);
    }else{
      return B(_q/* GHC.Base.++ */(_ng/* siE7 */, new T2(1,_ne/* siE5 */,_4/* GHC.Types.[] */)));
    }
  }), _nh/* siE8 */, _ni/* siE9 */, _nj/* siEa */, _nk/* siEb */, _nl/* siEc */, _/* EXTERNAL */);});
},
_nm/* go2 */ = function(_nn/* siyj */){
  while(1){
    var _no/* siyk */ = E(_nn/* siyj */);
    if(!_no/* siyk */._){
      return true;
    }else{
      if(!E(E(_no/* siyk */.a).b)._){
        _nn/* siyj */ = _no/* siyk */.b;
        continue;
      }else{
        return false;
      }
    }
  }
},
_np/* modInt# */ = function(_nq/* scF6 */, _nr/* scF7 */){
  var _ns/* scF8 */ = _nq/* scF6 */%_nr/* scF7 */;
  if(_nq/* scF6 */<=0){
    if(_nq/* scF6 */>=0){
      return E(_ns/* scF8 */);
    }else{
      if(_nr/* scF7 */<=0){
        return E(_ns/* scF8 */);
      }else{
        var _nt/* scFf */ = E(_ns/* scF8 */);
        return (_nt/* scFf */==0) ? 0 : _nt/* scFf */+_nr/* scF7 */|0;
      }
    }
  }else{
    if(_nr/* scF7 */>=0){
      if(_nq/* scF6 */>=0){
        return E(_ns/* scF8 */);
      }else{
        if(_nr/* scF7 */<=0){
          return E(_ns/* scF8 */);
        }else{
          var _nu/* scFm */ = E(_ns/* scF8 */);
          return (_nu/* scFm */==0) ? 0 : _nu/* scFm */+_nr/* scF7 */|0;
        }
      }
    }else{
      var _nv/* scFn */ = E(_ns/* scF8 */);
      return (_nv/* scFn */==0) ? 0 : _nv/* scFn */+_nr/* scF7 */|0;
    }
  }
},
_nw/* $wa13 */ = function(_nx/* siys */, _/* EXTERNAL */){
  var _ny/* siyN */ = new T(function(){
    var _nz/* siyu */ = E(_nx/* siys */),
    _nA/* siyy */ = _nz/* siyu */.d,
    _nB/* siyA */ = new T(function(){
      switch(E(_nz/* siyu */.b)){
        case 0:
          switch(B(_np/* GHC.Classes.modInt# */(1, 4))+1|0){
            case 1:
              return 0;
              break;
            case 2:
              return 1;
              break;
            case 3:
              return 3;
              break;
            case 4:
              return 2;
              break;
            default:
              return E(_hk/* LudoJS.numToColor1 */);
          }
          break;
        case 1:
          switch(B(_np/* GHC.Classes.modInt# */(2, 4))+1|0){
            case 1:
              return 0;
              break;
            case 2:
              return 1;
              break;
            case 3:
              return 3;
              break;
            case 4:
              return 2;
              break;
            default:
              return E(_hk/* LudoJS.numToColor1 */);
          }
          break;
        case 2:
          switch(B(_np/* GHC.Classes.modInt# */(4, 4))+1|0){
            case 1:
              return 0;
              break;
            case 2:
              return 1;
              break;
            case 3:
              return 3;
              break;
            case 4:
              return 2;
              break;
            default:
              return E(_hk/* LudoJS.numToColor1 */);
          }
          break;
        default:
          switch(B(_np/* GHC.Classes.modInt# */(3, 4))+1|0){
            case 1:
              return 0;
              break;
            case 2:
              return 1;
              break;
            case 3:
              return 3;
              break;
            case 4:
              return 2;
              break;
            default:
              return E(_hk/* LudoJS.numToColor1 */);
          }
      }
    });
    return new T5(0,_mw/* LudoJS.play11 */,_nB/* siyA */,new T(function(){
      if(!B(_nm/* LudoJS.go2 */(B(_da/* LudoJS.$s!1 */(_nB/* siyA */, _nA/* siyy */))))){
        return E(_di/* LudoJS.numPiecesAt2 */);
      }else{
        return E(_63/* LudoJS.play10 */);
      }
    }),_nA/* siyy */,_nz/* siyu */.e);
  });
  return new T2(0,_eb/* GHC.Tuple.() */,_ny/* siyN */);
},
_nC/* $wa15 */ = function(_nD/* sizv */, _nE/* sizw */, _nF/* sizx */, _nG/* sizy */, _nH/* sizz */, _/* EXTERNAL */){
  var _nI/* sizD */ = new T5(0,_nD/* sizv */,_nE/* sizw */,_nF/* sizx */,_nG/* sizy */,_nH/* sizz */),
  _nJ/* sizE */ = function(_nK/* sizF */){
    var _nL/* sizG */ = B(_nw/* LudoJS.$wa13 */(_nI/* sizD */, _/* EXTERNAL */)),
    _nM/* sizJ */ = E(_nL/* sizG */),
    _nN/* sizM */ = E(_nM/* sizJ */.b),
    _nO/* sizS */ = B(_nC/* LudoJS.$wa15 */(_nN/* sizM */.a, _nN/* sizM */.b, _nN/* sizM */.c, _nN/* sizM */.d, _nN/* sizM */.e, _/* EXTERNAL */));
    return new T2(0,new T2(1,_nM/* sizJ */.a,new T(function(){
      return E(E(_nO/* sizS */).a);
    })),new T(function(){
      return E(E(_nO/* sizS */).b);
    }));
  };
  if(!E(B(_mL/* GHC.List.$wlenAcc */(B(_da/* LudoJS.$s!1 */(_nE/* sizw */, _nG/* sizy */)), 0)))){
    return new F(function(){return _nJ/* sizE */(_/* EXTERNAL */);});
  }else{
    if(E(_nF/* sizx */)>=1){
      return new T2(0,_4/* GHC.Types.[] */,_nI/* sizD */);
    }else{
      return new F(function(){return _nJ/* sizE */(_/* EXTERNAL */);});
    }
  }
},
_nP/* $fToAnyOption3 */ = "Move",
_nQ/* $fToAnyOption4 */ = "option",
_nR/* $fToAnyOption2 */ = new T2(0,_nQ/* LudoJS.$fToAnyOption4 */,_nP/* LudoJS.$fToAnyOption3 */),
_nS/* $fToAnyOption7 */ = "Play",
_nT/* $fToAnyOption6 */ = new T2(0,_nQ/* LudoJS.$fToAnyOption4 */,_nS/* LudoJS.$fToAnyOption7 */),
_nU/* $w$ctoAny2 */ = function(_nV/* si5x */){
  var _nW/* si5y */ = E(_nV/* si5x */);
  if(!_nW/* si5y */._){
    return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,_nT/* LudoJS.$fToAnyOption6 */,new T2(1,new T2(0,_1N/* LudoJS.$fToAnyOption5 */,_nW/* si5y */.a),_4/* GHC.Types.[] */)));});
  }else{
    return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,_nR/* LudoJS.$fToAnyOption2 */,new T2(1,new T2(0,_1N/* LudoJS.$fToAnyOption5 */,_nW/* si5y */.a),new T2(1,new T2(0,_1M/* LudoJS.$fToAnyOption1 */,_nW/* si5y */.b),_4/* GHC.Types.[] */))));});
  }
},
_nX/* $fToAnyOption_$ctoAny */ = function(_nY/* si5K */){
  return new F(function(){return _nU/* LudoJS.$w$ctoAny2 */(_nY/* si5K */);});
},
_nZ/* a42 */ = function(_o0/* shJP */, _o1/* shJQ */, _/* EXTERNAL */){
  var _o2/* shM2 */ = new T(function(){
    var _o3/* shJS */ = E(_o1/* shJQ */),
    _o4/* shJY */ = function(_o5/* shJZ */){
      var _o6/* shK0 */ = E(_o5/* shJZ */);
      if(!_o6/* shK0 */._){
        return __Z/* EXTERNAL */;
      }else{
        var _o7/* shK2 */ = _o6/* shK0 */.b,
        _o8/* shK3 */ = E(_o6/* shK0 */.a),
        _o9/* shK4 */ = _o8/* shK3 */.a,
        _oa/* shK6 */ = E(_o8/* shK3 */.b);
        if(!_oa/* shK6 */._){
          var _ob/* shK9 */ = E(_o0/* shJP */);
          if(_ob/* shK9 */==6){
            var _oc/* shKV */ = new T(function(){
              var _od/* shKx */ = function(_oe/*  shKy */){
                while(1){
                  var _of/*  shKx */ = B((function(_og/* shKy */){
                    var _oh/* shKz */ = E(_og/* shKy */);
                    if(!_oh/* shKz */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _oi/* shKB */ = _oh/* shKz */.b,
                      _oj/* shKC */ = E(_oh/* shKz */.a),
                      _ok/* shKD */ = _oj/* shKC */.a,
                      _ol/* shKF */ = E(_oj/* shKC */.b);
                      if(!_ol/* shKF */._){
                        return new T2(1,new T1(0,_ok/* shKD */),new T(function(){
                          return B(_od/* shKx */(_oi/* shKB */));
                        }));
                      }else{
                        var _om/* shKJ */ = E(_ol/* shKF */.a);
                        if(_om/* shKJ */>=51){
                          if((6+_om/* shKJ */|0)==56){
                            return new T2(1,new T2(1,_ok/* shKD */,56),new T(function(){
                              return B(_od/* shKx */(_oi/* shKB */));
                            }));
                          }else{
                            _oe/*  shKy */ = _oi/* shKB */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          return new T2(1,new T2(1,_ok/* shKD */,6+_om/* shKJ */|0),new T(function(){
                            return B(_od/* shKx */(_oi/* shKB */));
                          }));
                        }
                      }
                    }
                  })(_oe/*  shKy */));
                  if(_of/*  shKx */!=__continue/* EXTERNAL */){
                    return _of/*  shKx */;
                  }
                }
              };
              return B(_od/* shKx */(_o7/* shK2 */));
            });
            return new T2(1,new T1(0,_o9/* shK4 */),_oc/* shKV */);
          }else{
            var _on/* shKa */ = function(_oo/*  shKb */){
              while(1){
                var _op/*  shKa */ = B((function(_oq/* shKb */){
                  var _or/* shKc */ = E(_oq/* shKb */);
                  if(!_or/* shKc */._){
                    return __Z/* EXTERNAL */;
                  }else{
                    var _os/* shKe */ = _or/* shKc */.b,
                    _ot/* shKf */ = E(_or/* shKc */.a),
                    _ou/* shKg */ = _ot/* shKf */.a,
                    _ov/* shKi */ = E(_ot/* shKf */.b);
                    if(!_ov/* shKi */._){
                      _oo/*  shKb */ = _os/* shKe */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _ow/* shKk */ = E(_ov/* shKi */.a);
                      if(_ow/* shKk */>=51){
                        if((_ob/* shK9 */+_ow/* shKk */|0)==56){
                          return new T2(1,new T2(1,_ou/* shKg */,56),new T(function(){
                            return B(_on/* shKa */(_os/* shKe */));
                          }));
                        }else{
                          _oo/*  shKb */ = _os/* shKe */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        return new T2(1,new T2(1,_ou/* shKg */,_ob/* shK9 */+_ow/* shKk */|0),new T(function(){
                          return B(_on/* shKa */(_os/* shKe */));
                        }));
                      }
                    }
                  }
                })(_oo/*  shKb */));
                if(_op/*  shKa */!=__continue/* EXTERNAL */){
                  return _op/*  shKa */;
                }
              }
            };
            return new F(function(){return _on/* shKa */(_o7/* shK2 */);});
          }
        }else{
          var _ox/* shKX */ = E(_oa/* shK6 */.a);
          if(_ox/* shKX */>=51){
            var _oy/* shL1 */ = E(_o0/* shJP */);
            if((_oy/* shL1 */+_ox/* shKX */|0)==56){
              var _oz/* shLU */ = new T(function(){
                var _oA/* shLv */ = function(_oB/*  shLw */){
                  while(1){
                    var _oC/*  shLv */ = B((function(_oD/* shLw */){
                      var _oE/* shLx */ = E(_oD/* shLw */);
                      if(!_oE/* shLx */._){
                        return __Z/* EXTERNAL */;
                      }else{
                        var _oF/* shLz */ = _oE/* shLx */.b,
                        _oG/* shLA */ = E(_oE/* shLx */.a),
                        _oH/* shLB */ = _oG/* shLA */.a,
                        _oI/* shLD */ = E(_oG/* shLA */.b);
                        if(!_oI/* shLD */._){
                          if(E(_oy/* shL1 */)==6){
                            return new T2(1,new T1(0,_oH/* shLB */),new T(function(){
                              return B(_oA/* shLv */(_oF/* shLz */));
                            }));
                          }else{
                            _oB/*  shLw */ = _oF/* shLz */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          var _oJ/* shLI */ = E(_oI/* shLD */.a);
                          if(_oJ/* shLI */>=51){
                            if((_oy/* shL1 */+_oJ/* shLI */|0)==56){
                              return new T2(1,new T2(1,_oH/* shLB */,56),new T(function(){
                                return B(_oA/* shLv */(_oF/* shLz */));
                              }));
                            }else{
                              _oB/*  shLw */ = _oF/* shLz */;
                              return __continue/* EXTERNAL */;
                            }
                          }else{
                            return new T2(1,new T2(1,_oH/* shLB */,_oy/* shL1 */+_oJ/* shLI */|0),new T(function(){
                              return B(_oA/* shLv */(_oF/* shLz */));
                            }));
                          }
                        }
                      }
                    })(_oB/*  shLw */));
                    if(_oC/*  shLv */!=__continue/* EXTERNAL */){
                      return _oC/*  shLv */;
                    }
                  }
                };
                return B(_oA/* shLv */(_o7/* shK2 */));
              });
              return new T2(1,new T2(1,_o9/* shK4 */,56),_oz/* shLU */);
            }else{
              var _oK/* shL4 */ = function(_oL/*  shL5 */){
                while(1){
                  var _oM/*  shL4 */ = B((function(_oN/* shL5 */){
                    var _oO/* shL6 */ = E(_oN/* shL5 */);
                    if(!_oO/* shL6 */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _oP/* shL8 */ = _oO/* shL6 */.b,
                      _oQ/* shL9 */ = E(_oO/* shL6 */.a),
                      _oR/* shLa */ = _oQ/* shL9 */.a,
                      _oS/* shLc */ = E(_oQ/* shL9 */.b);
                      if(!_oS/* shLc */._){
                        if(E(_oy/* shL1 */)==6){
                          return new T2(1,new T1(0,_oR/* shLa */),new T(function(){
                            return B(_oK/* shL4 */(_oP/* shL8 */));
                          }));
                        }else{
                          _oL/*  shL5 */ = _oP/* shL8 */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        var _oT/* shLh */ = E(_oS/* shLc */.a);
                        if(_oT/* shLh */>=51){
                          if((_oy/* shL1 */+_oT/* shLh */|0)==56){
                            return new T2(1,new T2(1,_oR/* shLa */,56),new T(function(){
                              return B(_oK/* shL4 */(_oP/* shL8 */));
                            }));
                          }else{
                            _oL/*  shL5 */ = _oP/* shL8 */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          return new T2(1,new T2(1,_oR/* shLa */,_oy/* shL1 */+_oT/* shLh */|0),new T(function(){
                            return B(_oK/* shL4 */(_oP/* shL8 */));
                          }));
                        }
                      }
                    }
                  })(_oL/*  shL5 */));
                  if(_oM/*  shL4 */!=__continue/* EXTERNAL */){
                    return _oM/*  shL4 */;
                  }
                }
              };
              return new F(function(){return _oK/* shL4 */(_o7/* shK2 */);});
            }
          }else{
            return new T2(1,new T2(1,_o9/* shK4 */,new T(function(){
              return E(_o0/* shJP */)+_ox/* shKX */|0;
            })),new T(function(){
              return B(_o4/* shJY */(_o7/* shK2 */));
            }));
          }
        }
      }
    };
    return B(_o4/* shJY */(B(_da/* LudoJS.$s!1 */(_o3/* shJS */.b, _o3/* shJS */.d))));
  });
  return new T2(0,_o2/* shM2 */,_o1/* shJQ */);
},
_oU/* play3 */ = "((gs, opts, rolls) => drawBoard(gs, opts, rolls))",
_oV/* f2 */ = new T(function(){
  return eval/* EXTERNAL */(E(_oU/* LudoJS.play3 */));
}),
_oW/* lvl13 */ = new T(function(){
  return B(_1o/* Control.Exception.Base.patError */("LudoJS.hs:(312,5)-(316,32)|case"));
}),
_oX/* $wa16 */ = function(_oY/* siCS */, _oZ/* siCT */, _p0/* siCU */, _p1/* siCV */, _p2/* siCW */, _p3/* siCX */, _/* EXTERNAL */){
  var _p4/* siCZ */ = function(_p5/* siD0 */, _p6/* siD1 */){
    var _p7/* siD4 */ = function(_/* EXTERNAL */, _p8/* siD6 */, _p9/* siD7 */){
      var _pa/* siDd */ = __lst2arr/* EXTERNAL */(B(_lD/* GHC.Base.map */(_nX/* LudoJS.$fToAnyOption_$ctoAny */, _p8/* siD6 */))),
      _pb/* siDj */ = __app3/* EXTERNAL */(E(_oV/* LudoJS.f2 */), B(_lH/* LudoJS.$w$ctoAny */(new T5(0,_oY/* siCS */,_oZ/* siCT */,_p0/* siCU */,_p1/* siCV */,_p2/* siCW */))), _pa/* siDd */, _p5/* siD0 */);
      return new T2(0,_eb/* GHC.Tuple.() */,_p9/* siD7 */);
    };
    if(E(_p5/* siD0 */)==( -1)){
      return new F(function(){return _p7/* siD4 */(_/* EXTERNAL */, _4/* GHC.Types.[] */, _p3/* siCX */);});
    }else{
      var _pc/* siDo */ = B(_nZ/* LudoJS.a42 */(_p6/* siD1 */, _p3/* siCX */, _/* EXTERNAL */)),
      _pd/* siDr */ = E(_pc/* siDo */);
      return new F(function(){return _p7/* siD4 */(_/* EXTERNAL */, _pd/* siDr */.a, _pd/* siDr */.b);});
    }
  },
  _pe/* siDu */ = E(_oY/* siCS */);
  switch(_pe/* siDu */._){
    case 0:
      var _pf/* siDw */ = E(_pe/* siDu */.a);
      if(!_pf/* siDw */._){
        return new F(function(){return _p4/* siCZ */( -1, _kX/* LudoJS.$fToAnyGameState18 */);});
      }else{
        var _pg/* siDy */ = E(_pf/* siDw */.a);
        return new F(function(){return _p4/* siCZ */(_pg/* siDy */, _pg/* siDy */);});
      }
      break;
    case 1:
      var _ph/* siDB */ = E(_pe/* siDu */.a);
      return new F(function(){return _p4/* siCZ */(_ph/* siDB */, _ph/* siDB */);});
      break;
    case 2:
      var _pi/* siDF */ = E(_pe/* siDu */.a);
      return new F(function(){return _p4/* siCZ */(_pi/* siDF */, _pi/* siDF */);});
      break;
    default:
      return E(_oW/* LudoJS.lvl13 */);
  }
},
_pj/* neInt */ = function(_pk/* scEM */, _pl/* scEN */){
  return E(_pk/* scEM */)!=E(_pl/* scEN */);
},
_pm/* $fEqInt */ = new T2(0,_mx/* GHC.Classes.eqInt */,_pj/* GHC.Classes.neInt */),
_pn/* $soutByCell */ = function(_po/* shMl */, _pp/* shMm */){
  var _pq/* shMn */ = E(_po/* shMl */);
  if(!_pq/* shMn */._){
    return __Z/* EXTERNAL */;
  }else{
    var _pr/* shMp */ = _pq/* shMn */.b,
    _ps/* shMq */ = E(_pq/* shMn */.a),
    _pt/* shMt */ = E(_ps/* shMq */.b);
    return (_pt/* shMt */._==0) ? new T2(1,_ps/* shMq */,new T(function(){
      return B(_pn/* LudoJS.$soutByCell */(_pr/* shMp */, _pp/* shMm */));
    })) : (_pp/* shMm */!=E(_pt/* shMt */.a)) ? new T2(1,_ps/* shMq */,new T(function(){
      return B(_pn/* LudoJS.$soutByCell */(_pr/* shMp */, _pp/* shMm */));
    })) : new T2(1,new T2(0,_ps/* shMq */.a,_60/* LudoJS.Out */),new T(function(){
      return B(_pn/* LudoJS.$soutByCell */(_pr/* shMp */, _pp/* shMm */));
    }));
  }
},
_pu/* $sremoveFrom */ = function(_pv/* shMX */, _pw/* shMY */){
  var _px/* shMZ */ = E(_pv/* shMX */);
  if(!_px/* shMZ */._){
    return __Z/* EXTERNAL */;
  }else{
    var _py/* shN1 */ = _px/* shMZ */.b,
    _pz/* shN2 */ = E(_px/* shMZ */.a);
    return (_pw/* shMY */!=E(_pz/* shN2 */.a)) ? new T2(1,_pz/* shN2 */,new T(function(){
      return B(_pu/* LudoJS.$sremoveFrom */(_py/* shN1 */, _pw/* shMY */));
    })) : E(_py/* shN1 */);
  }
},
_pA/* lvl1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("!!: negative index"));
}),
_pB/* prel_list_str */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Prelude."));
}),
_pC/* lvl2 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_pB/* GHC.List.prel_list_str */, _pA/* GHC.List.lvl1 */));
}),
_pD/* negIndex */ = new T(function(){
  return B(err/* EXTERNAL */(_pC/* GHC.List.lvl2 */));
}),
_pE/* lvl3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("!!: index too large"));
}),
_pF/* lvl4 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_pB/* GHC.List.prel_list_str */, _pE/* GHC.List.lvl3 */));
}),
_pG/* !!1 */ = new T(function(){
  return B(err/* EXTERNAL */(_pF/* GHC.List.lvl4 */));
}),
_pH/* poly_$wgo2 */ = function(_pI/* sbFw */, _pJ/* sbFx */){
  while(1){
    var _pK/* sbFy */ = E(_pI/* sbFw */);
    if(!_pK/* sbFy */._){
      return E(_pG/* GHC.List.!!1 */);
    }else{
      var _pL/* sbFB */ = E(_pJ/* sbFx */);
      if(!_pL/* sbFB */){
        return E(_pK/* sbFy */.a);
      }else{
        _pI/* sbFw */ = _pK/* sbFy */.b;
        _pJ/* sbFx */ = _pL/* sbFB */-1|0;
        continue;
      }
    }
  }
},
_pM/* $w!! */ = function(_pN/* sbFD */, _pO/* sbFE */){
  if(_pO/* sbFE */>=0){
    return new F(function(){return _pH/* GHC.List.poly_$wgo2 */(_pN/* sbFD */, _pO/* sbFE */);});
  }else{
    return E(_pD/* GHC.List.negIndex */);
  }
},
_pP/* $wconvertCell */ = function(_pQ/* shNJ */, _pR/* shNK */, _pS/* shNL */){
  switch(E(_pS/* shNL */)){
    case 0:
      switch(E(_pQ/* shNJ */)){
        case 0:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
      break;
    case 1:
      switch(E(_pQ/* shNJ */)){
        case 0:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
      break;
    case 2:
      switch(E(_pQ/* shNJ */)){
        case 0:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
      break;
    default:
      switch(E(_pQ/* shNJ */)){
        case 0:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shNK */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
  }
},
_pT/* $s!_$spoly_go1 */ = function(_pU/* shCV */){
  while(1){
    var _pV/* shCW */ = E(_pU/* shCV */);
    if(!_pV/* shCW */._){
      var _pW/* shD0 */ = _pV/* shCW */.d;
      switch(E(_pV/* shCW */.b)){
        case 0:
          _pU/* shCV */ = _pV/* shCW */.e;
          continue;
        case 1:
          return E(_pV/* shCW */.c);
        case 2:
          _pU/* shCV */ = _pW/* shD0 */;
          continue;
        default:
          _pU/* shCV */ = _pW/* shD0 */;
          continue;
      }
    }else{
      return E(_d9/* LudoJS.lvl26 */);
    }
  }
},
_pX/* $s!_$spoly_go10 */ = function(_pY/* shCN */){
  while(1){
    var _pZ/* shCO */ = E(_pY/* shCN */);
    if(!_pZ/* shCO */._){
      if(E(_pZ/* shCO */.b)==3){
        return E(_pZ/* shCO */.c);
      }else{
        _pY/* shCN */ = _pZ/* shCO */.e;
        continue;
      }
    }else{
      return E(_d9/* LudoJS.lvl26 */);
    }
  }
},
_q0/* $s!_$spoly_go2 */ = function(_q1/* shD3 */){
  while(1){
    var _q2/* shD4 */ = E(_q1/* shD3 */);
    if(!_q2/* shD4 */._){
      var _q3/* shD8 */ = _q2/* shD4 */.d;
      switch(E(_q2/* shD4 */.b)){
        case 0:
          return E(_q2/* shD4 */.c);
        case 1:
          _q1/* shD3 */ = _q3/* shD8 */;
          continue;
        case 2:
          _q1/* shD3 */ = _q3/* shD8 */;
          continue;
        default:
          _q1/* shD3 */ = _q3/* shD8 */;
          continue;
      }
    }else{
      return E(_d9/* LudoJS.lvl26 */);
    }
  }
},
_q4/* $sinsert_$s$sgo1 */ = function(_q5/* shEZ */, _q6/* shF0 */){
  var _q7/* shF1 */ = E(_q6/* shF0 */);
  if(!_q7/* shF1 */._){
    var _q8/* shF4 */ = _q7/* shF1 */.c,
    _q9/* shF5 */ = _q7/* shF1 */.d,
    _qa/* shF6 */ = _q7/* shF1 */.e;
    switch(E(_q7/* shF1 */.b)){
      case 0:
        return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1O/* LudoJS.Blue */, _q8/* shF4 */, _q9/* shF5 */, B(_q4/* LudoJS.$sinsert_$s$sgo1 */(_q5/* shEZ */, _qa/* shF6 */)));});
        break;
      case 1:
        return new T5(0,_q7/* shF1 */.a,E(_1P/* LudoJS.Green */),_q5/* shEZ */,E(_q9/* shF5 */),E(_qa/* shF6 */));
      case 2:
        return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1Q/* LudoJS.Red */, _q8/* shF4 */, B(_q4/* LudoJS.$sinsert_$s$sgo1 */(_q5/* shEZ */, _q9/* shF5 */)), _qa/* shF6 */);});
        break;
      default:
        return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1S/* LudoJS.Yellow */, _q8/* shF4 */, B(_q4/* LudoJS.$sinsert_$s$sgo1 */(_q5/* shEZ */, _q9/* shF5 */)), _qa/* shF6 */);});
    }
  }else{
    return new T5(0,1,E(_1P/* LudoJS.Green */),_q5/* shEZ */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
  }
},
_qb/* $sinsert_$s$sgo10 */ = function(_qc/* shEN */, _qd/* shEO */){
  var _qe/* shEP */ = E(_qd/* shEO */);
  if(!_qe/* shEP */._){
    var _qf/* shES */ = _qe/* shEP */.c,
    _qg/* shET */ = _qe/* shEP */.d,
    _qh/* shEU */ = _qe/* shEP */.e;
    switch(E(_qe/* shEP */.b)){
      case 0:
        return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1O/* LudoJS.Blue */, _qf/* shES */, _qg/* shET */, B(_qb/* LudoJS.$sinsert_$s$sgo10 */(_qc/* shEN */, _qh/* shEU */)));});
        break;
      case 1:
        return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1P/* LudoJS.Green */, _qf/* shES */, _qg/* shET */, B(_qb/* LudoJS.$sinsert_$s$sgo10 */(_qc/* shEN */, _qh/* shEU */)));});
        break;
      case 2:
        return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1Q/* LudoJS.Red */, _qf/* shES */, _qg/* shET */, B(_qb/* LudoJS.$sinsert_$s$sgo10 */(_qc/* shEN */, _qh/* shEU */)));});
        break;
      default:
        return new T5(0,_qe/* shEP */.a,E(_1S/* LudoJS.Yellow */),_qc/* shEN */,E(_qg/* shET */),E(_qh/* shEU */));
    }
  }else{
    return new T5(0,1,E(_1S/* LudoJS.Yellow */),_qc/* shEN */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
  }
},
_qi/* $sinsert_$s$sgo2 */ = function(_qj/* shFb */, _qk/* shFc */){
  var _ql/* shFd */ = E(_qk/* shFc */);
  if(!_ql/* shFd */._){
    var _qm/* shFg */ = _ql/* shFd */.c,
    _qn/* shFh */ = _ql/* shFd */.d,
    _qo/* shFi */ = _ql/* shFd */.e;
    switch(E(_ql/* shFd */.b)){
      case 0:
        return new T5(0,_ql/* shFd */.a,E(_1O/* LudoJS.Blue */),_qj/* shFb */,E(_qn/* shFh */),E(_qo/* shFi */));
      case 1:
        return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1P/* LudoJS.Green */, _qm/* shFg */, B(_qi/* LudoJS.$sinsert_$s$sgo2 */(_qj/* shFb */, _qn/* shFh */)), _qo/* shFi */);});
        break;
      case 2:
        return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1Q/* LudoJS.Red */, _qm/* shFg */, B(_qi/* LudoJS.$sinsert_$s$sgo2 */(_qj/* shFb */, _qn/* shFh */)), _qo/* shFi */);});
        break;
      default:
        return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1S/* LudoJS.Yellow */, _qm/* shFg */, B(_qi/* LudoJS.$sinsert_$s$sgo2 */(_qj/* shFb */, _qn/* shFh */)), _qo/* shFi */);});
    }
  }else{
    return new T5(0,1,E(_1O/* LudoJS.Blue */),_qj/* shFb */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
  }
},
_qp/* False */ = false,
_qq/* geInteger */ = function(_qr/* s1FG */, _qs/* s1FH */){
  var _qt/* s1FI */ = E(_qr/* s1FG */);
  if(!_qt/* s1FI */._){
    var _qu/* s1FJ */ = _qt/* s1FI */.a,
    _qv/* s1FK */ = E(_qs/* s1FH */);
    return (_qv/* s1FK */._==0) ? _qu/* s1FJ */>=_qv/* s1FK */.a : I_compareInt/* EXTERNAL */(_qv/* s1FK */.a, _qu/* s1FJ */)<=0;
  }else{
    var _qw/* s1FR */ = _qt/* s1FI */.a,
    _qx/* s1FS */ = E(_qs/* s1FH */);
    return (_qx/* s1FS */._==0) ? I_compareInt/* EXTERNAL */(_qw/* s1FR */, _qx/* s1FS */.a)>=0 : I_compare/* EXTERNAL */(_qw/* s1FR */, _qx/* s1FS */.a)>=0;
  }
},
_qy/* lvl10 */ = new T1(0,2),
_qz/* lvl11 */ = new T1(0,0),
_qA/* lvl12 */ = new T1(0,1),
_qB/* outByCell */ = function(_qC/* shMD */, _qD/* shME */){
  var _qE/* shMF */ = E(_qC/* shMD */);
  if(!_qE/* shMF */._){
    return __Z/* EXTERNAL */;
  }else{
    var _qF/* shMH */ = _qE/* shMF */.b,
    _qG/* shMI */ = E(_qE/* shMF */.a),
    _qH/* shML */ = E(_qG/* shMI */.b);
    if(!_qH/* shML */._){
      return new T2(1,_qG/* shMI */,new T(function(){
        return B(_qB/* LudoJS.outByCell */(_qF/* shMH */, _qD/* shME */));
      }));
    }else{
      var _qI/* shMO */ = E(_qD/* shME */);
      return (_qI/* shMO */!=E(_qH/* shML */.a)) ? new T2(1,_qG/* shMI */,new T(function(){
        return B(_pn/* LudoJS.$soutByCell */(_qF/* shMH */, _qI/* shMO */));
      })) : new T2(1,new T2(0,_qG/* shMI */.a,_60/* LudoJS.Out */),new T(function(){
        return B(_pn/* LudoJS.$soutByCell */(_qF/* shMH */, _qI/* shMO */));
      }));
    }
  }
},
_qJ/* plusInteger */ = function(_qK/* s1Qe */, _qL/* s1Qf */){
  while(1){
    var _qM/* s1Qg */ = E(_qK/* s1Qe */);
    if(!_qM/* s1Qg */._){
      var _qN/* s1Qh */ = _qM/* s1Qg */.a,
      _qO/* s1Qi */ = E(_qL/* s1Qf */);
      if(!_qO/* s1Qi */._){
        var _qP/* s1Qj */ = _qO/* s1Qi */.a,
        _qQ/* s1Qk */ = addC/* EXTERNAL */(_qN/* s1Qh */, _qP/* s1Qj */);
        if(!E(_qQ/* s1Qk */.b)){
          return new T1(0,_qQ/* s1Qk */.a);
        }else{
          _qK/* s1Qe */ = new T1(1,I_fromInt/* EXTERNAL */(_qN/* s1Qh */));
          _qL/* s1Qf */ = new T1(1,I_fromInt/* EXTERNAL */(_qP/* s1Qj */));
          continue;
        }
      }else{
        _qK/* s1Qe */ = new T1(1,I_fromInt/* EXTERNAL */(_qN/* s1Qh */));
        _qL/* s1Qf */ = _qO/* s1Qi */;
        continue;
      }
    }else{
      var _qR/* s1Qz */ = E(_qL/* s1Qf */);
      if(!_qR/* s1Qz */._){
        _qK/* s1Qe */ = _qM/* s1Qg */;
        _qL/* s1Qf */ = new T1(1,I_fromInt/* EXTERNAL */(_qR/* s1Qz */.a));
        continue;
      }else{
        return new T1(1,I_add/* EXTERNAL */(_qM/* s1Qg */.a, _qR/* s1Qz */.a));
      }
    }
  }
},
_qS/* a43 */ = function(_qT/* shPf */, _qU/* shPg */, _qV/* shPh */, _/* EXTERNAL */){
  var _qW/* shPj */ = function(_qX/* shPk */){
    var _qY/* shPl */ = E(_qX/* shPk */);
    if(!_qY/* shPl */._){
      return E(_qz/* LudoJS.lvl11 */);
    }else{
      var _qZ/* shPn */ = _qY/* shPl */.b,
      _r0/* shPo */ = E(_qU/* shPg */);
      if(_r0/* shPo */!=E(_qY/* shPl */.a)){
        var _r1/* shPu */ = function(_r2/* shPv */){
          while(1){
            var _r3/* shPw */ = E(_r2/* shPv */);
            if(!_r3/* shPw */._){
              return E(_qz/* LudoJS.lvl11 */);
            }else{
              var _r4/* shPy */ = _r3/* shPw */.b;
              if(_r0/* shPo */!=E(_r3/* shPw */.a)){
                _r2/* shPv */ = _r4/* shPy */;
                continue;
              }else{
                return new F(function(){return _qJ/* GHC.Integer.Type.plusInteger */(B(_r1/* shPu */(_r4/* shPy */)), _qA/* LudoJS.lvl12 */);});
              }
            }
          }
        };
        return new F(function(){return _r1/* shPu */(_qZ/* shPn */);});
      }else{
        var _r5/* shPE */ = function(_r6/* shPF */){
          while(1){
            var _r7/* shPG */ = E(_r6/* shPF */);
            if(!_r7/* shPG */._){
              return E(_qz/* LudoJS.lvl11 */);
            }else{
              var _r8/* shPI */ = _r7/* shPG */.b;
              if(_r0/* shPo */!=E(_r7/* shPG */.a)){
                _r6/* shPF */ = _r8/* shPI */;
                continue;
              }else{
                return new F(function(){return _qJ/* GHC.Integer.Type.plusInteger */(B(_r5/* shPE */(_r8/* shPI */)), _qA/* LudoJS.lvl12 */);});
              }
            }
          }
        };
        return new F(function(){return _qJ/* GHC.Integer.Type.plusInteger */(B(_r5/* shPE */(_qZ/* shPn */)), _qA/* LudoJS.lvl12 */);});
      }
    }
  },
  _r9/* shPP */ = function(_ra/* shPQ */, _/* EXTERNAL */){
    var _rb/* shQg */ = new T(function(){
      var _rc/* shPY */ = function(_rd/*  shPZ */){
        while(1){
          var _re/*  shPY */ = B((function(_rf/* shPZ */){
            var _rg/* shQ0 */ = E(_rf/* shPZ */);
            if(!_rg/* shQ0 */._){
              return __Z/* EXTERNAL */;
            }else{
              var _rh/* shQ2 */ = _rg/* shQ0 */.b,
              _ri/* shQ6 */ = E(E(_rg/* shQ0 */.a).b);
              if(!_ri/* shQ6 */._){
                _rd/*  shPZ */ = _rh/* shQ2 */;
                return __continue/* EXTERNAL */;
              }else{
                return new T2(1,new T(function(){
                  return B(_pP/* LudoJS.$wconvertCell */(_1O/* LudoJS.Blue */, E(_ri/* shQ6 */.a), _qT/* shPf */));
                }),new T(function(){
                  return B(_rc/* shPY */(_rh/* shQ2 */));
                }));
              }
            }
          })(_rd/*  shPZ */));
          if(_re/*  shPY */!=__continue/* EXTERNAL */){
            return _re/*  shPY */;
          }
        }
      };
      return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shPj */(B(_rc/* shPY */(B(_q0/* LudoJS.$s!_$spoly_go2 */(E(_ra/* shPQ */).d)))))), _qy/* LudoJS.lvl10 */));
    });
    return new T2(0,_rb/* shQg */,_ra/* shPQ */);
  },
  _rj/* shQi */ = function(_/* EXTERNAL */, _rk/* shQk */){
    var _rl/* shQl */ = E(_rk/* shQk */),
    _rm/* shQn */ = _rl/* shQl */.b;
    if(!E(_rl/* shQl */.a)){
      var _rn/* shQp */ = function(_/* EXTERNAL */, _ro/* shQr */, _rp/* shQs */){
        var _rq/* shQt */ = function(_/* EXTERNAL */, _rr/* shQv */, _rs/* shQw */){
          var _rt/* shQx */ = function(_/* EXTERNAL */, _ru/* shQz */){
            var _rv/* shQA */ = E(_qT/* shPf */);
            if(_rv/* shQA */==2){
              return new T2(0,_eb/* GHC.Tuple.() */,new T(function(){
                return E(E(_ru/* shQz */).b);
              }));
            }else{
              var _rw/* shR9 */ = new T(function(){
                var _rx/* shQE */ = E(E(_ru/* shQz */).b),
                _ry/* shQI */ = _rx/* shQE */.d,
                _rz/* shR8 */ = new T(function(){
                  var _rA/* shR7 */ = new T(function(){
                    return B(_qB/* LudoJS.outByCell */(B(_da/* LudoJS.$s!1 */(_1Q/* LudoJS.Red */, _ry/* shQI */)), new T(function(){
                      var _rB/* shQL */ = E(_qU/* shPg */);
                      switch(E(_rv/* shQA */)){
                        case 0:
                          return B(_np/* GHC.Classes.modInt# */(_rB/* shQL */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52));
                          break;
                        case 1:
                          return B(_np/* GHC.Classes.modInt# */(_rB/* shQL */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52));
                          break;
                        default:
                          return B(_np/* GHC.Classes.modInt# */(_rB/* shQL */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52));
                      }
                    },1)));
                  });
                  return B(_3j/* LudoJS.$sinsert_$sgo10 */(_1Q/* LudoJS.Red */, _rA/* shR7 */, _ry/* shQI */));
                });
                return new T5(0,_rx/* shQE */.a,_rx/* shQE */.b,_rx/* shQE */.c,_rz/* shR8 */,_rx/* shQE */.e);
              });
              return new T2(0,_eb/* GHC.Tuple.() */,_rw/* shR9 */);
            }
          },
          _rC/* shRg */ = E(_qT/* shPf */);
          if(_rC/* shRg */==3){
            return new F(function(){return _rt/* shQx */(_/* EXTERNAL */, new T2(0,_eb/* GHC.Tuple.() */,_rs/* shQw */));});
          }else{
            var _rD/* shRM */ = new T(function(){
              var _rE/* shRh */ = E(_rs/* shQw */),
              _rF/* shRl */ = _rE/* shRh */.d,
              _rG/* shRL */ = new T(function(){
                var _rH/* shRK */ = new T(function(){
                  return B(_qB/* LudoJS.outByCell */(B(_pX/* LudoJS.$s!_$spoly_go10 */(_rF/* shRl */)), new T(function(){
                    var _rI/* shRo */ = E(_qU/* shPg */);
                    switch(E(_rC/* shRg */)){
                      case 0:
                        return B(_np/* GHC.Classes.modInt# */(_rI/* shRo */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52));
                        break;
                      case 1:
                        return B(_np/* GHC.Classes.modInt# */(_rI/* shRo */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52));
                        break;
                      default:
                        return B(_np/* GHC.Classes.modInt# */(_rI/* shRo */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52));
                    }
                  },1)));
                });
                return B(_qb/* LudoJS.$sinsert_$s$sgo10 */(_rH/* shRK */, _rF/* shRl */));
              });
              return new T5(0,_rE/* shRh */.a,_rE/* shRh */.b,_rE/* shRh */.c,_rG/* shRL */,_rE/* shRh */.e);
            });
            return new F(function(){return _rt/* shQx */(_/* EXTERNAL */, new T2(0,_eb/* GHC.Tuple.() */,_rD/* shRM */));});
          }
        },
        _rJ/* shRP */ = E(_qT/* shPf */);
        if(_rJ/* shRP */==1){
          return new F(function(){return _rq/* shQt */(_/* EXTERNAL */, _eb/* GHC.Tuple.() */, _rp/* shQs */);});
        }else{
          var _rK/* shSl */ = new T(function(){
            var _rL/* shRQ */ = E(_rp/* shQs */),
            _rM/* shRU */ = _rL/* shRQ */.d,
            _rN/* shSk */ = new T(function(){
              var _rO/* shSj */ = new T(function(){
                return B(_qB/* LudoJS.outByCell */(B(_pT/* LudoJS.$s!_$spoly_go1 */(_rM/* shRU */)), new T(function(){
                  var _rP/* shRX */ = E(_qU/* shPg */);
                  switch(E(_rJ/* shRP */)){
                    case 0:
                      return B(_np/* GHC.Classes.modInt# */(_rP/* shRX */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52));
                      break;
                    case 2:
                      return B(_np/* GHC.Classes.modInt# */(_rP/* shRX */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52));
                      break;
                    default:
                      return B(_np/* GHC.Classes.modInt# */(_rP/* shRX */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52));
                  }
                },1)));
              });
              return B(_q4/* LudoJS.$sinsert_$s$sgo1 */(_rO/* shSj */, _rM/* shRU */));
            });
            return new T5(0,_rL/* shRQ */.a,_rL/* shRQ */.b,_rL/* shRQ */.c,_rN/* shSk */,_rL/* shRQ */.e);
          },1);
          return new F(function(){return _rq/* shQt */(_/* EXTERNAL */, _eb/* GHC.Tuple.() */, _rK/* shSl */);});
        }
      },
      _rQ/* shSm */ = E(_qT/* shPf */);
      if(!_rQ/* shSm */){
        return new F(function(){return _rn/* shQp */(_/* EXTERNAL */, _eb/* GHC.Tuple.() */, _rm/* shQn */);});
      }else{
        var _rR/* shSS */ = new T(function(){
          var _rS/* shSn */ = E(_rm/* shQn */),
          _rT/* shSr */ = _rS/* shSn */.d,
          _rU/* shSR */ = new T(function(){
            var _rV/* shSQ */ = new T(function(){
              return B(_qB/* LudoJS.outByCell */(B(_q0/* LudoJS.$s!_$spoly_go2 */(_rT/* shSr */)), new T(function(){
                var _rW/* shSu */ = E(_qU/* shPg */);
                switch(E(_rQ/* shSm */)){
                  case 1:
                    return B(_np/* GHC.Classes.modInt# */(_rW/* shSu */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52));
                    break;
                  case 2:
                    return B(_np/* GHC.Classes.modInt# */(_rW/* shSu */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52));
                    break;
                  default:
                    return B(_np/* GHC.Classes.modInt# */(_rW/* shSu */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52));
                }
              },1)));
            });
            return B(_qi/* LudoJS.$sinsert_$s$sgo2 */(_rV/* shSQ */, _rT/* shSr */));
          });
          return new T5(0,_rS/* shSn */.a,_rS/* shSn */.b,_rS/* shSn */.c,_rU/* shSR */,_rS/* shSn */.e);
        },1);
        return new F(function(){return _rn/* shQp */(_/* EXTERNAL */, _eb/* GHC.Tuple.() */, _rR/* shSS */);});
      }
    }else{
      var _rX/* shT2 */ = new T(function(){
        var _rY/* shST */ = E(_rm/* shQn */),
        _rZ/* shSX */ = _rY/* shST */.d,
        _s0/* shT1 */ = new T(function(){
          return B(_3j/* LudoJS.$sinsert_$sgo10 */(_qT/* shPf */, new T(function(){
            return B(_qB/* LudoJS.outByCell */(B(_da/* LudoJS.$s!1 */(_qT/* shPf */, _rZ/* shSX */)), _qU/* shPg */));
          }), _rZ/* shSX */));
        });
        return new T5(0,_rY/* shST */.a,_rY/* shST */.b,_rY/* shST */.c,_s0/* shT1 */,_rY/* shST */.e);
      });
      return new T2(0,_eb/* GHC.Tuple.() */,_rX/* shT2 */);
    }
  };
  switch(E(_qT/* shPf */)){
    case 0:
      var _s1/* shT7 */ = function(_s2/*  shU9 */, _s3/*  shUa */, _s4/*  shUb */, _/* EXTERNAL */){
        while(1){
          var _s5/*  shT7 */ = B((function(_s6/* shU9 */, _s7/* shUa */, _s8/* shUb */, _/* EXTERNAL */){
            var _s9/* shUd */ = E(_s6/* shU9 */);
            if(!_s9/* shUd */._){
              return new T2(0,_s7/* shUa */,_s8/* shUb */);
            }else{
              var _sa/* shUg */ = _s9/* shUd */.b,
              _sb/* shUh */ = E(_s9/* shUd */.a);
              if(!_sb/* shUh */){
                var _sc/*   shUa */ = _s7/* shUa */,
                _sd/*   shUb */ = _s8/* shUb */;
                _s2/*  shU9 */ = _sa/* shUg */;
                _s3/*  shUa */ = _sc/*   shUa */;
                _s4/*  shUb */ = _sd/*   shUb */;
                return __continue/* EXTERNAL */;
              }else{
                var _se/* shUH */ = new T(function(){
                  if(!E(_s7/* shUa */)){
                    var _sf/* shUp */ = function(_sg/*  shUq */){
                      while(1){
                        var _sh/*  shUp */ = B((function(_si/* shUq */){
                          var _sj/* shUr */ = E(_si/* shUq */);
                          if(!_sj/* shUr */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _sk/* shUt */ = _sj/* shUr */.b,
                            _sl/* shUx */ = E(E(_sj/* shUr */.a).b);
                            if(!_sl/* shUx */._){
                              _sg/*  shUq */ = _sk/* shUt */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_pP/* LudoJS.$wconvertCell */(_sb/* shUh */, E(_sl/* shUx */.a), _1O/* LudoJS.Blue */));
                              }),new T(function(){
                                return B(_sf/* shUp */(_sk/* shUt */));
                              }));
                            }
                          }
                        })(_sg/*  shUq */));
                        if(_sh/*  shUp */!=__continue/* EXTERNAL */){
                          return _sh/*  shUp */;
                        }
                      }
                    };
                    return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shPj */(B(_sf/* shUp */(B(_da/* LudoJS.$s!1 */(_sb/* shUh */, E(_s8/* shUb */).d)))))), _qy/* LudoJS.lvl10 */));
                  }else{
                    return true;
                  }
                }),
                _sd/*   shUb */ = _s8/* shUb */;
                _s2/*  shU9 */ = _sa/* shUg */;
                _s3/*  shUa */ = _se/* shUH */;
                _s4/*  shUb */ = _sd/*   shUb */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_s2/*  shU9 */, _s3/*  shUa */, _s4/*  shUb */, _/* EXTERNAL */));
          if(_s5/*  shT7 */!=__continue/* EXTERNAL */){
            return _s5/*  shT7 */;
          }
        }
      },
      _sm/* shT5 */ = function(_sn/*  shT8 */, _so/*  shT9 */, _/* EXTERNAL */){
        while(1){
          var _sp/*  shT5 */ = B((function(_sq/* shT8 */, _sr/* shT9 */, _/* EXTERNAL */){
            var _ss/* shTb */ = E(_sq/* shT8 */);
            if(!_ss/* shTb */._){
              return new T2(0,_qp/* GHC.Types.False */,_sr/* shT9 */);
            }else{
              var _st/* shTe */ = _ss/* shTb */.b,
              _su/* shTf */ = E(_ss/* shTb */.a);
              if(!_su/* shTf */){
                var _sv/*   shT9 */ = _sr/* shT9 */;
                _sn/*  shT8 */ = _st/* shTe */;
                _so/*  shT9 */ = _sv/*   shT9 */;
                return __continue/* EXTERNAL */;
              }else{
                var _sw/* shTE */ = new T(function(){
                  var _sx/* shTm */ = function(_sy/*  shTn */){
                    while(1){
                      var _sz/*  shTm */ = B((function(_sA/* shTn */){
                        var _sB/* shTo */ = E(_sA/* shTn */);
                        if(!_sB/* shTo */._){
                          return __Z/* EXTERNAL */;
                        }else{
                          var _sC/* shTq */ = _sB/* shTo */.b,
                          _sD/* shTu */ = E(E(_sB/* shTo */.a).b);
                          if(!_sD/* shTu */._){
                            _sy/*  shTn */ = _sC/* shTq */;
                            return __continue/* EXTERNAL */;
                          }else{
                            return new T2(1,new T(function(){
                              return B(_pP/* LudoJS.$wconvertCell */(_su/* shTf */, E(_sD/* shTu */.a), _1O/* LudoJS.Blue */));
                            }),new T(function(){
                              return B(_sx/* shTm */(_sC/* shTq */));
                            }));
                          }
                        }
                      })(_sy/*  shTn */));
                      if(_sz/*  shTm */!=__continue/* EXTERNAL */){
                        return _sz/*  shTm */;
                      }
                    }
                  };
                  return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shPj */(B(_sx/* shTm */(B(_da/* LudoJS.$s!1 */(_su/* shTf */, E(_sr/* shT9 */).d)))))), _qy/* LudoJS.lvl10 */));
                });
                return new F(function(){return _s1/* shT7 */(_st/* shTe */, _sw/* shTE */, _sr/* shT9 */, _/* EXTERNAL */);});
              }
            }
          })(_sn/*  shT8 */, _so/*  shT9 */, _/* EXTERNAL */));
          if(_sp/*  shT5 */!=__continue/* EXTERNAL */){
            return _sp/*  shT5 */;
          }
        }
      },
      _sE/* shT6 */ = function(_sF/* shTF */, _sG/* shTG */, _sH/* shTH */, _/* EXTERNAL */){
        var _sI/* shTJ */ = E(_sF/* shTF */);
        if(!_sI/* shTJ */){
          return new F(function(){return _sm/* shT5 */(_sG/* shTG */, _sH/* shTH */, _/* EXTERNAL */);});
        }else{
          var _sJ/* shU8 */ = new T(function(){
            var _sK/* shTQ */ = function(_sL/*  shTR */){
              while(1){
                var _sM/*  shTQ */ = B((function(_sN/* shTR */){
                  var _sO/* shTS */ = E(_sN/* shTR */);
                  if(!_sO/* shTS */._){
                    return __Z/* EXTERNAL */;
                  }else{
                    var _sP/* shTU */ = _sO/* shTS */.b,
                    _sQ/* shTY */ = E(E(_sO/* shTS */.a).b);
                    if(!_sQ/* shTY */._){
                      _sL/*  shTR */ = _sP/* shTU */;
                      return __continue/* EXTERNAL */;
                    }else{
                      return new T2(1,new T(function(){
                        return B(_pP/* LudoJS.$wconvertCell */(_sI/* shTJ */, E(_sQ/* shTY */.a), _1O/* LudoJS.Blue */));
                      }),new T(function(){
                        return B(_sK/* shTQ */(_sP/* shTU */));
                      }));
                    }
                  }
                })(_sL/*  shTR */));
                if(_sM/*  shTQ */!=__continue/* EXTERNAL */){
                  return _sM/*  shTQ */;
                }
              }
            };
            return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shPj */(B(_sK/* shTQ */(B(_da/* LudoJS.$s!1 */(_sI/* shTJ */, E(_sH/* shTH */).d)))))), _qy/* LudoJS.lvl10 */));
          });
          return new F(function(){return _s1/* shT7 */(_sG/* shTG */, _sJ/* shU8 */, _sH/* shTH */, _/* EXTERNAL */);});
        }
      },
      _sR/* shUI */ = B(_sE/* shT6 */(_1P/* LudoJS.Green */, _6b/* LudoJS.lvl8 */, _qV/* shPh */, _/* EXTERNAL */));
      return new F(function(){return _rj/* shQi */(_/* EXTERNAL */, _sR/* shUI */);});
      break;
    case 1:
      var _sS/* shUL */ = B(_r9/* shPP */(_qV/* shPh */, _/* EXTERNAL */)),
      _sT/* shUP */ = function(_sU/*  shUU */, _sV/*  shUV */, _sW/*  shUW */, _/* EXTERNAL */){
        while(1){
          var _sX/*  shUP */ = B((function(_sY/* shUU */, _sZ/* shUV */, _t0/* shUW */, _/* EXTERNAL */){
            var _t1/* shUY */ = E(_sY/* shUU */);
            if(!_t1/* shUY */._){
              return new T2(0,_sZ/* shUV */,_t0/* shUW */);
            }else{
              var _t2/* shV1 */ = _t1/* shUY */.b,
              _t3/* shV2 */ = E(_t1/* shUY */.a);
              if(_t3/* shV2 */==1){
                var _t4/*   shUV */ = _sZ/* shUV */,
                _t5/*   shUW */ = _t0/* shUW */;
                _sU/*  shUU */ = _t2/* shV1 */;
                _sV/*  shUV */ = _t4/*   shUV */;
                _sW/*  shUW */ = _t5/*   shUW */;
                return __continue/* EXTERNAL */;
              }else{
                var _t6/* shVs */ = new T(function(){
                  if(!E(_sZ/* shUV */)){
                    var _t7/* shVa */ = function(_t8/*  shVb */){
                      while(1){
                        var _t9/*  shVa */ = B((function(_ta/* shVb */){
                          var _tb/* shVc */ = E(_ta/* shVb */);
                          if(!_tb/* shVc */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _tc/* shVe */ = _tb/* shVc */.b,
                            _td/* shVi */ = E(E(_tb/* shVc */.a).b);
                            if(!_td/* shVi */._){
                              _t8/*  shVb */ = _tc/* shVe */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_pP/* LudoJS.$wconvertCell */(_t3/* shV2 */, E(_td/* shVi */.a), _1P/* LudoJS.Green */));
                              }),new T(function(){
                                return B(_t7/* shVa */(_tc/* shVe */));
                              }));
                            }
                          }
                        })(_t8/*  shVb */));
                        if(_t9/*  shVa */!=__continue/* EXTERNAL */){
                          return _t9/*  shVa */;
                        }
                      }
                    };
                    return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shPj */(B(_t7/* shVa */(B(_da/* LudoJS.$s!1 */(_t3/* shV2 */, E(_t0/* shUW */).d)))))), _qy/* LudoJS.lvl10 */));
                  }else{
                    return true;
                  }
                }),
                _t5/*   shUW */ = _t0/* shUW */;
                _sU/*  shUU */ = _t2/* shV1 */;
                _sV/*  shUV */ = _t6/* shVs */;
                _sW/*  shUW */ = _t5/*   shUW */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_sU/*  shUU */, _sV/*  shUV */, _sW/*  shUW */, _/* EXTERNAL */));
          if(_sX/*  shUP */!=__continue/* EXTERNAL */){
            return _sX/*  shUP */;
          }
        }
      },
      _te/* shVB */ = B((function(_tf/* shUQ */, _tg/* shUR */, _th/* shUS */, _/* EXTERNAL */){
        return new F(function(){return _sT/* shUP */(_tf/* shUQ */, _tg/* shUR */, _th/* shUS */, _/* EXTERNAL */);});
      })(_6b/* LudoJS.lvl8 */, new T(function(){
        return E(E(_sS/* shUL */).a);
      }), new T(function(){
        return E(E(_sS/* shUL */).b);
      }), _/* EXTERNAL */));
      return new F(function(){return _rj/* shQi */(_/* EXTERNAL */, _te/* shVB */);});
      break;
    case 2:
      var _ti/* shVE */ = B(_r9/* shPP */(_qV/* shPh */, _/* EXTERNAL */)),
      _tj/* shVI */ = function(_tk/*  shWf */, _tl/*  shWg */, _tm/*  shWh */, _/* EXTERNAL */){
        while(1){
          var _tn/*  shVI */ = B((function(_to/* shWf */, _tp/* shWg */, _tq/* shWh */, _/* EXTERNAL */){
            var _tr/* shWj */ = E(_to/* shWf */);
            if(!_tr/* shWj */._){
              return new T2(0,_tp/* shWg */,_tq/* shWh */);
            }else{
              var _ts/* shWm */ = _tr/* shWj */.b,
              _tt/* shWn */ = E(_tr/* shWj */.a);
              if(_tt/* shWn */==2){
                var _tu/*   shWg */ = _tp/* shWg */,
                _tv/*   shWh */ = _tq/* shWh */;
                _tk/*  shWf */ = _ts/* shWm */;
                _tl/*  shWg */ = _tu/*   shWg */;
                _tm/*  shWh */ = _tv/*   shWh */;
                return __continue/* EXTERNAL */;
              }else{
                var _tw/* shWN */ = new T(function(){
                  if(!E(_tp/* shWg */)){
                    var _tx/* shWv */ = function(_ty/*  shWw */){
                      while(1){
                        var _tz/*  shWv */ = B((function(_tA/* shWw */){
                          var _tB/* shWx */ = E(_tA/* shWw */);
                          if(!_tB/* shWx */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _tC/* shWz */ = _tB/* shWx */.b,
                            _tD/* shWD */ = E(E(_tB/* shWx */.a).b);
                            if(!_tD/* shWD */._){
                              _ty/*  shWw */ = _tC/* shWz */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_pP/* LudoJS.$wconvertCell */(_tt/* shWn */, E(_tD/* shWD */.a), _1Q/* LudoJS.Red */));
                              }),new T(function(){
                                return B(_tx/* shWv */(_tC/* shWz */));
                              }));
                            }
                          }
                        })(_ty/*  shWw */));
                        if(_tz/*  shWv */!=__continue/* EXTERNAL */){
                          return _tz/*  shWv */;
                        }
                      }
                    };
                    return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shPj */(B(_tx/* shWv */(B(_da/* LudoJS.$s!1 */(_tt/* shWn */, E(_tq/* shWh */).d)))))), _qy/* LudoJS.lvl10 */));
                  }else{
                    return true;
                  }
                }),
                _tv/*   shWh */ = _tq/* shWh */;
                _tk/*  shWf */ = _ts/* shWm */;
                _tl/*  shWg */ = _tw/* shWN */;
                _tm/*  shWh */ = _tv/*   shWh */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_tk/*  shWf */, _tl/*  shWg */, _tm/*  shWh */, _/* EXTERNAL */));
          if(_tn/*  shVI */!=__continue/* EXTERNAL */){
            return _tn/*  shVI */;
          }
        }
      },
      _tE/* shVH */ = function(_tF/* shVJ */, _tG/* shVK */, _tH/* shVL */, _tI/* shVM */, _/* EXTERNAL */){
        var _tJ/* shVO */ = E(_tF/* shVJ */);
        if(_tJ/* shVO */==2){
          return new F(function(){return _tj/* shVI */(_tG/* shVK */, _tH/* shVL */, _tI/* shVM */, _/* EXTERNAL */);});
        }else{
          var _tK/* shWe */ = new T(function(){
            if(!E(_tH/* shVL */)){
              var _tL/* shVW */ = function(_tM/*  shVX */){
                while(1){
                  var _tN/*  shVW */ = B((function(_tO/* shVX */){
                    var _tP/* shVY */ = E(_tO/* shVX */);
                    if(!_tP/* shVY */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _tQ/* shW0 */ = _tP/* shVY */.b,
                      _tR/* shW4 */ = E(E(_tP/* shVY */.a).b);
                      if(!_tR/* shW4 */._){
                        _tM/*  shVX */ = _tQ/* shW0 */;
                        return __continue/* EXTERNAL */;
                      }else{
                        return new T2(1,new T(function(){
                          return B(_pP/* LudoJS.$wconvertCell */(_tJ/* shVO */, E(_tR/* shW4 */.a), _1Q/* LudoJS.Red */));
                        }),new T(function(){
                          return B(_tL/* shVW */(_tQ/* shW0 */));
                        }));
                      }
                    }
                  })(_tM/*  shVX */));
                  if(_tN/*  shVW */!=__continue/* EXTERNAL */){
                    return _tN/*  shVW */;
                  }
                }
              };
              return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shPj */(B(_tL/* shVW */(B(_da/* LudoJS.$s!1 */(_tJ/* shVO */, E(_tI/* shVM */).d)))))), _qy/* LudoJS.lvl10 */));
            }else{
              return true;
            }
          });
          return new F(function(){return _tj/* shVI */(_tG/* shVK */, _tK/* shWe */, _tI/* shVM */, _/* EXTERNAL */);});
        }
      },
      _tS/* shWW */ = B(_tE/* shVH */(_1P/* LudoJS.Green */, _6b/* LudoJS.lvl8 */, new T(function(){
        return E(E(_ti/* shVE */).a);
      }), new T(function(){
        return E(E(_ti/* shVE */).b);
      }), _/* EXTERNAL */));
      return new F(function(){return _rj/* shQi */(_/* EXTERNAL */, _tS/* shWW */);});
      break;
    default:
      var _tT/* shWZ */ = B(_r9/* shPP */(_qV/* shPh */, _/* EXTERNAL */)),
      _tU/* shX3 */ = function(_tV/*  shXA */, _tW/*  shXB */, _tX/*  shXC */, _/* EXTERNAL */){
        while(1){
          var _tY/*  shX3 */ = B((function(_tZ/* shXA */, _u0/* shXB */, _u1/* shXC */, _/* EXTERNAL */){
            var _u2/* shXE */ = E(_tZ/* shXA */);
            if(!_u2/* shXE */._){
              return new T2(0,_u0/* shXB */,_u1/* shXC */);
            }else{
              var _u3/* shXH */ = _u2/* shXE */.b,
              _u4/* shXI */ = E(_u2/* shXE */.a);
              if(_u4/* shXI */==3){
                var _u5/*   shXB */ = _u0/* shXB */,
                _u6/*   shXC */ = _u1/* shXC */;
                _tV/*  shXA */ = _u3/* shXH */;
                _tW/*  shXB */ = _u5/*   shXB */;
                _tX/*  shXC */ = _u6/*   shXC */;
                return __continue/* EXTERNAL */;
              }else{
                var _u7/* shY8 */ = new T(function(){
                  if(!E(_u0/* shXB */)){
                    var _u8/* shXQ */ = function(_u9/*  shXR */){
                      while(1){
                        var _ua/*  shXQ */ = B((function(_ub/* shXR */){
                          var _uc/* shXS */ = E(_ub/* shXR */);
                          if(!_uc/* shXS */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _ud/* shXU */ = _uc/* shXS */.b,
                            _ue/* shXY */ = E(E(_uc/* shXS */.a).b);
                            if(!_ue/* shXY */._){
                              _u9/*  shXR */ = _ud/* shXU */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_pP/* LudoJS.$wconvertCell */(_u4/* shXI */, E(_ue/* shXY */.a), _1S/* LudoJS.Yellow */));
                              }),new T(function(){
                                return B(_u8/* shXQ */(_ud/* shXU */));
                              }));
                            }
                          }
                        })(_u9/*  shXR */));
                        if(_ua/*  shXQ */!=__continue/* EXTERNAL */){
                          return _ua/*  shXQ */;
                        }
                      }
                    };
                    return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shPj */(B(_u8/* shXQ */(B(_da/* LudoJS.$s!1 */(_u4/* shXI */, E(_u1/* shXC */).d)))))), _qy/* LudoJS.lvl10 */));
                  }else{
                    return true;
                  }
                }),
                _u6/*   shXC */ = _u1/* shXC */;
                _tV/*  shXA */ = _u3/* shXH */;
                _tW/*  shXB */ = _u7/* shY8 */;
                _tX/*  shXC */ = _u6/*   shXC */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_tV/*  shXA */, _tW/*  shXB */, _tX/*  shXC */, _/* EXTERNAL */));
          if(_tY/*  shX3 */!=__continue/* EXTERNAL */){
            return _tY/*  shX3 */;
          }
        }
      },
      _uf/* shX2 */ = function(_ug/* shX4 */, _uh/* shX5 */, _ui/* shX6 */, _uj/* shX7 */, _/* EXTERNAL */){
        var _uk/* shX9 */ = E(_ug/* shX4 */);
        if(_uk/* shX9 */==3){
          return new F(function(){return _tU/* shX3 */(_uh/* shX5 */, _ui/* shX6 */, _uj/* shX7 */, _/* EXTERNAL */);});
        }else{
          var _ul/* shXz */ = new T(function(){
            if(!E(_ui/* shX6 */)){
              var _um/* shXh */ = function(_un/*  shXi */){
                while(1){
                  var _uo/*  shXh */ = B((function(_up/* shXi */){
                    var _uq/* shXj */ = E(_up/* shXi */);
                    if(!_uq/* shXj */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _ur/* shXl */ = _uq/* shXj */.b,
                      _us/* shXp */ = E(E(_uq/* shXj */.a).b);
                      if(!_us/* shXp */._){
                        _un/*  shXi */ = _ur/* shXl */;
                        return __continue/* EXTERNAL */;
                      }else{
                        return new T2(1,new T(function(){
                          return B(_pP/* LudoJS.$wconvertCell */(_uk/* shX9 */, E(_us/* shXp */.a), _1S/* LudoJS.Yellow */));
                        }),new T(function(){
                          return B(_um/* shXh */(_ur/* shXl */));
                        }));
                      }
                    }
                  })(_un/*  shXi */));
                  if(_uo/*  shXh */!=__continue/* EXTERNAL */){
                    return _uo/*  shXh */;
                  }
                }
              };
              return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shPj */(B(_um/* shXh */(B(_da/* LudoJS.$s!1 */(_uk/* shX9 */, E(_uj/* shX7 */).d)))))), _qy/* LudoJS.lvl10 */));
            }else{
              return true;
            }
          });
          return new F(function(){return _tU/* shX3 */(_uh/* shX5 */, _ul/* shXz */, _uj/* shX7 */, _/* EXTERNAL */);});
        }
      },
      _ut/* shYh */ = B(_uf/* shX2 */(_1P/* LudoJS.Green */, _6b/* LudoJS.lvl8 */, new T(function(){
        return E(E(_tT/* shWZ */).a);
      }), new T(function(){
        return E(E(_tT/* shWZ */).b);
      }), _/* EXTERNAL */));
      return new F(function(){return _rj/* shQi */(_/* EXTERNAL */, _ut/* shYh */);});
  }
},
_uu/* == */ = function(_uv/* scBK */){
  return E(E(_uv/* scBK */).a);
},
_uw/* elem */ = function(_ux/* sbGb */, _uy/* sbGc */, _uz/* sbGd */){
  while(1){
    var _uA/* sbGe */ = E(_uz/* sbGd */);
    if(!_uA/* sbGe */._){
      return false;
    }else{
      if(!B(A3(_uu/* GHC.Classes.== */,_ux/* sbGb */, _uy/* sbGc */, _uA/* sbGe */.a))){
        _uz/* sbGd */ = _uA/* sbGe */.b;
        continue;
      }else{
        return true;
      }
    }
  }
},
_uB/* findIndex */ = function(_uC/* s1ZJc */, _uD/* s1ZJd */){
  var _uE/* s1ZJe */ = function(_uF/*  s1ZJf */, _uG/*  s1ZJg */){
    while(1){
      var _uH/*  s1ZJe */ = B((function(_uI/* s1ZJf */, _uJ/* s1ZJg */){
        var _uK/* s1ZJh */ = E(_uI/* s1ZJf */);
        if(!_uK/* s1ZJh */._){
          return __Z/* EXTERNAL */;
        }else{
          var _uL/* s1ZJj */ = _uK/* s1ZJh */.b;
          if(!B(A1(_uC/* s1ZJc */,_uK/* s1ZJh */.a))){
            var _uM/*   s1ZJg */ = _uJ/* s1ZJg */+1|0;
            _uF/*  s1ZJf */ = _uL/* s1ZJj */;
            _uG/*  s1ZJg */ = _uM/*   s1ZJg */;
            return __continue/* EXTERNAL */;
          }else{
            return new T2(1,_uJ/* s1ZJg */,new T(function(){
              return B(_uE/* s1ZJe */(_uL/* s1ZJj */, _uJ/* s1ZJg */+1|0));
            }));
          }
        }
      })(_uF/*  s1ZJf */, _uG/*  s1ZJg */));
      if(_uH/*  s1ZJe */!=__continue/* EXTERNAL */){
        return _uH/*  s1ZJe */;
      }
    }
  },
  _uN/* s1ZJp */ = B(_uE/* s1ZJe */(_uD/* s1ZJd */, 0));
  return (_uN/* s1ZJp */._==0) ? __Z/* EXTERNAL */ : new T1(1,_uN/* s1ZJp */.a);
},
_uO/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Maybe.fromJust: Nothing"));
}),
_uP/* fromJust1 */ = new T(function(){
  return B(err/* EXTERNAL */(_uO/* Data.Maybe.lvl */));
}),
_uQ/* a19 */ = 47,
_uR/* a20 */ = new T2(1,_uQ/* LudoJS.a19 */,_4/* GHC.Types.[] */),
_uS/* a21 */ = 39,
_uT/* a22 */ = new T2(1,_uS/* LudoJS.a21 */,_uR/* LudoJS.a20 */),
_uU/* a23 */ = 34,
_uV/* a24 */ = new T2(1,_uU/* LudoJS.a23 */,_uT/* LudoJS.a22 */),
_uW/* a25 */ = 26,
_uX/* a26 */ = new T2(1,_uW/* LudoJS.a25 */,_uV/* LudoJS.a24 */),
_uY/* a27 */ = 21,
_uZ/* a28 */ = new T2(1,_uY/* LudoJS.a27 */,_uX/* LudoJS.a26 */),
_v0/* a29 */ = 13,
_v1/* a30 */ = new T2(1,_v0/* LudoJS.a29 */,_uZ/* LudoJS.a28 */),
_v2/* a31 */ = 8,
_v3/* globeCells */ = new T2(1,_v2/* LudoJS.a31 */,_v1/* LudoJS.a30 */),
_v4/* lvl3 */ = 56,
_v5/* lvl4 */ = new T2(1,_v4/* LudoJS.lvl3 */,_4/* GHC.Types.[] */),
_v6/* $fShowStage8 */ = 11,
_v7/* a5 */ = 50,
_v8/* a6 */ = new T2(1,_v7/* LudoJS.a5 */,_4/* GHC.Types.[] */),
_v9/* a7 */ = 44,
_va/* a8 */ = new T2(1,_v9/* LudoJS.a7 */,_v8/* LudoJS.a6 */),
_vb/* a9 */ = 37,
_vc/* a10 */ = new T2(1,_vb/* LudoJS.a9 */,_va/* LudoJS.a8 */),
_vd/* a11 */ = 31,
_ve/* a12 */ = new T2(1,_vd/* LudoJS.a11 */,_vc/* LudoJS.a10 */),
_vf/* a13 */ = 24,
_vg/* a14 */ = new T2(1,_vf/* LudoJS.a13 */,_ve/* LudoJS.a12 */),
_vh/* a15 */ = 18,
_vi/* a16 */ = new T2(1,_vh/* LudoJS.a15 */,_vg/* LudoJS.a14 */),
_vj/* a17 */ = new T2(1,_v6/* LudoJS.$fShowStage8 */,_vi/* LudoJS.a16 */),
_vk/* a18 */ = 5,
_vl/* starCells */ = new T2(1,_vk/* LudoJS.a18 */,_vj/* LudoJS.a17 */),
_vm/* lvl5 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_vl/* LudoJS.starCells */, _v5/* LudoJS.lvl4 */));
}),
_vn/* lvl6 */ = new T(function(){
  return B(_mL/* GHC.List.$wlenAcc */(_vl/* LudoJS.starCells */, 0));
}),
_vo/* $wa7 */ = function(_vp/* shYk */, _vq/* shYl */, _vr/* shYm */, _/* EXTERNAL */){
  var _vs/* shYo */ = new T(function(){
    return E(E(_vr/* shYm */).b);
  }),
  _vt/* shYv */ = new T(function(){
    return E(E(_vr/* shYm */).d);
  }),
  _vu/* shYC */ = E(_vq/* shYl */);
  if(_vu/* shYC */==56){
    var _vv/* si2Y */ = new T(function(){
      var _vw/* si2D */ = E(_vr/* shYm */),
      _vx/* si2X */ = new T(function(){
        var _vy/* si2W */ = new T(function(){
          var _vz/* si2J */ = B(_da/* LudoJS.$s!1 */(_vs/* shYo */, _vt/* shYv */));
          if(!_vz/* si2J */._){
            return __Z/* EXTERNAL */;
          }else{
            var _vA/* si2L */ = _vz/* si2J */.b,
            _vB/* si2M */ = E(_vz/* si2J */.a),
            _vC/* si2P */ = E(_vp/* shYk */);
            if(_vC/* si2P */!=E(_vB/* si2M */.a)){
              return new T2(1,_vB/* si2M */,new T(function(){
                return B(_pu/* LudoJS.$sremoveFrom */(_vA/* si2L */, _vC/* si2P */));
              }));
            }else{
              return E(_vA/* si2L */);
            }
          }
        });
        return B(_3j/* LudoJS.$sinsert_$sgo10 */(_vs/* shYo */, _vy/* si2W */, _vt/* shYv */));
      });
      return new T5(0,_vw/* si2D */.a,_vw/* si2D */.b,_vw/* si2D */.c,_vx/* si2X */,_vw/* si2D */.e);
    });
    return new T2(0,_eb/* GHC.Tuple.() */,_vv/* si2Y */);
  }else{
    if(!B(_uw/* GHC.List.elem */(_pm/* GHC.Classes.$fEqInt */, _vu/* shYC */, _vl/* LudoJS.starCells */))){
      if(!B(_uw/* GHC.List.elem */(_pm/* GHC.Classes.$fEqInt */, _vu/* shYC */, _v3/* LudoJS.globeCells */))){
        if(_vu/* shYC */<51){
          var _vD/* shZ6 */ = new T(function(){
            var _vE/* shYI */ = E(_vr/* shYm */),
            _vF/* shZ5 */ = new T(function(){
              var _vG/* shZ3 */ = new T(function(){
                var _vH/* shYQ */ = B(_da/* LudoJS.$s!1 */(_vs/* shYo */, _vt/* shYv */));
                if(!_vH/* shYQ */._){
                  return __Z/* EXTERNAL */;
                }else{
                  var _vI/* shYS */ = _vH/* shYQ */.b,
                  _vJ/* shYT */ = E(_vH/* shYQ */.a),
                  _vK/* shYW */ = E(_vp/* shYk */);
                  if(_vK/* shYW */!=E(_vJ/* shYT */.a)){
                    return new T2(1,_vJ/* shYT */,new T(function(){
                      return B(_pu/* LudoJS.$sremoveFrom */(_vI/* shYS */, _vK/* shYW */));
                    }));
                  }else{
                    return E(_vI/* shYS */);
                  }
                }
              });
              return B(_3j/* LudoJS.$sinsert_$sgo10 */(_vs/* shYo */, new T2(1,new T2(0,_vp/* shYk */,new T1(1,_vu/* shYC */)),_vG/* shZ3 */), _vt/* shYv */));
            });
            return new T5(0,_vE/* shYI */.a,_vE/* shYI */.b,_vE/* shYI */.c,_vF/* shZ5 */,_vE/* shYI */.e);
          });
          return new F(function(){return _qS/* LudoJS.a43 */(_vs/* shYo */, _vu/* shYC */, _vD/* shZ6 */, _/* EXTERNAL */);});
        }else{
          var _vL/* shZv */ = new T(function(){
            var _vM/* shZ7 */ = E(_vr/* shYm */),
            _vN/* shZu */ = new T(function(){
              var _vO/* shZs */ = new T(function(){
                var _vP/* shZf */ = B(_da/* LudoJS.$s!1 */(_vs/* shYo */, _vt/* shYv */));
                if(!_vP/* shZf */._){
                  return __Z/* EXTERNAL */;
                }else{
                  var _vQ/* shZh */ = _vP/* shZf */.b,
                  _vR/* shZi */ = E(_vP/* shZf */.a),
                  _vS/* shZl */ = E(_vp/* shYk */);
                  if(_vS/* shZl */!=E(_vR/* shZi */.a)){
                    return new T2(1,_vR/* shZi */,new T(function(){
                      return B(_pu/* LudoJS.$sremoveFrom */(_vQ/* shZh */, _vS/* shZl */));
                    }));
                  }else{
                    return E(_vQ/* shZh */);
                  }
                }
              });
              return B(_3j/* LudoJS.$sinsert_$sgo10 */(_vs/* shYo */, new T2(1,new T2(0,_vp/* shYk */,new T1(1,_vu/* shYC */)),_vO/* shZs */), _vt/* shYv */));
            });
            return new T5(0,_vM/* shZ7 */.a,_vM/* shZ7 */.b,_vM/* shZ7 */.c,_vN/* shZu */,_vM/* shZ7 */.e);
          });
          return new T2(0,_eb/* GHC.Tuple.() */,_vL/* shZv */);
        }
      }else{
        var _vT/* shZx */ = E(_vr/* shYm */),
        _vU/* shZy */ = _vT/* shZx */.a,
        _vV/* shZz */ = _vT/* shZx */.b,
        _vW/* shZA */ = _vT/* shZx */.c,
        _vX/* shZC */ = _vT/* shZx */.e,
        _vY/* shZD */ = function(_vZ/* shZE */, _w0/* shZF */, _w1/* shZG */, _w2/* shZH */, _w3/* shZI */, _w4/* shZJ */, _/* EXTERNAL */){
          var _w5/* shZL */ = new T(function(){
            return B(_pP/* LudoJS.$wconvertCell */(_w1/* shZG */, _vu/* shYC */, _vZ/* shZE */));
          }),
          _w6/* shZN */ = function(_w7/*  shZO */){
            while(1){
              var _w8/*  shZN */ = B((function(_w9/* shZO */){
                var _wa/* shZP */ = E(_w9/* shZO */);
                if(!_wa/* shZP */._){
                  return false;
                }else{
                  var _wb/* shZR */ = _wa/* shZP */.b,
                  _wc/* shZV */ = E(E(_wa/* shZP */.a).b);
                  if(!_wc/* shZV */._){
                    _w7/*  shZO */ = _wb/* shZR */;
                    return __continue/* EXTERNAL */;
                  }else{
                    var _wd/* shZX */ = E(_w5/* shZL */);
                    if(_wd/* shZX */!=E(_wc/* shZV */.a)){
                      var _we/* si03 */ = function(_wf/* si04 */){
                        while(1){
                          var _wg/* si05 */ = E(_wf/* si04 */);
                          if(!_wg/* si05 */._){
                            return false;
                          }else{
                            var _wh/* si07 */ = _wg/* si05 */.b,
                            _wi/* si0b */ = E(E(_wg/* si05 */.a).b);
                            if(!_wi/* si0b */._){
                              _wf/* si04 */ = _wh/* si07 */;
                              continue;
                            }else{
                              if(_wd/* shZX */!=E(_wi/* si0b */.a)){
                                _wf/* si04 */ = _wh/* si07 */;
                                continue;
                              }else{
                                return true;
                              }
                            }
                          }
                        }
                      };
                      return new F(function(){return _we/* si03 */(_wb/* shZR */);});
                    }else{
                      return true;
                    }
                  }
                }
              })(_w7/*  shZO */));
              if(_w8/*  shZN */!=__continue/* EXTERNAL */){
                return _w8/*  shZN */;
              }
            }
          };
          if(!B(_w6/* shZN */(B(_da/* LudoJS.$s!1 */(_vZ/* shZE */, _w3/* shZI */))))){
            return new T2(0,_eb/* GHC.Tuple.() */,new T5(0,_w0/* shZF */,_w1/* shZG */,_w2/* shZH */,_w3/* shZI */,_w4/* shZJ */));
          }else{
            var _wj/* si0n */ = new T(function(){
              return B(_3j/* LudoJS.$sinsert_$sgo10 */(_w1/* shZG */, new T(function(){
                return B(_pn/* LudoJS.$soutByCell */(B(_da/* LudoJS.$s!1 */(_w1/* shZG */, _w3/* shZI */)), _vu/* shYC */));
              }), _w3/* shZI */));
            });
            return new T2(0,_eb/* GHC.Tuple.() */,new T5(0,_w0/* shZF */,_w1/* shZG */,_w2/* shZH */,_wj/* si0n */,_w4/* shZJ */));
          }
        },
        _wk/* si0q */ = function(_wl/* si0r */, _wm/* si0s */, _wn/* si0t */, _wo/* si0u */, _wp/* si0v */, _/* EXTERNAL */){
          var _wq/* si0x */ = function(_wr/* si0y */, _ws/* si0z */, _wt/* si0A */, _wu/* si0B */, _wv/* si0C */, _/* EXTERNAL */){
            var _ww/* si0E */ = E(_vs/* shYo */);
            if(_ww/* si0E */==3){
              return new F(function(){return _vY/* shZD */(_1Q/* LudoJS.Red */, _wr/* si0y */, _ws/* si0z */, _wt/* si0A */, _wu/* si0B */, _wv/* si0C */, _/* EXTERNAL */);});
            }else{
              var _wx/* si0F */ = B(_vY/* shZD */(_1S/* LudoJS.Yellow */, _wr/* si0y */, _ws/* si0z */, _wt/* si0A */, _wu/* si0B */, _wv/* si0C */, _/* EXTERNAL */));
              if(E(_ww/* si0E */)==2){
                return new T2(0,_eb/* GHC.Tuple.() */,new T(function(){
                  return E(E(_wx/* si0F */).b);
                }));
              }else{
                var _wy/* si0M */ = E(E(_wx/* si0F */).b);
                return new F(function(){return _vY/* shZD */(_1Q/* LudoJS.Red */, _wy/* si0M */.a, _wy/* si0M */.b, _wy/* si0M */.c, _wy/* si0M */.d, _wy/* si0M */.e, _/* EXTERNAL */);});
              }
            }
          };
          if(E(_vs/* shYo */)==1){
            return new F(function(){return _wq/* si0x */(_wl/* si0r */, _wm/* si0s */, _wn/* si0t */, _wo/* si0u */, _wp/* si0v */, _/* EXTERNAL */);});
          }else{
            var _wz/* si0Y */ = B(_vY/* shZD */(_1P/* LudoJS.Green */, _wl/* si0r */, _wm/* si0s */, _wn/* si0t */, _wo/* si0u */, _wp/* si0v */, _/* EXTERNAL */)),
            _wA/* si14 */ = E(E(_wz/* si0Y */).b);
            return new F(function(){return _wq/* si0x */(_wA/* si14 */.a, _wA/* si14 */.b, _wA/* si14 */.c, _wA/* si14 */.d, _wA/* si14 */.e, _/* EXTERNAL */);});
          }
        },
        _wB/* si1a */ = new T(function(){
          var _wC/* si1q */ = new T(function(){
            var _wD/* si1d */ = B(_da/* LudoJS.$s!1 */(_vs/* shYo */, _vt/* shYv */));
            if(!_wD/* si1d */._){
              return __Z/* EXTERNAL */;
            }else{
              var _wE/* si1f */ = _wD/* si1d */.b,
              _wF/* si1g */ = E(_wD/* si1d */.a),
              _wG/* si1j */ = E(_vp/* shYk */);
              if(_wG/* si1j */!=E(_wF/* si1g */.a)){
                return new T2(1,_wF/* si1g */,new T(function(){
                  return B(_pu/* LudoJS.$sremoveFrom */(_wE/* si1f */, _wG/* si1j */));
                }));
              }else{
                return E(_wE/* si1f */);
              }
            }
          });
          return B(_3j/* LudoJS.$sinsert_$sgo10 */(_vs/* shYo */, new T2(1,new T2(0,_vp/* shYk */,new T1(1,_vu/* shYC */)),_wC/* si1q */), _vt/* shYv */));
        });
        if(!E(_vs/* shYo */)){
          return new F(function(){return _wk/* si0q */(_vU/* shZy */, _vV/* shZz */, _vW/* shZA */, _wB/* si1a */, _vX/* shZC */, _/* EXTERNAL */);});
        }else{
          var _wH/* si1t */ = B(_vY/* shZD */(_1O/* LudoJS.Blue */, _vU/* shZy */, _vV/* shZz */, _vW/* shZA */, _wB/* si1a */, _vX/* shZC */, _/* EXTERNAL */)),
          _wI/* si1z */ = E(E(_wH/* si1t */).b);
          return new F(function(){return _wk/* si0q */(_wI/* si1z */.a, _wI/* si1z */.b, _wI/* si1z */.c, _wI/* si1z */.d, _wI/* si1z */.e, _/* EXTERNAL */);});
        }
      }
    }else{
      var _wJ/* si1F */ = new T(function(){
        var _wK/* si1H */ = B(_uB/* Data.OldList.findIndex */(function(_wL/* B1 */){
          return new F(function(){return _mx/* GHC.Classes.eqInt */(_vu/* shYC */, _wL/* B1 */);});
        }, _vl/* LudoJS.starCells */));
        if(!_wK/* si1H */._){
          return E(_uP/* Data.Maybe.fromJust1 */);
        }else{
          return E(_wK/* si1H */.a);
        }
      }),
      _wM/* si1J */ = new T(function(){
        return B(_pM/* GHC.List.$w!! */(_vm/* LudoJS.lvl5 */, E(_wJ/* si1F */)+1|0));
      }),
      _wN/* si2v */ = new T(function(){
        var _wO/* si1N */ = E(_vr/* shYm */),
        _wP/* si2u */ = new T(function(){
          var _wQ/* si2t */ = new T(function(){
            if((E(_wJ/* si1F */)+1|0)!=E(_vn/* LudoJS.lvl6 */)){
              var _wR/* si2f */ = new T(function(){
                var _wS/* si22 */ = B(_da/* LudoJS.$s!1 */(_vs/* shYo */, _vt/* shYv */));
                if(!_wS/* si22 */._){
                  return __Z/* EXTERNAL */;
                }else{
                  var _wT/* si24 */ = _wS/* si22 */.b,
                  _wU/* si25 */ = E(_wS/* si22 */.a),
                  _wV/* si28 */ = E(_vp/* shYk */);
                  if(_wV/* si28 */!=E(_wU/* si25 */.a)){
                    return new T2(1,_wU/* si25 */,new T(function(){
                      return B(_pu/* LudoJS.$sremoveFrom */(_wT/* si24 */, _wV/* si28 */));
                    }));
                  }else{
                    return E(_wT/* si24 */);
                  }
                }
              });
              return new T2(1,new T2(0,_vp/* shYk */,new T1(1,_wM/* si1J */)),_wR/* si2f */);
            }else{
              var _wW/* si2g */ = B(_da/* LudoJS.$s!1 */(_vs/* shYo */, _vt/* shYv */));
              if(!_wW/* si2g */._){
                return __Z/* EXTERNAL */;
              }else{
                var _wX/* si2i */ = _wW/* si2g */.b,
                _wY/* si2j */ = E(_wW/* si2g */.a),
                _wZ/* si2m */ = E(_vp/* shYk */);
                if(_wZ/* si2m */!=E(_wY/* si2j */.a)){
                  return new T2(1,_wY/* si2j */,new T(function(){
                    return B(_pu/* LudoJS.$sremoveFrom */(_wX/* si2i */, _wZ/* si2m */));
                  }));
                }else{
                  return E(_wX/* si2i */);
                }
              }
            }
          });
          return B(_3j/* LudoJS.$sinsert_$sgo10 */(_vs/* shYo */, _wQ/* si2t */, _vt/* shYv */));
        });
        return new T5(0,_wO/* si1N */.a,_wO/* si1N */.b,_wO/* si1N */.c,_wP/* si2u */,_wO/* si1N */.e);
      }),
      _x0/* si2w */ = B(_qS/* LudoJS.a43 */(_vs/* shYo */, _vu/* shYC */, _wN/* si2v */, _/* EXTERNAL */));
      return new F(function(){return _qS/* LudoJS.a43 */(_vs/* shYo */, _wM/* si1J */, new T(function(){
        return E(E(_x0/* si2w */).b);
      }), _/* EXTERNAL */);});
    }
  }
},
_x1/* True */ = true,
_x2/* $wa14 */ = function(_x3/* siyP */, _x4/* siyQ */, _x5/* siyR */, _x6/* siyS */, _x7/* siyT */, _/* EXTERNAL */){
  var _x8/* siyX */ = new T5(0,_x3/* siyP */,_x4/* siyQ */,_x5/* siyR */,_x6/* siyS */,_x7/* siyT */),
  _x9/* siyY */ = function(_xa/* siyZ */){
    var _xb/* siz0 */ = B(_nw/* LudoJS.$wa13 */(_x8/* siyX */, _/* EXTERNAL */)),
    _xc/* siz3 */ = E(_xb/* siz0 */),
    _xd/* siz6 */ = E(_xc/* siz3 */.b),
    _xe/* sizc */ = B(_x2/* LudoJS.$wa14 */(_xd/* siz6 */.a, _xd/* siz6 */.b, _xd/* siz6 */.c, _xd/* siz6 */.d, _xd/* siz6 */.e, _/* EXTERNAL */));
    return new T2(0,new T2(1,_xc/* siz3 */.a,new T(function(){
      return E(E(_xe/* sizc */).a);
    })),new T(function(){
      return E(E(_xe/* sizc */).b);
    }));
  };
  if(!E(B(_mL/* GHC.List.$wlenAcc */(B(_da/* LudoJS.$s!1 */(_x4/* siyQ */, _x6/* siyS */)), 0)))){
    return new F(function(){return _x9/* siyY */(_/* EXTERNAL */);});
  }else{
    if(E(_x5/* siyR */)>=1){
      return new T2(0,_4/* GHC.Types.[] */,_x8/* siyX */);
    }else{
      return new F(function(){return _x9/* siyY */(_/* EXTERNAL */);});
    }
  }
},
_xf/* f1 */ = new T(function(){
  return eval/* EXTERNAL */("((gs, opts, rolls, prevRolls) => drawDiceAnimation(gs, opts, rolls, prevRolls))");
}),
_xg/* lvl41 */ = function(_xh/* siAh */){
  var _xi/* siAi */ = B(_gY/* System.Random.$w$crandomR12 */(_gV/* System.Random.Internal.$fRandomGenStdGen */, 1, 6, _xh/* siAh */));
  return new T2(0,E(_xi/* siAi */.b),_xi/* siAi */.a);
},
_xj/* $fFractionalFixed1 */ = new T1(0,0),
_xk/* lvl */ = new T1(0,0),
_xl/* orInteger */ = function(_xm/* s1KS */, _xn/* s1KT */){
  while(1){
    var _xo/* s1KU */ = E(_xm/* s1KS */);
    if(!_xo/* s1KU */._){
      var _xp/* s1KV */ = _xo/* s1KU */.a,
      _xq/* s1KW */ = E(_xn/* s1KT */);
      if(!_xq/* s1KW */._){
        return new T1(0,(_xp/* s1KV */>>>0|_xq/* s1KW */.a>>>0)>>>0&4294967295);
      }else{
        _xm/* s1KS */ = new T1(1,I_fromInt/* EXTERNAL */(_xp/* s1KV */));
        _xn/* s1KT */ = _xq/* s1KW */;
        continue;
      }
    }else{
      var _xr/* s1L7 */ = E(_xn/* s1KT */);
      if(!_xr/* s1L7 */._){
        _xm/* s1KS */ = _xo/* s1KU */;
        _xn/* s1KT */ = new T1(1,I_fromInt/* EXTERNAL */(_xr/* s1L7 */.a));
        continue;
      }else{
        return new T1(1,I_or/* EXTERNAL */(_xo/* s1KU */.a, _xr/* s1L7 */.a));
      }
    }
  }
},
_xs/* shiftLInteger */ = function(_xt/* s1Jk */, _xu/* s1Jl */){
  while(1){
    var _xv/* s1Jm */ = E(_xt/* s1Jk */);
    if(!_xv/* s1Jm */._){
      _xt/* s1Jk */ = new T1(1,I_fromInt/* EXTERNAL */(_xv/* s1Jm */.a));
      continue;
    }else{
      return new T1(1,I_shiftLeft/* EXTERNAL */(_xv/* s1Jm */.a, _xu/* s1Jl */));
    }
  }
},
_xw/* mkInteger_f */ = function(_xx/* s1S6 */){
  var _xy/* s1S7 */ = E(_xx/* s1S6 */);
  if(!_xy/* s1S7 */._){
    return E(_xk/* GHC.Integer.Type.lvl */);
  }else{
    return new F(function(){return _xl/* GHC.Integer.Type.orInteger */(new T1(0,E(_xy/* s1S7 */.a)), B(_xs/* GHC.Integer.Type.shiftLInteger */(B(_xw/* GHC.Integer.Type.mkInteger_f */(_xy/* s1S7 */.b)), 31)));});
  }
},
_xz/* log2I1 */ = new T1(0,1),
_xA/* lvl2 */ = new T1(0,2147483647),
_xB/* lvl3 */ = new T(function(){
  return B(_qJ/* GHC.Integer.Type.plusInteger */(_xA/* GHC.Integer.Type.lvl2 */, _xz/* GHC.Integer.Type.log2I1 */));
}),
_xC/* negateInteger */ = function(_xD/* s1QH */){
  var _xE/* s1QI */ = E(_xD/* s1QH */);
  if(!_xE/* s1QI */._){
    var _xF/* s1QK */ = E(_xE/* s1QI */.a);
    return (_xF/* s1QK */==( -2147483648)) ? E(_xB/* GHC.Integer.Type.lvl3 */) : new T1(0, -_xF/* s1QK */);
  }else{
    return new T1(1,I_negate/* EXTERNAL */(_xE/* s1QI */.a));
  }
},
_xG/* mkInteger */ = function(_xH/* s1Sf */, _xI/* s1Sg */){
  if(!E(_xH/* s1Sf */)){
    return new F(function(){return _xC/* GHC.Integer.Type.negateInteger */(B(_xw/* GHC.Integer.Type.mkInteger_f */(_xI/* s1Sg */)));});
  }else{
    return new F(function(){return _xw/* GHC.Integer.Type.mkInteger_f */(_xI/* s1Sg */);});
  }
},
_xJ/* s6TCi */ = 1420103680,
_xK/* s6TCj */ = 465,
_xL/* s6TCk */ = new T2(1,_xK/* s6TCj */,_4/* GHC.Types.[] */),
_xM/* s6TCl */ = new T2(1,_xJ/* s6TCi */,_xL/* s6TCk */),
_xN/* $fHasResolutionE5 */ = new T(function(){
  return B(_xG/* GHC.Integer.Type.mkInteger */(_x1/* GHC.Types.True */, _xM/* s6TCl */));
}),
_xO/* $wa1 */ = function(_xP/* s3vU */, _/* EXTERNAL */){
  var _xQ/* s3vZ */ = __get/* EXTERNAL */(_xP/* s3vU */, 0),
  _xR/* s3w5 */ = __get/* EXTERNAL */(_xP/* s3vU */, 1),
  _xS/* s3w9 */ = Number/* EXTERNAL */(_xQ/* s3vZ */),
  _xT/* s3wd */ = jsTrunc/* EXTERNAL */(_xS/* s3w9 */),
  _xU/* s3wh */ = Number/* EXTERNAL */(_xR/* s3w5 */),
  _xV/* s3wl */ = jsTrunc/* EXTERNAL */(_xU/* s3wh */);
  return new T2(0,_xT/* s3wd */,_xV/* s3wl */);
},
_xW/* divInt# */ = function(_xX/* scDT */, _xY/* scDU */){
  if(_xX/* scDT */<=0){
    if(_xX/* scDT */>=0){
      return new F(function(){return quot/* EXTERNAL */(_xX/* scDT */, _xY/* scDU */);});
    }else{
      if(_xY/* scDU */<=0){
        return new F(function(){return quot/* EXTERNAL */(_xX/* scDT */, _xY/* scDU */);});
      }else{
        return quot/* EXTERNAL */(_xX/* scDT */+1|0, _xY/* scDU */)-1|0;
      }
    }
  }else{
    if(_xY/* scDU */>=0){
      if(_xX/* scDT */>=0){
        return new F(function(){return quot/* EXTERNAL */(_xX/* scDT */, _xY/* scDU */);});
      }else{
        if(_xY/* scDU */<=0){
          return new F(function(){return quot/* EXTERNAL */(_xX/* scDT */, _xY/* scDU */);});
        }else{
          return quot/* EXTERNAL */(_xX/* scDT */+1|0, _xY/* scDU */)-1|0;
        }
      }
    }else{
      return quot/* EXTERNAL */(_xX/* scDT */-1|0, _xY/* scDU */)-1|0;
    }
  }
},
_xZ/* divInteger */ = function(_y0/* s1Nz */, _y1/* s1NA */){
  while(1){
    var _y2/* s1NB */ = E(_y0/* s1Nz */);
    if(!_y2/* s1NB */._){
      var _y3/* s1ND */ = E(_y2/* s1NB */.a);
      if(_y3/* s1ND */==( -2147483648)){
        _y0/* s1Nz */ = new T1(1,I_fromInt/* EXTERNAL */( -2147483648));
        continue;
      }else{
        var _y4/* s1NE */ = E(_y1/* s1NA */);
        if(!_y4/* s1NE */._){
          return new T1(0,B(_xW/* GHC.Classes.divInt# */(_y3/* s1ND */, _y4/* s1NE */.a)));
        }else{
          _y0/* s1Nz */ = new T1(1,I_fromInt/* EXTERNAL */(_y3/* s1ND */));
          _y1/* s1NA */ = _y4/* s1NE */;
          continue;
        }
      }
    }else{
      var _y5/* s1NO */ = _y2/* s1NB */.a,
      _y6/* s1NP */ = E(_y1/* s1NA */);
      return (_y6/* s1NP */._==0) ? new T1(1,I_div/* EXTERNAL */(_y5/* s1NO */, I_fromInt/* EXTERNAL */(_y6/* s1NP */.a))) : new T1(1,I_div/* EXTERNAL */(_y5/* s1NO */, _y6/* s1NP */.a));
    }
  }
},
_y7/* eqInteger */ = function(_y8/* s1Fo */, _y9/* s1Fp */){
  var _ya/* s1Fq */ = E(_y8/* s1Fo */);
  if(!_ya/* s1Fq */._){
    var _yb/* s1Fr */ = _ya/* s1Fq */.a,
    _yc/* s1Fs */ = E(_y9/* s1Fp */);
    return (_yc/* s1Fs */._==0) ? _yb/* s1Fr */==_yc/* s1Fs */.a : (I_compareInt/* EXTERNAL */(_yc/* s1Fs */.a, _yb/* s1Fr */)==0) ? true : false;
  }else{
    var _yd/* s1Fy */ = _ya/* s1Fq */.a,
    _ye/* s1Fz */ = E(_y9/* s1Fp */);
    return (_ye/* s1Fz */._==0) ? (I_compareInt/* EXTERNAL */(_yd/* s1Fy */, _ye/* s1Fz */.a)==0) ? true : false : (I_compare/* EXTERNAL */(_yd/* s1Fy */, _ye/* s1Fz */.a)==0) ? true : false;
  }
},
_yf/* getCTimeval_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function(){var ms = new Date().getTime();                   return [(ms/1000)|0, ((ms % 1000)*1000)|0];})");
}),
_yg/* slti */ = 660865024,
_yh/* sltj */ = 465661287,
_yi/* sltk */ = new T2(1,_yh/* sltj */,_4/* GHC.Types.[] */),
_yj/* sltl */ = new T2(1,_yg/* slti */,_yi/* sltk */),
_yk/* getPOSIXTime2 */ = new T(function(){
  return B(_xG/* GHC.Integer.Type.mkInteger */(_x1/* GHC.Types.True */, _yj/* sltl */));
}),
_yl/* smallInteger */ = function(_ym/* B1 */){
  return new T1(0,_ym/* B1 */);
},
_yn/* timesInteger */ = function(_yo/* s1PN */, _yp/* s1PO */){
  while(1){
    var _yq/* s1PP */ = E(_yo/* s1PN */);
    if(!_yq/* s1PP */._){
      var _yr/* s1PQ */ = _yq/* s1PP */.a,
      _ys/* s1PR */ = E(_yp/* s1PO */);
      if(!_ys/* s1PR */._){
        var _yt/* s1PS */ = _ys/* s1PR */.a;
        if(!(imul/* EXTERNAL */(_yr/* s1PQ */, _yt/* s1PS */)|0)){
          return new T1(0,imul/* EXTERNAL */(_yr/* s1PQ */, _yt/* s1PS */)|0);
        }else{
          _yo/* s1PN */ = new T1(1,I_fromInt/* EXTERNAL */(_yr/* s1PQ */));
          _yp/* s1PO */ = new T1(1,I_fromInt/* EXTERNAL */(_yt/* s1PS */));
          continue;
        }
      }else{
        _yo/* s1PN */ = new T1(1,I_fromInt/* EXTERNAL */(_yr/* s1PQ */));
        _yp/* s1PO */ = _ys/* s1PR */;
        continue;
      }
    }else{
      var _yu/* s1Q6 */ = E(_yp/* s1PO */);
      if(!_yu/* s1Q6 */._){
        _yo/* s1PN */ = _yq/* s1PP */;
        _yp/* s1PO */ = new T1(1,I_fromInt/* EXTERNAL */(_yu/* s1Q6 */.a));
        continue;
      }else{
        return new T1(1,I_mul/* EXTERNAL */(_yq/* s1PP */.a, _yu/* s1Q6 */.a));
      }
    }
  }
},
_yv/* getPOSIXTime1 */ = function(_/* EXTERNAL */){
  var _yw/* sltq */ = __app0/* EXTERNAL */(E(_yf/* Data.Time.Clock.CTimeval.getCTimeval_f1 */)),
  _yx/* sltt */ = B(_xO/* Data.Time.Clock.CTimeval.$wa1 */(_yw/* sltq */, _/* EXTERNAL */));
  return new T(function(){
    var _yy/* sltw */ = E(_yx/* sltt */);
    if(!B(_y7/* GHC.Integer.Type.eqInteger */(_yk/* Data.Time.Clock.POSIX.getPOSIXTime2 */, _xj/* Data.Fixed.$fFractionalFixed1 */))){
      return B(_qJ/* GHC.Integer.Type.plusInteger */(B(_yn/* GHC.Integer.Type.timesInteger */(B(_yl/* GHC.Integer.Type.smallInteger */(E(_yy/* sltw */.a))), _xN/* Data.Fixed.$fHasResolutionE5 */)), B(_xZ/* GHC.Integer.Type.divInteger */(B(_yn/* GHC.Integer.Type.timesInteger */(B(_yn/* GHC.Integer.Type.timesInteger */(B(_yl/* GHC.Integer.Type.smallInteger */(E(_yy/* sltw */.b))), _xN/* Data.Fixed.$fHasResolutionE5 */)), _xN/* Data.Fixed.$fHasResolutionE5 */)), _yk/* Data.Time.Clock.POSIX.getPOSIXTime2 */))));
    }else{
      return E(_ft/* GHC.Real.divZeroError */);
    }
  });
},
_yz/* $fBitsWord4 */ = 0,
_yA/* $fBoundedWord32_$cmaxBound */ = 4294967295,
_yB/* $fBoundedWord32 */ = new T2(0,_yz/* GHC.Word.$fBitsWord4 */,_yA/* GHC.Word.$fBoundedWord32_$cmaxBound */),
_yC/* $fEnumRatio1 */ = new T1(0,1),
_yD/* $p1Integral */ = function(_yE/* sv9T */){
  return E(E(_yE/* sv9T */).a);
},
_yF/* $p1Real */ = function(_yG/* svbu */){
  return E(E(_yG/* svbu */).a);
},
_yH/* fromInteger */ = function(_yI/* s6Go */){
  return E(E(_yI/* s6Go */).g);
},
_yJ/* gtInteger */ = function(_yK/* s1G1 */, _yL/* s1G2 */){
  var _yM/* s1G3 */ = E(_yK/* s1G1 */);
  if(!_yM/* s1G3 */._){
    var _yN/* s1G4 */ = _yM/* s1G3 */.a,
    _yO/* s1G5 */ = E(_yL/* s1G2 */);
    return (_yO/* s1G5 */._==0) ? _yN/* s1G4 */>_yO/* s1G5 */.a : I_compareInt/* EXTERNAL */(_yO/* s1G5 */.a, _yN/* s1G4 */)<0;
  }else{
    var _yP/* s1Gc */ = _yM/* s1G3 */.a,
    _yQ/* s1Gd */ = E(_yL/* s1G2 */);
    return (_yQ/* s1Gd */._==0) ? I_compareInt/* EXTERNAL */(_yP/* s1Gc */, _yQ/* s1Gd */.a)>0 : I_compare/* EXTERNAL */(_yP/* s1Gc */, _yQ/* s1Gd */.a)>0;
  }
},
_yR/* maxBound */ = function(_yS/* smih */){
  return E(E(_yS/* smih */).b);
},
_yT/* toInteger */ = function(_yU/* svbj */){
  return E(E(_yU/* svbj */).i);
},
_yV/* integralEnumFrom */ = function(_yW/* svrj */, _yX/* svrk */, _yY/* svrl */){
  var _yZ/* svro */ = new T(function(){
    return B(_yH/* GHC.Num.fromInteger */(new T(function(){
      return B(_yF/* GHC.Real.$p1Real */(new T(function(){
        return B(_yD/* GHC.Real.$p1Integral */(_yW/* svrj */));
      })));
    })));
  }),
  _z0/* svrq */ = new T(function(){
    return B(_yR/* GHC.Enum.maxBound */(_yX/* svrk */));
  }),
  _z1/* svrr */ = function(_z2/* svrs */){
    return (!B(_yJ/* GHC.Integer.Type.gtInteger */(_z2/* svrs */, B(A2(_yT/* GHC.Real.toInteger */,_yW/* svrj */, _z0/* svrq */))))) ? new T2(1,new T(function(){
      return B(A1(_yZ/* svro */,_z2/* svrs */));
    }),new T(function(){
      return B(_z1/* svrr */(B(_qJ/* GHC.Integer.Type.plusInteger */(_z2/* svrs */, _yC/* GHC.Real.$fEnumRatio1 */))));
    })) : __Z/* EXTERNAL */;
  };
  return new F(function(){return _z1/* svrr */(B(A2(_yT/* GHC.Real.toInteger */,_yW/* svrj */, _yY/* svrl */)));});
},
_z3/* $fEnumWord32_$cenumFrom */ = function(_z4/* B1 */){
  return new F(function(){return _yV/* GHC.Real.integralEnumFrom */(_z5/* GHC.Word.$fIntegralWord32 */, _yB/* GHC.Word.$fBoundedWord32 */, _z4/* B1 */);});
},
_z6/* $fEnumInteger1 */ = new T1(0,0),
_z7/* ltInteger */ = function(_z8/* s1GH */, _z9/* s1GI */){
  var _za/* s1GJ */ = E(_z8/* s1GH */);
  if(!_za/* s1GJ */._){
    var _zb/* s1GK */ = _za/* s1GJ */.a,
    _zc/* s1GL */ = E(_z9/* s1GI */);
    return (_zc/* s1GL */._==0) ? _zb/* s1GK */<_zc/* s1GL */.a : I_compareInt/* EXTERNAL */(_zc/* s1GL */.a, _zb/* s1GK */)>0;
  }else{
    var _zd/* s1GS */ = _za/* s1GJ */.a,
    _ze/* s1GT */ = E(_z9/* s1GI */);
    return (_ze/* s1GT */._==0) ? I_compareInt/* EXTERNAL */(_zd/* s1GS */, _ze/* s1GT */.a)<0 : I_compare/* EXTERNAL */(_zd/* s1GS */, _ze/* s1GT */.a)<0;
  }
},
_zf/* up_fb */ = function(_zg/* smjD */, _zh/* smjE */, _zi/* smjF */, _zj/* smjG */, _zk/* smjH */){
  var _zl/* smjI */ = function(_zm/* smjJ */){
    if(!B(_yJ/* GHC.Integer.Type.gtInteger */(_zm/* smjJ */, _zk/* smjH */))){
      return new F(function(){return A2(_zg/* smjD */,_zm/* smjJ */, new T(function(){
        return B(_zl/* smjI */(B(_qJ/* GHC.Integer.Type.plusInteger */(_zm/* smjJ */, _zj/* smjG */))));
      }));});
    }else{
      return E(_zh/* smjE */);
    }
  };
  return new F(function(){return _zl/* smjI */(_zi/* smjF */);});
},
_zn/* enumDeltaToIntegerFB */ = function(_zo/* smFL */, _zp/* smFM */, _zq/* smFN */, _zr/* smFO */, _zs/* smFP */){
  if(!B(_qq/* GHC.Integer.Type.geInteger */(_zr/* smFO */, _z6/* GHC.Enum.$fEnumInteger1 */))){
    var _zt/* smFR */ = function(_zu/* smFS */){
      if(!B(_z7/* GHC.Integer.Type.ltInteger */(_zu/* smFS */, _zs/* smFP */))){
        return new F(function(){return A2(_zo/* smFL */,_zu/* smFS */, new T(function(){
          return B(_zt/* smFR */(B(_qJ/* GHC.Integer.Type.plusInteger */(_zu/* smFS */, _zr/* smFO */))));
        }));});
      }else{
        return E(_zp/* smFM */);
      }
    };
    return new F(function(){return _zt/* smFR */(_zq/* smFN */);});
  }else{
    return new F(function(){return _zf/* GHC.Enum.up_fb */(_zo/* smFL */, _zp/* smFM */, _zq/* smFN */, _zr/* smFO */, _zs/* smFP */);});
  }
},
_zv/* minBound */ = function(_zw/* smid */){
  return E(E(_zw/* smid */).a);
},
_zx/* minusInteger */ = function(_zy/* s1P0 */, _zz/* s1P1 */){
  while(1){
    var _zA/* s1P2 */ = E(_zy/* s1P0 */);
    if(!_zA/* s1P2 */._){
      var _zB/* s1P3 */ = _zA/* s1P2 */.a,
      _zC/* s1P4 */ = E(_zz/* s1P1 */);
      if(!_zC/* s1P4 */._){
        var _zD/* s1P5 */ = _zC/* s1P4 */.a,
        _zE/* s1P6 */ = subC/* EXTERNAL */(_zB/* s1P3 */, _zD/* s1P5 */);
        if(!E(_zE/* s1P6 */.b)){
          return new T1(0,_zE/* s1P6 */.a);
        }else{
          _zy/* s1P0 */ = new T1(1,I_fromInt/* EXTERNAL */(_zB/* s1P3 */));
          _zz/* s1P1 */ = new T1(1,I_fromInt/* EXTERNAL */(_zD/* s1P5 */));
          continue;
        }
      }else{
        _zy/* s1P0 */ = new T1(1,I_fromInt/* EXTERNAL */(_zB/* s1P3 */));
        _zz/* s1P1 */ = _zC/* s1P4 */;
        continue;
      }
    }else{
      var _zF/* s1Pl */ = E(_zz/* s1P1 */);
      if(!_zF/* s1Pl */._){
        _zy/* s1P0 */ = _zA/* s1P2 */;
        _zz/* s1P1 */ = new T1(1,I_fromInt/* EXTERNAL */(_zF/* s1Pl */.a));
        continue;
      }else{
        return new T1(1,I_sub/* EXTERNAL */(_zA/* s1P2 */.a, _zF/* s1Pl */.a));
      }
    }
  }
},
_zG/* integralEnumFromThen */ = function(_zH/* svry */, _zI/* svrz */, _zJ/* svrA */, _zK/* svrB */){
  var _zL/* svrC */ = B(A2(_yT/* GHC.Real.toInteger */,_zH/* svry */, _zK/* svrB */)),
  _zM/* svrD */ = B(A2(_yT/* GHC.Real.toInteger */,_zH/* svry */, _zJ/* svrA */));
  if(!B(_qq/* GHC.Integer.Type.geInteger */(_zL/* svrC */, _zM/* svrD */))){
    var _zN/* svrH */ = new T(function(){
      return B(_yH/* GHC.Num.fromInteger */(new T(function(){
        return B(_yF/* GHC.Real.$p1Real */(new T(function(){
          return B(_yD/* GHC.Real.$p1Integral */(_zH/* svry */));
        })));
      })));
    }),
    _zO/* svrL */ = function(_zP/* svrI */, _zQ/* svrJ */){
      return new T2(1,new T(function(){
        return B(A1(_zN/* svrH */,_zP/* svrI */));
      }),_zQ/* svrJ */);
    };
    return new F(function(){return _zn/* GHC.Enum.enumDeltaToIntegerFB */(_zO/* svrL */, _4/* GHC.Types.[] */, _zM/* svrD */, B(_zx/* GHC.Integer.Type.minusInteger */(_zL/* svrC */, _zM/* svrD */)), B(A2(_yT/* GHC.Real.toInteger */,_zH/* svry */, new T(function(){
      return B(_zv/* GHC.Enum.minBound */(_zI/* svrz */));
    }))));});
  }else{
    var _zR/* svrR */ = new T(function(){
      return B(_yH/* GHC.Num.fromInteger */(new T(function(){
        return B(_yF/* GHC.Real.$p1Real */(new T(function(){
          return B(_yD/* GHC.Real.$p1Integral */(_zH/* svry */));
        })));
      })));
    }),
    _zS/* svrV */ = function(_zT/* svrS */, _zU/* svrT */){
      return new T2(1,new T(function(){
        return B(A1(_zR/* svrR */,_zT/* svrS */));
      }),_zU/* svrT */);
    };
    return new F(function(){return _zn/* GHC.Enum.enumDeltaToIntegerFB */(_zS/* svrV */, _4/* GHC.Types.[] */, _zM/* svrD */, B(_zx/* GHC.Integer.Type.minusInteger */(_zL/* svrC */, _zM/* svrD */)), B(A2(_yT/* GHC.Real.toInteger */,_zH/* svry */, new T(function(){
      return B(_yR/* GHC.Enum.maxBound */(_zI/* svrz */));
    }))));});
  }
},
_zV/* $fEnumWord32_$cenumFromThen */ = function(_zW/* B2 */, _z4/* B1 */){
  return new F(function(){return _zG/* GHC.Real.integralEnumFromThen */(_z5/* GHC.Word.$fIntegralWord32 */, _yB/* GHC.Word.$fBoundedWord32 */, _zW/* B2 */, _z4/* B1 */);});
},
_zX/* integralEnumFromThenTo */ = function(_zY/* svsd */, _zZ/* svse */, _A0/* svsf */, _A1/* svsg */){
  var _A2/* svsh */ = B(A2(_yT/* GHC.Real.toInteger */,_zY/* svsd */, _zZ/* svse */)),
  _A3/* svsk */ = new T(function(){
    return B(_yH/* GHC.Num.fromInteger */(new T(function(){
      return B(_yF/* GHC.Real.$p1Real */(new T(function(){
        return B(_yD/* GHC.Real.$p1Integral */(_zY/* svsd */));
      })));
    })));
  }),
  _A4/* svso */ = function(_A5/* svsl */, _A6/* svsm */){
    return new T2(1,new T(function(){
      return B(A1(_A3/* svsk */,_A5/* svsl */));
    }),_A6/* svsm */);
  };
  return new F(function(){return _zn/* GHC.Enum.enumDeltaToIntegerFB */(_A4/* svso */, _4/* GHC.Types.[] */, _A2/* svsh */, B(_zx/* GHC.Integer.Type.minusInteger */(B(A2(_yT/* GHC.Real.toInteger */,_zY/* svsd */, _A0/* svsf */)), _A2/* svsh */)), B(A2(_yT/* GHC.Real.toInteger */,_zY/* svsd */, _A1/* svsg */)));});
},
_A7/* $fEnumWord32_$cenumFromThenTo */ = function(_A8/* B3 */, _zW/* B2 */, _z4/* B1 */){
  return new F(function(){return _zX/* GHC.Real.integralEnumFromThenTo */(_z5/* GHC.Word.$fIntegralWord32 */, _A8/* B3 */, _zW/* B2 */, _z4/* B1 */);});
},
_A9/* integralEnumFromTo */ = function(_Aa/* svrZ */, _Ab/* svs0 */, _Ac/* svs1 */){
  var _Ad/* svs4 */ = new T(function(){
    return B(_yH/* GHC.Num.fromInteger */(new T(function(){
      return B(_yF/* GHC.Real.$p1Real */(new T(function(){
        return B(_yD/* GHC.Real.$p1Integral */(_Aa/* svrZ */));
      })));
    })));
  }),
  _Ae/* svs6 */ = function(_Af/* svs7 */){
    return (!B(_yJ/* GHC.Integer.Type.gtInteger */(_Af/* svs7 */, B(A2(_yT/* GHC.Real.toInteger */,_Aa/* svrZ */, _Ac/* svs1 */))))) ? new T2(1,new T(function(){
      return B(A1(_Ad/* svs4 */,_Af/* svs7 */));
    }),new T(function(){
      return B(_Ae/* svs6 */(B(_qJ/* GHC.Integer.Type.plusInteger */(_Af/* svs7 */, _yC/* GHC.Real.$fEnumRatio1 */))));
    })) : __Z/* EXTERNAL */;
  };
  return new F(function(){return _Ae/* svs6 */(B(A2(_yT/* GHC.Real.toInteger */,_Aa/* svrZ */, _Ab/* svs0 */)));});
},
_Ag/* $fEnumWord32_$cenumFromTo */ = function(_zW/* B2 */, _z4/* B1 */){
  return new F(function(){return _A9/* GHC.Real.integralEnumFromTo */(_z5/* GHC.Word.$fIntegralWord32 */, _zW/* B2 */, _z4/* B1 */);});
},
_Ah/* wordToInteger */ = function(_Ai/* s1J7 */){
  return new T1(1,I_fromInt/* EXTERNAL */(_Ai/* s1J7 */));
},
_Aj/* $fIntegralWord32_$ctoInteger */ = function(_Ak/* s1RpU */){
  var _Al/* s1RpV */ = E(_Ak/* s1RpU */),
  _Am/* s1RpX */ = _Al/* s1RpV */&4294967295;
  if(_Am/* s1RpX */<0){
    return new F(function(){return _Ah/* GHC.Integer.Type.wordToInteger */(_Al/* s1RpV */);});
  }else{
    return new F(function(){return _yl/* GHC.Integer.Type.smallInteger */(_Am/* s1RpX */);});
  }
},
_An/* integerToJSString */ = function(_Ao/* s1Ii */){
  while(1){
    var _Ap/* s1Ij */ = E(_Ao/* s1Ii */);
    if(!_Ap/* s1Ij */._){
      _Ao/* s1Ii */ = new T1(1,I_fromInt/* EXTERNAL */(_Ap/* s1Ij */.a));
      continue;
    }else{
      return new F(function(){return I_toString/* EXTERNAL */(_Ap/* s1Ij */.a);});
    }
  }
},
_Aq/* integerToString */ = function(_Ar/* sf6p */, _As/* sf6q */){
  return new F(function(){return _q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(B(_An/* GHC.Integer.Type.integerToJSString */(_Ar/* sf6p */))), _As/* sf6q */);});
},
_At/* shows9 */ = new T1(0,0),
_Au/* $w$cshowsPrec1 */ = function(_Av/* sf7E */, _Aw/* sf7F */, _Ax/* sf7G */){
  if(_Av/* sf7E */<=6){
    return new F(function(){return _Aq/* GHC.Show.integerToString */(_Aw/* sf7F */, _Ax/* sf7G */);});
  }else{
    if(!B(_z7/* GHC.Integer.Type.ltInteger */(_Aw/* sf7F */, _At/* GHC.Show.shows9 */))){
      return new F(function(){return _Aq/* GHC.Show.integerToString */(_Aw/* sf7F */, _Ax/* sf7G */);});
    }else{
      return new T2(1,_5U/* GHC.Show.shows8 */,new T(function(){
        return B(_q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(B(_An/* GHC.Integer.Type.integerToJSString */(_Aw/* sf7F */))), new T2(1,_5T/* GHC.Show.shows7 */,_Ax/* sf7G */)));
      }));
    }
  }
},
_Ay/* $fShowWord32_$cshow */ = function(_Az/* s1RrO */){
  return new F(function(){return _Au/* GHC.Show.$w$cshowsPrec1 */(0, B(_Aj/* GHC.Word.$fIntegralWord32_$ctoInteger */(_Az/* s1RrO */)), _4/* GHC.Types.[] */);});
},
_AA/* $fShowWord2 */ = function(_AB/* s1RrD */, _AC/* s1RrE */){
  var _AD/* s1RrF */ = E(_AB/* s1RrD */),
  _AE/* s1RrH */ = _AD/* s1RrF */&4294967295;
  if(_AE/* s1RrH */<0){
    return new F(function(){return _Au/* GHC.Show.$w$cshowsPrec1 */(0, B(_Ah/* GHC.Integer.Type.wordToInteger */(_AD/* s1RrF */)), _AC/* s1RrE */);});
  }else{
    return new F(function(){return _Au/* GHC.Show.$w$cshowsPrec1 */(0, B(_yl/* GHC.Integer.Type.smallInteger */(_AE/* s1RrH */)), _AC/* s1RrE */);});
  }
},
_AF/* $fShowWord32_$cshowList */ = function(_AG/* s1RrM */, _AH/* s1RrN */){
  return new F(function(){return _A/* GHC.Show.showList__ */(_AA/* GHC.Word.$fShowWord2 */, _AG/* s1RrM */, _AH/* s1RrN */);});
},
_AI/* $fShowWord32_$cshowsPrec */ = function(_AJ/* s1Rrr */, _AK/* s1Rrs */){
  var _AL/* s1Rrt */ = new T(function(){
    var _AM/* s1Rru */ = E(_AK/* s1Rrs */),
    _AN/* s1Rrw */ = _AM/* s1Rru */&4294967295;
    if(_AN/* s1Rrw */<0){
      return B(_Ah/* GHC.Integer.Type.wordToInteger */(_AM/* s1Rru */));
    }else{
      return B(_yl/* GHC.Integer.Type.smallInteger */(_AN/* s1Rrw */));
    }
  });
  return function(_AO/* s1Rrz */){
    return new F(function(){return _Au/* GHC.Show.$w$cshowsPrec1 */(E(_AJ/* s1Rrr */), _AL/* s1Rrt */, _AO/* s1Rrz */);});
  };
},
_AP/* $fShowWord32 */ = new T3(0,_AI/* GHC.Word.$fShowWord32_$cshowsPrec */,_Ay/* GHC.Word.$fShowWord32_$cshow */,_AF/* GHC.Word.$fShowWord32_$cshowList */),
_AQ/* lvl */ = new T2(1,_5T/* GHC.Show.shows7 */,_4/* GHC.Types.[] */),
_AR/* $fShow(,)1 */ = function(_AS/* sfbb */, _AT/* sfbc */, _AU/* sfbd */){
  return new F(function(){return A1(_AS/* sfbb */,new T2(1,_x/* GHC.Show.showList__1 */,new T(function(){
    return B(A1(_AT/* sfbc */,_AU/* sfbd */));
  })));});
},
_AV/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */(": empty list"));
}),
_AW/* errorEmptyList */ = function(_AX/* sbDG */){
  return new F(function(){return err/* EXTERNAL */(B(_q/* GHC.Base.++ */(_pB/* GHC.List.prel_list_str */, new T(function(){
    return B(_q/* GHC.Base.++ */(_AX/* sbDG */, _AV/* GHC.List.lvl */));
  },1))));});
},
_AY/* lvl7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("foldr1"));
}),
_AZ/* lvl8 */ = new T(function(){
  return B(_AW/* GHC.List.errorEmptyList */(_AY/* GHC.List.lvl7 */));
}),
_B0/* foldr1 */ = function(_B1/* sbKQ */, _B2/* sbKR */){
  var _B3/* sbKS */ = E(_B2/* sbKR */);
  if(!_B3/* sbKS */._){
    return E(_AZ/* GHC.List.lvl8 */);
  }else{
    var _B4/* sbKT */ = _B3/* sbKS */.a,
    _B5/* sbKV */ = E(_B3/* sbKS */.b);
    if(!_B5/* sbKV */._){
      return E(_B4/* sbKT */);
    }else{
      return new F(function(){return A2(_B1/* sbKQ */,_B4/* sbKT */, new T(function(){
        return B(_B0/* GHC.List.foldr1 */(_B1/* sbKQ */, _B5/* sbKV */));
      }));});
    }
  }
},
_B6/* lvl14 */ = function(_B7/* smzT */){
  return new F(function(){return _5V/* GHC.Show.$wshowSignedInt */(0,  -2147483648, _B7/* smzT */);});
},
_B8/* lvl15 */ = function(_B9/* smzU */){
  return new F(function(){return _5V/* GHC.Show.$wshowSignedInt */(0, 2147483647, _B9/* smzU */);});
},
_Ba/* lvl16 */ = new T2(1,_B8/* GHC.Enum.lvl15 */,_4/* GHC.Types.[] */),
_Bb/* lvl17 */ = new T2(1,_B6/* GHC.Enum.lvl14 */,_Ba/* GHC.Enum.lvl16 */),
_Bc/* lvl18 */ = new T(function(){
  return B(_B0/* GHC.List.foldr1 */(_AR/* GHC.Show.$fShow(,)1 */, _Bb/* GHC.Enum.lvl17 */));
}),
_Bd/* lvl19 */ = new T(function(){
  return B(A1(_Bc/* GHC.Enum.lvl18 */,_AQ/* GHC.Enum.lvl */));
}),
_Be/* lvl20 */ = new T2(1,_5U/* GHC.Show.shows8 */,_Bd/* GHC.Enum.lvl19 */),
_Bf/* lvl21 */ = new T(function(){
  return B(unAppCStr/* EXTERNAL */(") is outside of Int\'s bounds ", _Be/* GHC.Enum.lvl20 */));
}),
_Bg/* show */ = function(_Bh/* sf6a */){
  return E(E(_Bh/* sf6a */).b);
},
_Bi/* lvl22 */ = function(_Bj/* smzV */, _Bk/* smzW */, _Bl/* smzX */){
  var _Bm/* smA1 */ = new T(function(){
    var _Bn/* smA0 */ = new T(function(){
      return B(unAppCStr/* EXTERNAL */("}: value (", new T(function(){
        return B(_q/* GHC.Base.++ */(B(A2(_Bg/* GHC.Show.show */,_Bl/* smzX */, _Bk/* smzW */)), _Bf/* GHC.Enum.lvl21 */));
      })));
    },1);
    return B(_q/* GHC.Base.++ */(_Bj/* smzV */, _Bn/* smA0 */));
  });
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("Enum.fromEnum{", _Bm/* smA1 */)));});
},
_Bo/* fromEnumError */ = function(_Bp/* smA3 */, _Bq/* smA4 */, _Br/* smA5 */){
  return new F(function(){return _Bi/* GHC.Enum.lvl22 */(_Bq/* smA4 */, _Br/* smA5 */, _Bp/* smA3 */);});
},
_Bs/* lvl4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Word32"));
}),
_Bt/* lvl10 */ = function(_Bu/* s1RrQ */){
  return new F(function(){return _Bo/* GHC.Enum.fromEnumError */(_AP/* GHC.Word.$fShowWord32 */, _Bs/* GHC.Word.lvl4 */, _Bu/* s1RrQ */);});
},
_Bv/* $fEnumWord32_$cfromEnum */ = function(_Bw/* s1RMa */){
  var _Bx/* s1RMb */ = E(_Bw/* s1RMa */);
  if(_Bx/* s1RMb */>2147483647){
    return new F(function(){return _Bt/* GHC.Word.lvl10 */(_Bx/* s1RMb */);});
  }else{
    return _Bx/* s1RMb */&4294967295;
  }
},
_By/* lvl1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("}: tried to take `pred\' of minBound"));
}),
_Bz/* lvl2 */ = function(_BA/* smnl */){
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("Enum.pred{", new T(function(){
    return B(_q/* GHC.Base.++ */(_BA/* smnl */, _By/* GHC.Enum.lvl1 */));
  }))));});
},
_BB/* predError */ = function(_BC/* B1 */){
  return new F(function(){return _Bz/* GHC.Enum.lvl2 */(_BC/* B1 */);});
},
_BD/* $fEnumWord7 */ = new T(function(){
  return B(_BB/* GHC.Enum.predError */(_Bs/* GHC.Word.lvl4 */));
}),
_BE/* $fEnumWord32_$cpred */ = function(_BF/* s1RNu */){
  var _BG/* s1RNx */ = E(_BF/* s1RNu */);
  return (_BG/* s1RNx */==0) ? E(_BD/* GHC.Word.$fEnumWord7 */) : _BG/* s1RNx */-1>>>0;
},
_BH/* lvl3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("}: tried to take `succ\' of maxBound"));
}),
_BI/* lvl4 */ = function(_BJ/* smno */){
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("Enum.succ{", new T(function(){
    return B(_q/* GHC.Base.++ */(_BJ/* smno */, _BH/* GHC.Enum.lvl3 */));
  }))));});
},
_BK/* succError */ = function(_BC/* B1 */){
  return new F(function(){return _BI/* GHC.Enum.lvl4 */(_BC/* B1 */);});
},
_BL/* $fEnumWord9 */ = new T(function(){
  return B(_BK/* GHC.Enum.succError */(_Bs/* GHC.Word.lvl4 */));
}),
_BM/* $fEnumWord32_$csucc */ = function(_BN/* s1RNp */){
  var _BO/* s1RNs */ = E(_BN/* s1RNp */);
  return (_BO/* s1RNs */==4294967295) ? E(_BL/* GHC.Word.$fEnumWord9 */) : _BO/* s1RNs */+1>>>0;
},
_BP/* lvl12 */ = new T2(0,_yz/* GHC.Word.$fBitsWord4 */,_yA/* GHC.Word.$fBoundedWord32_$cmaxBound */),
_BQ/* shows14 */ = 0,
_BR/* showsPrec */ = function(_BS/* sf65 */){
  return E(E(_BS/* sf65 */).a);
},
_BT/* lvl5 */ = function(_BU/* smnr */, _BV/* smns */, _BW/* smnt */, _BX/* smnu */){
  var _BY/* smnK */ = new T(function(){
    var _BZ/* smnJ */ = new T(function(){
      var _C0/* smnI */ = new T(function(){
        var _C1/* smnH */ = new T(function(){
          var _C2/* smnG */ = new T(function(){
            var _C3/* smny */ = E(_BW/* smnt */),
            _C4/* smnF */ = new T(function(){
              return B(A3(_B0/* GHC.List.foldr1 */,_AR/* GHC.Show.$fShow(,)1 */, new T2(1,new T(function(){
                return B(A3(_BR/* GHC.Show.showsPrec */,_BX/* smnu */, _BQ/* GHC.Show.shows14 */, _C3/* smny */.a));
              }),new T2(1,new T(function(){
                return B(A3(_BR/* GHC.Show.showsPrec */,_BX/* smnu */, _BQ/* GHC.Show.shows14 */, _C3/* smny */.b));
              }),_4/* GHC.Types.[] */)), _AQ/* GHC.Enum.lvl */));
            });
            return new T2(1,_5U/* GHC.Show.shows8 */,_C4/* smnF */);
          });
          return B(unAppCStr/* EXTERNAL */(") is outside of bounds ", _C2/* smnG */));
        },1);
        return B(_q/* GHC.Base.++ */(B(_5V/* GHC.Show.$wshowSignedInt */(0, E(_BV/* smns */), _4/* GHC.Types.[] */)), _C1/* smnH */));
      });
      return B(unAppCStr/* EXTERNAL */("}: tag (", _C0/* smnI */));
    },1);
    return B(_q/* GHC.Base.++ */(_BU/* smnr */, _BZ/* smnJ */));
  });
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("Enum.toEnum{", _BY/* smnK */)));});
},
_C5/* toEnumError */ = function(_C6/* smnM */, _C7/* smnN */, _C8/* smnO */, _C9/* smnP */){
  return new F(function(){return _BT/* GHC.Enum.lvl5 */(_C7/* smnN */, _C8/* smnO */, _C9/* smnP */, _C6/* smnM */);});
},
_Ca/* $fEnumWord6 */ = function(_Cb/* s1Rs9 */){
  return new F(function(){return _C5/* GHC.Enum.toEnumError */(_AP/* GHC.Word.$fShowWord32 */, _Bs/* GHC.Word.lvl4 */, _Cb/* s1Rs9 */, _BP/* GHC.Word.lvl12 */);});
},
_Cc/* $fEnumWord32_$ctoEnum */ = function(_Cd/* s1Rse */){
  var _Ce/* s1Rsf */ = E(_Cd/* s1Rse */);
  if(_Ce/* s1Rsf */<0){
    return new F(function(){return _Ca/* GHC.Word.$fEnumWord6 */(_Ce/* s1Rsf */);});
  }else{
    return _Ce/* s1Rsf */>>>0;
  }
},
_Cf/* $fEnumWord32 */ = new T(function(){
  return {_:0,a:_BM/* GHC.Word.$fEnumWord32_$csucc */,b:_BE/* GHC.Word.$fEnumWord32_$cpred */,c:_Cc/* GHC.Word.$fEnumWord32_$ctoEnum */,d:_Bv/* GHC.Word.$fEnumWord32_$cfromEnum */,e:_z3/* GHC.Word.$fEnumWord32_$cenumFrom */,f:_zV/* GHC.Word.$fEnumWord32_$cenumFromThen */,g:_Ag/* GHC.Word.$fEnumWord32_$cenumFromTo */,h:_A7/* GHC.Word.$fEnumWord32_$cenumFromThenTo */};
}),
_Cg/* $fIntegralWord32_$cdivMod */ = function(_Ch/* s1RNe */, _Ci/* s1RNf */){
  var _Cj/* s1RNg */ = E(_Ch/* s1RNe */),
  _Ck/* s1RNk */ = E(_Ci/* s1RNf */);
  return (_Ck/* s1RNk */==0) ? E(_ft/* GHC.Real.divZeroError */) : new T2(0,new T(function(){
    return quot/* EXTERNAL */(_Cj/* s1RNg */, _Ck/* s1RNk */);
  }),new T(function(){
    return _Cj/* s1RNg */%_Ck/* s1RNk */;
  }));
},
_Cl/* $fIntegralWord32_$cquot */ = function(_Cm/* s1RMM */, _Cn/* s1RMN */){
  var _Co/* s1RMS */ = E(_Cn/* s1RMN */);
  if(!_Co/* s1RMS */){
    return E(_ft/* GHC.Real.divZeroError */);
  }else{
    return new F(function(){return quot/* EXTERNAL */(E(_Cm/* s1RMM */), _Co/* s1RMS */);});
  }
},
_Cp/* $fIntegralWord32_$cquotRem */ = function(_Cq/* s1RN2 */, _Cr/* s1RN3 */){
  var _Cs/* s1RN8 */ = E(_Cr/* s1RN3 */);
  if(!_Cs/* s1RN8 */){
    return E(_ft/* GHC.Real.divZeroError */);
  }else{
    var _Ct/* s1RN9 */ = quotRemI/* EXTERNAL */(E(_Cq/* s1RN2 */), _Cs/* s1RN8 */);
    return new T2(0,_Ct/* s1RN9 */.a,_Ct/* s1RN9 */.b);
  }
},
_Cu/* $fIntegralWord32_$crem */ = function(_Cv/* s1RMU */, _Cw/* s1RMV */){
  var _Cx/* s1RN0 */ = E(_Cw/* s1RMV */);
  return (_Cx/* s1RN0 */==0) ? E(_ft/* GHC.Real.divZeroError */) : E(_Cv/* s1RMU */)%_Cx/* s1RN0 */;
},
_Cy/* integer2Word# */ = function(_Cz/* s2C */){
  return I_toInt/* EXTERNAL */(_Cz/* s2C */)>>>0;
},
_CA/* integerToWord */ = function(_CB/* s1Rr */){
  var _CC/* s1Rs */ = E(_CB/* s1Rr */);
  if(!_CC/* s1Rs */._){
    return _CC/* s1Rs */.a>>>0;
  }else{
    return new F(function(){return _Cy/* GHC.Integer.GMP.Prim.integer2Word# */(_CC/* s1Rs */.a);});
  }
},
_CD/* $cfromInteger2 */ = function(_CE/* s1Rpq */){
  return new F(function(){return _CA/* GHC.Integer.Type.integerToWord */(_CE/* s1Rpq */);});
},
_CF/* $fNumWord32_$c* */ = function(_CG/* s1Rpz */, _CH/* s1RpA */){
  return imul/* EXTERNAL */(E(_CG/* s1Rpz */), E(_CH/* s1RpA */))>>>0;
},
_CI/* $fNumWord32_$c+ */ = function(_CJ/* s1RpN */, _CK/* s1RpO */){
  return E(_CJ/* s1RpN */)+E(_CK/* s1RpO */)>>>0;
},
_CL/* $fNumWord32_$c- */ = function(_CM/* s1RpG */, _CN/* s1RpH */){
  return E(_CM/* s1RpG */)-E(_CN/* s1RpH */)>>>0;
},
_CO/* $fNumWord32_$cabs */ = function(_CP/* s1Rps */){
  return E(_CP/* s1Rps */);
},
_CQ/* $fNumWord32_$cnegate */ = function(_CR/* s1Rpt */){
  return  -(E(_CR/* s1Rpt */)&4294967295)>>>0;
},
_CS/* $fNumWord2 */ = 1,
_CT/* $fNumWord32_$csignum */ = function(_CU/* s1RNz */){
  return (E(_CU/* s1RNz */)==0) ? E(_yz/* GHC.Word.$fBitsWord4 */) : E(_CS/* GHC.Word.$fNumWord2 */);
},
_CV/* $fNumWord32 */ = {_:0,a:_CI/* GHC.Word.$fNumWord32_$c+ */,b:_CL/* GHC.Word.$fNumWord32_$c- */,c:_CF/* GHC.Word.$fNumWord32_$c* */,d:_CQ/* GHC.Word.$fNumWord32_$cnegate */,e:_CO/* GHC.Word.$fNumWord32_$cabs */,f:_CT/* GHC.Word.$fNumWord32_$csignum */,g:_CD/* GHC.Word.$cfromInteger2 */},
_CW/* $fBitsWord32_$c/= */ = function(_CX/* s1RME */, _CY/* s1RMF */){
  return (E(_CX/* s1RME */)!=E(_CY/* s1RMF */)) ? true : false;
},
_CZ/* $fEqWord32_$c== */ = function(_D0/* s1RMx */, _D1/* s1RMy */){
  return E(_D0/* s1RMx */)==E(_D1/* s1RMy */);
},
_D2/* $fEqWord32 */ = new T2(0,_CZ/* GHC.Word.$fEqWord32_$c== */,_CW/* GHC.Word.$fBitsWord32_$c/= */),
_D3/* $fOrdWord32_$c< */ = function(_D4/* s1RMg */, _D5/* s1RMh */){
  return E(_D4/* s1RMg */)<E(_D5/* s1RMh */);
},
_D6/* $fOrdWord32_$c<= */ = function(_D7/* s1RLP */, _D8/* s1RLQ */){
  return E(_D7/* s1RLP */)<=E(_D8/* s1RLQ */);
},
_D9/* $fOrdWord32_$c> */ = function(_Da/* s1RLI */, _Db/* s1RLJ */){
  return E(_Da/* s1RLI */)>E(_Db/* s1RLJ */);
},
_Dc/* $fOrdWord32_$c>= */ = function(_Dd/* s1RLB */, _De/* s1RLC */){
  return E(_Dd/* s1RLB */)>=E(_De/* s1RLC */);
},
_Df/* $fOrdWord32_$ccompare */ = function(_Dg/* s1RMn */, _Dh/* s1RMo */){
  var _Di/* s1RMp */ = E(_Dg/* s1RMn */),
  _Dj/* s1RMr */ = E(_Dh/* s1RMo */);
  return (_Di/* s1RMp */>=_Dj/* s1RMr */) ? (_Di/* s1RMp */!=_Dj/* s1RMr */) ? 2 : 1 : 0;
},
_Dk/* $fOrdWord32_$cmax */ = function(_Dl/* s1ROa */, _Dm/* s1ROb */){
  var _Dn/* s1ROc */ = E(_Dl/* s1ROa */),
  _Do/* s1ROe */ = E(_Dm/* s1ROb */);
  return (_Dn/* s1ROc */>_Do/* s1ROe */) ? E(_Dn/* s1ROc */) : E(_Do/* s1ROe */);
},
_Dp/* $fOrdWord32_$cmin */ = function(_Dq/* s1RO2 */, _Dr/* s1RO3 */){
  var _Ds/* s1RO4 */ = E(_Dq/* s1RO2 */),
  _Dt/* s1RO6 */ = E(_Dr/* s1RO3 */);
  return (_Ds/* s1RO4 */>_Dt/* s1RO6 */) ? E(_Dt/* s1RO6 */) : E(_Ds/* s1RO4 */);
},
_Du/* $fOrdWord32 */ = {_:0,a:_D2/* GHC.Word.$fEqWord32 */,b:_Df/* GHC.Word.$fOrdWord32_$ccompare */,c:_D3/* GHC.Word.$fOrdWord32_$c< */,d:_D6/* GHC.Word.$fOrdWord32_$c<= */,e:_D9/* GHC.Word.$fOrdWord32_$c> */,f:_Dc/* GHC.Word.$fOrdWord32_$c>= */,g:_Dk/* GHC.Word.$fOrdWord32_$cmax */,h:_Dp/* GHC.Word.$fOrdWord32_$cmin */},
_Dv/* $fRealWord1 */ = new T1(0,1),
_Dw/* even1 */ = new T1(0,0),
_Dx/* remInteger */ = function(_Dy/* s1NY */, _Dz/* s1NZ */){
  while(1){
    var _DA/* s1O0 */ = E(_Dy/* s1NY */);
    if(!_DA/* s1O0 */._){
      var _DB/* s1O2 */ = E(_DA/* s1O0 */.a);
      if(_DB/* s1O2 */==( -2147483648)){
        _Dy/* s1NY */ = new T1(1,I_fromInt/* EXTERNAL */( -2147483648));
        continue;
      }else{
        var _DC/* s1O3 */ = E(_Dz/* s1NZ */);
        if(!_DC/* s1O3 */._){
          return new T1(0,_DB/* s1O2 */%_DC/* s1O3 */.a);
        }else{
          _Dy/* s1NY */ = new T1(1,I_fromInt/* EXTERNAL */(_DB/* s1O2 */));
          _Dz/* s1NZ */ = _DC/* s1O3 */;
          continue;
        }
      }
    }else{
      var _DD/* s1Od */ = _DA/* s1O0 */.a,
      _DE/* s1Oe */ = E(_Dz/* s1NZ */);
      return (_DE/* s1Oe */._==0) ? new T1(1,I_rem/* EXTERNAL */(_DD/* s1Od */, I_fromInt/* EXTERNAL */(_DE/* s1Oe */.a))) : new T1(1,I_rem/* EXTERNAL */(_DD/* s1Od */, _DE/* s1Oe */.a));
    }
  }
},
_DF/* $fIntegralInteger_$crem */ = function(_DG/* svkU */, _DH/* svkV */){
  if(!B(_y7/* GHC.Integer.Type.eqInteger */(_DH/* svkV */, _Dw/* GHC.Real.even1 */))){
    return new F(function(){return _Dx/* GHC.Integer.Type.remInteger */(_DG/* svkU */, _DH/* svkV */);});
  }else{
    return E(_ft/* GHC.Real.divZeroError */);
  }
},
_DI/* $fEnumRatio_gcd' */ = function(_DJ/* svl0 */, _DK/* svl1 */){
  while(1){
    if(!B(_y7/* GHC.Integer.Type.eqInteger */(_DK/* svl1 */, _Dw/* GHC.Real.even1 */))){
      var _DL/*  svl0 */ = _DK/* svl1 */,
      _DM/*  svl1 */ = B(_DF/* GHC.Real.$fIntegralInteger_$crem */(_DJ/* svl0 */, _DK/* svl1 */));
      _DJ/* svl0 */ = _DL/*  svl0 */;
      _DK/* svl1 */ = _DM/*  svl1 */;
      continue;
    }else{
      return E(_DJ/* svl0 */);
    }
  }
},
_DN/* absInteger */ = function(_DO/* s1QP */){
  var _DP/* s1QQ */ = E(_DO/* s1QP */);
  if(!_DP/* s1QQ */._){
    var _DQ/* s1QS */ = E(_DP/* s1QQ */.a);
    return (_DQ/* s1QS */==( -2147483648)) ? E(_xB/* GHC.Integer.Type.lvl3 */) : (_DQ/* s1QS */<0) ? new T1(0, -_DQ/* s1QS */) : E(_DP/* s1QQ */);
  }else{
    var _DR/* s1QW */ = _DP/* s1QQ */.a;
    return (I_compareInt/* EXTERNAL */(_DR/* s1QW */, 0)>=0) ? E(_DP/* s1QQ */) : new T1(1,I_negate/* EXTERNAL */(_DR/* s1QW */));
  }
},
_DS/* quotInteger */ = function(_DT/* s1On */, _DU/* s1Oo */){
  while(1){
    var _DV/* s1Op */ = E(_DT/* s1On */);
    if(!_DV/* s1Op */._){
      var _DW/* s1Or */ = E(_DV/* s1Op */.a);
      if(_DW/* s1Or */==( -2147483648)){
        _DT/* s1On */ = new T1(1,I_fromInt/* EXTERNAL */( -2147483648));
        continue;
      }else{
        var _DX/* s1Os */ = E(_DU/* s1Oo */);
        if(!_DX/* s1Os */._){
          return new T1(0,quot/* EXTERNAL */(_DW/* s1Or */, _DX/* s1Os */.a));
        }else{
          _DT/* s1On */ = new T1(1,I_fromInt/* EXTERNAL */(_DW/* s1Or */));
          _DU/* s1Oo */ = _DX/* s1Os */;
          continue;
        }
      }
    }else{
      var _DY/* s1OC */ = _DV/* s1Op */.a,
      _DZ/* s1OD */ = E(_DU/* s1Oo */);
      return (_DZ/* s1OD */._==0) ? new T1(0,I_toInt/* EXTERNAL */(I_quot/* EXTERNAL */(_DY/* s1OC */, I_fromInt/* EXTERNAL */(_DZ/* s1OD */.a)))) : new T1(1,I_quot/* EXTERNAL */(_DY/* s1OC */, _DZ/* s1OD */.a));
    }
  }
},
_E0/* RatioZeroDenominator */ = 5,
_E1/* ratioZeroDenomException */ = new T(function(){
  return B(_fq/* GHC.Exception.$fExceptionArithException_$ctoException */(_E0/* GHC.Exception.RatioZeroDenominator */));
}),
_E2/* ratioZeroDenominatorError */ = new T(function(){
  return die/* EXTERNAL */(_E1/* GHC.Exception.ratioZeroDenomException */);
}),
_E3/* $w$sreduce */ = function(_E4/* svlj */, _E5/* svlk */){
  if(!B(_y7/* GHC.Integer.Type.eqInteger */(_E5/* svlk */, _Dw/* GHC.Real.even1 */))){
    var _E6/* svlm */ = B(_DI/* GHC.Real.$fEnumRatio_gcd' */(B(_DN/* GHC.Integer.Type.absInteger */(_E4/* svlj */)), B(_DN/* GHC.Integer.Type.absInteger */(_E5/* svlk */))));
    return (!B(_y7/* GHC.Integer.Type.eqInteger */(_E6/* svlm */, _Dw/* GHC.Real.even1 */))) ? new T2(0,B(_DS/* GHC.Integer.Type.quotInteger */(_E4/* svlj */, _E6/* svlm */)),B(_DS/* GHC.Integer.Type.quotInteger */(_E5/* svlk */, _E6/* svlm */))) : E(_ft/* GHC.Real.divZeroError */);
  }else{
    return E(_E2/* GHC.Real.ratioZeroDenominatorError */);
  }
},
_E7/* $w$ctoRational */ = function(_E8/* s1RrR */){
  var _E9/* s1RrS */ = _E8/* s1RrR */&4294967295;
  if(_E9/* s1RrS */<0){
    return new F(function(){return _E3/* GHC.Real.$w$sreduce */(B(_yn/* GHC.Integer.Type.timesInteger */(B(_Ah/* GHC.Integer.Type.wordToInteger */(_E8/* s1RrR */)), _Dv/* GHC.Word.$fRealWord1 */)), _Dv/* GHC.Word.$fRealWord1 */);});
  }else{
    return new F(function(){return _E3/* GHC.Real.$w$sreduce */(B(_yn/* GHC.Integer.Type.timesInteger */(B(_yl/* GHC.Integer.Type.smallInteger */(_E9/* s1RrS */)), _Dv/* GHC.Word.$fRealWord1 */)), _Dv/* GHC.Word.$fRealWord1 */);});
  }
},
_Ea/* $fRealWord32_$ctoRational */ = function(_Eb/* s1RrZ */){
  var _Ec/* s1Rs2 */ = B(_E7/* GHC.Word.$w$ctoRational */(E(_Eb/* s1RrZ */)));
  return new T2(0,E(_Ec/* s1Rs2 */.a),E(_Ec/* s1Rs2 */.b));
},
_Ed/* $fRealWord32 */ = new T3(0,_CV/* GHC.Word.$fNumWord32 */,_Du/* GHC.Word.$fOrdWord32 */,_Ea/* GHC.Word.$fRealWord32_$ctoRational */),
_z5/* $fIntegralWord32 */ = new T(function(){
  return {_:0,a:_Ed/* GHC.Word.$fRealWord32 */,b:_Cf/* GHC.Word.$fEnumWord32 */,c:_Cl/* GHC.Word.$fIntegralWord32_$cquot */,d:_Cu/* GHC.Word.$fIntegralWord32_$crem */,e:_Cl/* GHC.Word.$fIntegralWord32_$cquot */,f:_Cu/* GHC.Word.$fIntegralWord32_$crem */,g:_Cp/* GHC.Word.$fIntegralWord32_$cquotRem */,h:_Cg/* GHC.Word.$fIntegralWord32_$cdivMod */,i:_Aj/* GHC.Word.$fIntegralWord32_$ctoInteger */};
}),
_Ee/* lvl1 */ = new T1(0, -1),
_Ef/* signumInteger */ = function(_Eg/* s1OO */){
  var _Eh/* s1OP */ = E(_Eg/* s1OO */);
  if(!_Eh/* s1OP */._){
    var _Ei/* s1OQ */ = _Eh/* s1OP */.a;
    return (_Ei/* s1OQ */>=0) ? (E(_Ei/* s1OQ */)==0) ? E(_xk/* GHC.Integer.Type.lvl */) : E(_xz/* GHC.Integer.Type.log2I1 */) : E(_Ee/* GHC.Integer.Type.lvl1 */);
  }else{
    var _Ej/* s1OW */ = I_compareInt/* EXTERNAL */(_Eh/* s1OP */.a, 0);
    return (_Ej/* s1OW */<=0) ? (E(_Ej/* s1OW */)==0) ? E(_xk/* GHC.Integer.Type.lvl */) : E(_Ee/* GHC.Integer.Type.lvl1 */) : E(_xz/* GHC.Integer.Type.log2I1 */);
  }
},
_Ek/* $w$s$c/ */ = function(_El/* svlJ */, _Em/* svlK */, _En/* svlL */, _Eo/* svlM */){
  var _Ep/* svlN */ = B(_yn/* GHC.Integer.Type.timesInteger */(_Em/* svlK */, _En/* svlL */));
  return new F(function(){return _E3/* GHC.Real.$w$sreduce */(B(_yn/* GHC.Integer.Type.timesInteger */(B(_yn/* GHC.Integer.Type.timesInteger */(_El/* svlJ */, _Eo/* svlM */)), B(_Ef/* GHC.Integer.Type.signumInteger */(_Ep/* svlN */)))), B(_DN/* GHC.Integer.Type.absInteger */(_Ep/* svlN */)));});
},
_Eq/* quotRemInteger */ = function(_Er/* s1Ma */, _Es/* s1Mb */){
  while(1){
    var _Et/* s1Mc */ = E(_Er/* s1Ma */);
    if(!_Et/* s1Mc */._){
      var _Eu/* s1Me */ = E(_Et/* s1Mc */.a);
      if(_Eu/* s1Me */==( -2147483648)){
        _Er/* s1Ma */ = new T1(1,I_fromInt/* EXTERNAL */( -2147483648));
        continue;
      }else{
        var _Ev/* s1Mf */ = E(_Es/* s1Mb */);
        if(!_Ev/* s1Mf */._){
          var _Ew/* s1Mg */ = _Ev/* s1Mf */.a;
          return new T2(0,new T1(0,quot/* EXTERNAL */(_Eu/* s1Me */, _Ew/* s1Mg */)),new T1(0,_Eu/* s1Me */%_Ew/* s1Mg */));
        }else{
          _Er/* s1Ma */ = new T1(1,I_fromInt/* EXTERNAL */(_Eu/* s1Me */));
          _Es/* s1Mb */ = _Ev/* s1Mf */;
          continue;
        }
      }
    }else{
      var _Ex/* s1Mt */ = E(_Es/* s1Mb */);
      if(!_Ex/* s1Mt */._){
        _Er/* s1Ma */ = _Et/* s1Mc */;
        _Es/* s1Mb */ = new T1(1,I_fromInt/* EXTERNAL */(_Ex/* s1Mt */.a));
        continue;
      }else{
        var _Ey/* s1MA */ = I_quotRem/* EXTERNAL */(_Et/* s1Mc */.a, _Ex/* s1Mt */.a);
        return new T2(0,new T1(1,_Ey/* s1MA */.a),new T1(1,_Ey/* s1MA */.b));
      }
    }
  }
},
_Ez/* $w$s$cproperFraction */ = function(_EA/* svzS */, _EB/* svzT */, _EC/* svzU */){
  var _ED/* svzV */ = new T(function(){
    if(!B(_y7/* GHC.Integer.Type.eqInteger */(_EC/* svzU */, _Dw/* GHC.Real.even1 */))){
      var _EE/* svzX */ = B(_Eq/* GHC.Integer.Type.quotRemInteger */(_EB/* svzT */, _EC/* svzU */));
      return new T2(0,_EE/* svzX */.a,_EE/* svzX */.b);
    }else{
      return E(_ft/* GHC.Real.divZeroError */);
    }
  }),
  _EF/* svA6 */ = new T(function(){
    return B(A2(_yH/* GHC.Num.fromInteger */,B(_yF/* GHC.Real.$p1Real */(B(_yD/* GHC.Real.$p1Integral */(_EA/* svzS */)))), new T(function(){
      return E(E(_ED/* svzV */).a);
    })));
  });
  return new T2(0,_EF/* svA6 */,new T(function(){
    return new T2(0,E(E(_ED/* svzV */).b),E(_EC/* svzU */));
  }));
},
_EG/* $fRealFracNominalDiffTime_$ctruncate */ = function(_EH/* sjkL */, _EI/* sjkM */){
  var _EJ/* sjkN */ = B(_Ek/* GHC.Real.$w$s$c/ */(_EI/* sjkM */, _yC/* GHC.Real.$fEnumRatio1 */, _xN/* Data.Fixed.$fHasResolutionE5 */, _yC/* GHC.Real.$fEnumRatio1 */));
  return E(B(_Ez/* GHC.Real.$w$s$cproperFraction */(_EH/* sjkL */, _EJ/* sjkN */.a, _EJ/* sjkN */.b)).a);
},
_EK/* $w$cshiftL */ = function(_EL/* s1Rz0 */, _EM/* s1Rz1 */){
  if(_EM/* s1Rz1 */<64){
    var _EN/* s1Rz5 */ = hs_uncheckedShiftL64/* EXTERNAL */(_EL/* s1Rz0 */, _EM/* s1Rz1 */);
    return E(_EN/* s1Rz5 */);
  }else{
    var _EO/* s1Rz9 */ = hs_wordToWord64/* EXTERNAL */(0);
    return E(_EO/* s1Rz9 */);
  }
},
_EP/* lvl5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Negative exponent"));
}),
_EQ/* ^1 */ = new T(function(){
  return B(err/* EXTERNAL */(_EP/* GHC.Real.lvl5 */));
}),
_ER/* even2 */ = new T1(0,2),
_ES/* even3 */ = new T(function(){
  return B(_y7/* GHC.Integer.Type.eqInteger */(_ER/* GHC.Real.even2 */, _Dw/* GHC.Real.even1 */));
}),
_ET/* g */ = function(_EU/* svB9 */, _EV/* svBa */, _EW/* svBb */){
  while(1){
    if(!E(_ES/* GHC.Real.even3 */)){
      if(!B(_y7/* GHC.Integer.Type.eqInteger */(B(_Dx/* GHC.Integer.Type.remInteger */(_EV/* svBa */, _ER/* GHC.Real.even2 */)), _Dw/* GHC.Real.even1 */))){
        if(!B(_y7/* GHC.Integer.Type.eqInteger */(_EV/* svBa */, _yC/* GHC.Real.$fEnumRatio1 */))){
          var _EX/*  svB9 */ = B(_yn/* GHC.Integer.Type.timesInteger */(_EU/* svB9 */, _EU/* svB9 */)),
          _EY/*  svBa */ = B(_DS/* GHC.Integer.Type.quotInteger */(B(_zx/* GHC.Integer.Type.minusInteger */(_EV/* svBa */, _yC/* GHC.Real.$fEnumRatio1 */)), _ER/* GHC.Real.even2 */)),
          _EZ/*  svBb */ = B(_yn/* GHC.Integer.Type.timesInteger */(_EU/* svB9 */, _EW/* svBb */));
          _EU/* svB9 */ = _EX/*  svB9 */;
          _EV/* svBa */ = _EY/*  svBa */;
          _EW/* svBb */ = _EZ/*  svBb */;
          continue;
        }else{
          return new F(function(){return _yn/* GHC.Integer.Type.timesInteger */(_EU/* svB9 */, _EW/* svBb */);});
        }
      }else{
        var _EX/*  svB9 */ = B(_yn/* GHC.Integer.Type.timesInteger */(_EU/* svB9 */, _EU/* svB9 */)),
        _EY/*  svBa */ = B(_DS/* GHC.Integer.Type.quotInteger */(_EV/* svBa */, _ER/* GHC.Real.even2 */));
        _EU/* svB9 */ = _EX/*  svB9 */;
        _EV/* svBa */ = _EY/*  svBa */;
        continue;
      }
    }else{
      return E(_ft/* GHC.Real.divZeroError */);
    }
  }
},
_F0/* ^_f */ = function(_F1/* svBn */, _F2/* svBo */){
  while(1){
    if(!E(_ES/* GHC.Real.even3 */)){
      if(!B(_y7/* GHC.Integer.Type.eqInteger */(B(_Dx/* GHC.Integer.Type.remInteger */(_F2/* svBo */, _ER/* GHC.Real.even2 */)), _Dw/* GHC.Real.even1 */))){
        if(!B(_y7/* GHC.Integer.Type.eqInteger */(_F2/* svBo */, _yC/* GHC.Real.$fEnumRatio1 */))){
          return new F(function(){return _ET/* GHC.Real.g */(B(_yn/* GHC.Integer.Type.timesInteger */(_F1/* svBn */, _F1/* svBn */)), B(_DS/* GHC.Integer.Type.quotInteger */(B(_zx/* GHC.Integer.Type.minusInteger */(_F2/* svBo */, _yC/* GHC.Real.$fEnumRatio1 */)), _ER/* GHC.Real.even2 */)), _F1/* svBn */);});
        }else{
          return E(_F1/* svBn */);
        }
      }else{
        var _F3/*  svBn */ = B(_yn/* GHC.Integer.Type.timesInteger */(_F1/* svBn */, _F1/* svBn */)),
        _F4/*  svBo */ = B(_DS/* GHC.Integer.Type.quotInteger */(_F2/* svBo */, _ER/* GHC.Real.even2 */));
        _F1/* svBn */ = _F3/*  svBn */;
        _F2/* svBo */ = _F4/*  svBo */;
        continue;
      }
    }else{
      return E(_ft/* GHC.Real.divZeroError */);
    }
  }
},
_F5/* ^_$s^ */ = function(_F6/* svBz */, _F7/* svBA */){
  if(!B(_z7/* GHC.Integer.Type.ltInteger */(_F7/* svBA */, _Dw/* GHC.Real.even1 */))){
    if(!B(_y7/* GHC.Integer.Type.eqInteger */(_F7/* svBA */, _Dw/* GHC.Real.even1 */))){
      return new F(function(){return _F0/* GHC.Real.^_f */(_F6/* svBz */, _F7/* svBA */);});
    }else{
      return E(_yC/* GHC.Real.$fEnumRatio1 */);
    }
  }else{
    return E(_EQ/* GHC.Real.^1 */);
  }
},
_F8/* cpuTimePrecision1 */ = new T1(0,64),
_F9/* cpuTimePrecision2 */ = new T1(0,2),
_Fa/* cpuTimePrecision */ = new T(function(){
  return B(_F5/* GHC.Real.^_$s^ */(_F9/* System.CPUTime.cpuTimePrecision2 */, _F8/* System.CPUTime.cpuTimePrecision1 */));
}),
_Fb/* getCPUTime2 */ = new T1(0,0),
_Fc/* initSMGen3 */ = new T(function(){
  return B(_xZ/* GHC.Integer.Type.divInteger */(_Fb/* System.CPUTime.getCPUTime2 */, _Fa/* System.CPUTime.cpuTimePrecision */));
}),
_Fd/* initSMGen5 */ = new T1(0,0),
_Fe/* initSMGen4 */ = new T(function(){
  return B(_y7/* GHC.Integer.Type.eqInteger */(_Fa/* System.CPUTime.cpuTimePrecision */, _Fd/* System.Random.SplitMix.initSMGen5 */));
}),
_Ff/* initSMGen2 */ = function(_Fg/* saXA */, _/* EXTERNAL */){
  return new T(function(){
    if(!E(_Fe/* System.Random.SplitMix.initSMGen4 */)){
      var _Fh/* saXF */ = hs_wordToWord64/* EXTERNAL */(B(_CA/* GHC.Integer.Type.integerToWord */(_Fc/* System.Random.SplitMix.initSMGen3 */))),
      _Fi/* saXM */ = hs_wordToWord64/* EXTERNAL */(B(_EG/* Data.Time.Clock.UTC.$fRealFracNominalDiffTime_$ctruncate */(_z5/* GHC.Word.$fIntegralWord32 */, _Fg/* saXA */)));
      return hs_or64/* EXTERNAL */(B(_EK/* GHC.Word.$w$cshiftL */(_Fh/* saXF */, 32)), _Fi/* saXM */);
    }else{
      return E(_ft/* GHC.Real.divZeroError */);
    }
  });
},
_Fj/* mkSMGen */ = function(_Fk/* saZo */){
  var _Fl/* saZp */ = E(_Fk/* saZo */);
  return new T2(0,B(_e4/* System.Random.SplitMix.$wmix64 */(_Fl/* saZp */)),B(_gC/* System.Random.SplitMix.$wmixGamma */(B(_dL/* GHC.Word.$w$c+ */(_Fl/* saZp */, new Long/* EXTERNAL */(2135587861, 2654435769, true))))));
},
_Fm/* lvl */ = function(_/* EXTERNAL */){
  var _Fn/* szS3 */ = B(_yv/* Data.Time.Clock.POSIX.getPOSIXTime1 */(0)),
  _Fo/* szS6 */ = B(_Ff/* System.Random.SplitMix.initSMGen2 */(_Fn/* szS3 */, 0));
  return new F(function(){return nMV/* EXTERNAL */(new T(function(){
    return B(_Fj/* System.Random.SplitMix.mkSMGen */(_Fo/* szS6 */));
  }));});
},
_Fp/* theStdGen */ = new T(function(){
  return B(_dB/* GHC.IO.unsafeDupablePerformIO */(_Fm/* System.Random.lvl */));
}),
_Fq/* a55 */ = function(_Fr/* siAo */, _Fs/* siAp */, _/* EXTERNAL */){
  var _Ft/* siAt */ = mMV/* EXTERNAL */(E(_Fp/* System.Random.theStdGen */), _xg/* LudoJS.lvl41 */),
  _Fu/* siAw */ = E(_Ft/* siAt */),
  _Fv/* siAy */ = B(_nZ/* LudoJS.a42 */(_Fu/* siAw */, _Fs/* siAp */, _/* EXTERNAL */)),
  _Fw/* siAB */ = E(_Fv/* siAy */),
  _Fx/* siAD */ = _Fw/* siAB */.b,
  _Fy/* siAE */ = E(_Fw/* siAB */.a);
  if(!_Fy/* siAE */._){
    var _Fz/* siAF */ = E(_Fx/* siAD */),
    _FA/* siAR */ = B(_x2/* LudoJS.$wa14 */(new T1(0,new T1(1,_Fu/* siAw */)), _Fz/* siAF */.b, new T(function(){
      return E(_Fz/* siAF */.c)-1|0;
    }), _Fz/* siAF */.d, _Fz/* siAF */.e, _/* EXTERNAL */)),
    _FB/* siAX */ = E(E(_FA/* siAR */).b),
    _FC/* siAY */ = _FB/* siAX */.a,
    _FD/* siB3 */ = new T5(0,_FC/* siAY */,_FB/* siAX */.b,_FB/* siAX */.c,_FB/* siAX */.d,_FB/* siAX */.e),
    _FE/* siB4 */ = B(_lH/* LudoJS.$w$ctoAny */(_FD/* siB3 */)),
    _FF/* siB8 */ = __lst2arr/* EXTERNAL */(B(_lD/* GHC.Base.map */(_nX/* LudoJS.$fToAnyOption_$ctoAny */, _4/* GHC.Types.[] */))),
    _FG/* siBb */ = E(_FC/* siAY */);
    switch(_FG/* siBb */._){
      case 0:
        var _FH/* siBd */ = E(_FG/* siBb */.a);
        if(!_FH/* siBd */._){
          var _FI/* siBn */ = __app4/* EXTERNAL */(E(_xf/* LudoJS.f1 */), _FE/* siB4 */, _FF/* siB8 */,  -1, E(_Fr/* siAo */));
          return new T2(0,_eb/* GHC.Tuple.() */,_FD/* siB3 */);
        }else{
          var _FJ/* siBD */ = __app4/* EXTERNAL */(E(_xf/* LudoJS.f1 */), _FE/* siB4 */, _FF/* siB8 */, E(_FH/* siBd */.a), E(_Fr/* siAo */));
          return new T2(0,_eb/* GHC.Tuple.() */,_FD/* siB3 */);
        }
        break;
      case 1:
        var _FK/* siBT */ = __app4/* EXTERNAL */(E(_xf/* LudoJS.f1 */), _FE/* siB4 */, _FF/* siB8 */, E(_FG/* siBb */.a), E(_Fr/* siAo */));
        return new T2(0,_eb/* GHC.Tuple.() */,_FD/* siB3 */);
      case 2:
        var _FL/* siCa */ = __app4/* EXTERNAL */(E(_xf/* LudoJS.f1 */), _FE/* siB4 */, _FF/* siB8 */, E(_FG/* siBb */.a), E(_Fr/* siAo */));
        return new T2(0,_eb/* GHC.Tuple.() */,_FD/* siB3 */);
      default:
        return E(_oW/* LudoJS.lvl13 */);
    }
  }else{
    var _FM/* siCh */ = E(_Fx/* siAD */),
    _FN/* siCr */ = new T5(0,new T1(1,_Fu/* siAw */),_FM/* siCh */.b,new T(function(){
      if(E(_Fu/* siAw */)==6){
        return E(_di/* LudoJS.numPiecesAt2 */);
      }else{
        return E(_d7/* LudoJS.$fShowStage2 */);
      }
    }),_FM/* siCh */.d,_FM/* siCh */.e),
    _FO/* siCw */ = __lst2arr/* EXTERNAL */(B(_lD/* GHC.Base.map */(_nX/* LudoJS.$fToAnyOption_$ctoAny */, _Fy/* siAE */))),
    _FP/* siCI */ = __app4/* EXTERNAL */(E(_xf/* LudoJS.f1 */), B(_lH/* LudoJS.$w$ctoAny */(_FN/* siCr */)), _FO/* siCw */, _Fu/* siAw */, E(_Fr/* siAo */));
    return new T2(0,_eb/* GHC.Tuple.() */,_FN/* siCr */);
  }
},
_FQ/* f3 */ = new T(function(){
  return eval/* EXTERNAL */("(() => gameState)");
}),
_FR/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */("piece does not exist"));
}),
_FS/* lvl1 */ = new T(function(){
  return B(err/* EXTERNAL */(_FR/* LudoJS.lvl */));
}),
_FT/* lvl2 */ = "((x, y, player) => posToField(x, y, player))",
_FU/* lvl42 */ = function(_FV/* siEi */){
  var _FW/* siEj */ = B(_gY/* System.Random.$w$crandomR12 */(_gV/* System.Random.Internal.$fRandomGenStdGen */, 1, 4, _FV/* siEi */));
  return new T2(0,E(_FW/* siEj */.b),_FW/* siEj */.a);
},
_FX/* lvl9 */ = new T2(1,_1P/* LudoJS.Green */,_6b/* LudoJS.lvl8 */),
_FY/* play9 */ = new T2(1,_1S/* LudoJS.Yellow */,_4/* GHC.Types.[] */),
_FZ/* play8 */ = new T2(1,_1Q/* LudoJS.Red */,_FY/* LudoJS.play9 */),
_G0/* play7 */ = new T2(1,_1P/* LudoJS.Green */,_FZ/* LudoJS.play8 */),
_G1/* lvl15 */ = new T2(0,_61/* LudoJS.lvl14 */,_60/* LudoJS.Out */),
_G2/* lvl16 */ = new T2(1,_G1/* LudoJS.lvl15 */,_4/* GHC.Types.[] */),
_G3/* lvl17 */ = new T2(0,_63/* LudoJS.play10 */,_60/* LudoJS.Out */),
_G4/* lvl18 */ = new T2(1,_G3/* LudoJS.lvl17 */,_G2/* LudoJS.lvl16 */),
_G5/* lvl20 */ = new T2(0,_65/* LudoJS.lvl19 */,_60/* LudoJS.Out */),
_G6/* lvl21 */ = new T2(1,_G5/* LudoJS.lvl20 */,_G4/* LudoJS.lvl18 */),
_G7/* lvl22 */ = new T2(0,_di/* LudoJS.numPiecesAt2 */,_60/* LudoJS.Out */),
_G8/* lvl23 */ = new T2(1,_G7/* LudoJS.lvl22 */,_G6/* LudoJS.lvl21 */),
_G9/* go */ = function(_Ga/* shJJ */){
  var _Gb/* shJK */ = E(_Ga/* shJJ */);
  return (_Gb/* shJK */._==0) ? __Z/* EXTERNAL */ : new T2(1,new T2(0,_Gb/* shJK */.a,_G8/* LudoJS.lvl23 */),new T(function(){
    return B(_G9/* LudoJS.go */(_Gb/* shJK */.b));
  }));
},
_Gc/* play_$sgo */ = function(_Gd/* shJF */, _Ge/* shJG */){
  return new T2(1,new T2(0,_Gd/* shJF */,_G8/* LudoJS.lvl23 */),new T(function(){
    return B(_G9/* LudoJS.go */(_Ge/* shJG */));
  }));
},
_Gf/* play6 */ = new T(function(){
  return B(_Gc/* LudoJS.play_$sgo */(_1O/* LudoJS.Blue */, _G0/* LudoJS.play7 */));
}),
_Gg/* play5 */ = new T(function(){
  return B(_5z/* LudoJS.$sfromList */(_Gf/* LudoJS.play6 */));
}),
_Gh/* play_f1 */ = new T(function(){
  return eval/* EXTERNAL */("((gs) => gameState=gs)");
}),
_Gi/* $wlvl */ = function(_Gj/* siEp */, _Gk/* siEq */, _Gl/* siEr */, _Gm/* siEs */, _/* EXTERNAL */){
  var _Gn/* siEu */ = E(_Gl/* siEr */);
  if(!_Gn/* siEu */._){
    return _eb/* GHC.Tuple.() */;
  }else{
    if(!E(_Gn/* siEu */.a)){
      var _Go/* siEx */ = E(_FQ/* LudoJS.f3 */),
      _Gp/* siEA */ = __app0/* EXTERNAL */(_Go/* siEx */),
      _Gq/* siED */ = B(_cU/* LudoJS.$wa1 */(_Gp/* siEA */, _/* EXTERNAL */)),
      _Gr/* siEU */ = function(_Gs/* siEV */){
        var _Gt/* siF3 */ = eval/* EXTERNAL */(E(_FT/* LudoJS.lvl2 */)),
        _Gu/* siFb */ = __app3/* EXTERNAL */(E(_Gt/* siF3 */), E(_Gj/* siEp */), E(_Gk/* siEq */), toJSStr/* EXTERNAL */(_Gs/* siEV */)),
        _Gv/* siFh */ = __eq/* EXTERNAL */(_Gu/* siFb */, E(_dF/* Haste.Prim.Any.jsNull */));
        if(!E(_Gv/* siFh */)){
          var _Gw/* siFm */ = __isUndef/* EXTERNAL */(_Gu/* siFb */);
          if(!E(_Gw/* siFm */)){
            var _Gx/* siFq */ = new T(function(){
              var _Gy/* siFs */ = Number/* EXTERNAL */(_Gu/* siFb */);
              return jsTrunc/* EXTERNAL */(_Gy/* siFs */);
            }),
            _Gz/* siFA */ = __app0/* EXTERNAL */(_Go/* siEx */),
            _GA/* siFD */ = B(_cU/* LudoJS.$wa1 */(_Gz/* siFA */, _/* EXTERNAL */)),
            _GB/* siFG */ = E(_GA/* siFD */),
            _GC/* siFI */ = _GB/* siFG */.b,
            _GD/* siFJ */ = _GB/* siFG */.c,
            _GE/* siFK */ = _GB/* siFG */.d,
            _GF/* siFL */ = _GB/* siFG */.e,
            _GG/* siFM */ = E(_GB/* siFG */.a);
            switch(_GG/* siFM */._){
              case 0:
                if(E(_Gx/* siFq */)==( -5)){
                  var _GH/* siFY */ = E(_GG/* siFM */.a);
                  if(!_GH/* siFY */._){
                    var _GI/* siFZ */ = B(_Fq/* LudoJS.a55 */(_kX/* LudoJS.$fToAnyGameState18 */, _GB/* siFG */, _/* EXTERNAL */)),
                    _GJ/* siGf */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_GI/* siFZ */).b))));
                    return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                  }else{
                    var _GK/* siGj */ = B(_Fq/* LudoJS.a55 */(_GH/* siFY */.a, _GB/* siFG */, _/* EXTERNAL */)),
                    _GL/* siGz */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_GK/* siGj */).b))));
                    return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                  }
                }else{
                  var _GM/* siFV */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(_GB/* siFG */)));
                  return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                }
                break;
              case 1:
                var _GN/* siGD */ = E(_Gx/* siFq */),
                _GO/* siGF */ = function(_/* EXTERNAL */, _GP/* siGH */, _GQ/* siGI */, _GR/* siGJ */, _GS/* siGK */, _GT/* siGL */){
                  var _GU/* siGN */ = B(_oX/* LudoJS.$wa16 */(_GP/* siGH */, _GQ/* siGI */, _GR/* siGJ */, _GS/* siGK */, _GT/* siGL */, new T5(0,_GP/* siGH */,_GQ/* siGI */,_GR/* siGJ */,_GS/* siGK */,_GT/* siGL */), _/* EXTERNAL */)),
                  _GV/* siH3 */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_GU/* siGN */).b))));
                  return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                },
                _GW/* siH6 */ = function(_/* EXTERNAL */, _GX/* siH8 */, _GY/* siH9 */, _GZ/* siHa */, _H0/* siHb */, _H1/* siHc */, _H2/* siHd */){
                  var _H3/* siHe */ = E(_GX/* siH8 */);
                  if(!_H3/* siHe */._){
                    var _H4/* siHg */ = B(_oX/* LudoJS.$wa16 */(_GY/* siH9 */, _GZ/* siHa */, _H0/* siHb */, _H1/* siHc */, _H2/* siHd */, new T5(0,_GY/* siH9 */,_GZ/* siHa */,_H0/* siHb */,_H1/* siHc */,_H2/* siHd */), _/* EXTERNAL */)),
                    _H5/* siHw */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_H4/* siHg */).b))));
                    return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                  }else{
                    var _H6/* siHz */ = _H3/* siHe */.a,
                    _H7/* siHA */ = function(_H8/* siHB */){
                      var _H9/* siHD */ = B(_oX/* LudoJS.$wa16 */(_GY/* siH9 */, _GZ/* siHa */, _H0/* siHb */, _H1/* siHc */, _H2/* siHd */, new T5(0,_GY/* siH9 */,_GZ/* siHa */,_H0/* siHb */,_H1/* siHc */,_H2/* siHd */), _/* EXTERNAL */)),
                      _Ha/* siHT */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_H9/* siHD */).b))));
                      return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                    },
                    _Hb/* siHW */ = function(_Hc/* siHX */){
                      var _Hd/* siHY */ = new T2(2,_GG/* siFM */.a,_H6/* siHz */),
                      _He/* siI0 */ = B(_oX/* LudoJS.$wa16 */(_Hd/* siHY */, _GZ/* siHa */, _H0/* siHb */, _H1/* siHc */, _H2/* siHd */, new T5(0,_Hd/* siHY */,_GZ/* siHa */,_H0/* siHb */,_H1/* siHc */,_H2/* siHd */), _/* EXTERNAL */)),
                      _Hf/* siIg */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_He/* siI0 */).b))));
                      return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                    },
                    _Hg/* siIj */ = B(_da/* LudoJS.$s!1 */(_GZ/* siHa */, _H1/* siHc */));
                    if(!_Hg/* siIj */._){
                      return new F(function(){return _H7/* siHA */(_/* EXTERNAL */);});
                    }else{
                      var _Hh/* siIr */ = E(_H6/* siHz */);
                      if(E(E(_Hg/* siIj */.a).a)!=_Hh/* siIr */){
                        var _Hi/* siIv */ = function(_Hj/* siIw */){
                          while(1){
                            var _Hk/* siIx */ = E(_Hj/* siIw */);
                            if(!_Hk/* siIx */._){
                              return false;
                            }else{
                              if(E(E(_Hk/* siIx */.a).a)!=_Hh/* siIr */){
                                _Hj/* siIw */ = _Hk/* siIx */.b;
                                continue;
                              }else{
                                return true;
                              }
                            }
                          }
                        };
                        if(!B(_Hi/* siIv */(_Hg/* siIj */.b))){
                          return new F(function(){return _H7/* siHA */(_/* EXTERNAL */);});
                        }else{
                          return new F(function(){return _Hb/* siHW */(_/* EXTERNAL */);});
                        }
                      }else{
                        return new F(function(){return _Hb/* siHW */(_/* EXTERNAL */);});
                      }
                    }
                  }
                };
                if(_GN/* siGD */<( -4)){
                  if(_GN/* siGD */<0){
                    return new F(function(){return _GO/* siGF */(_/* EXTERNAL */, _GG/* siFM */, _GC/* siFI */, _GD/* siFJ */, _GE/* siFK */, _GF/* siFL */);});
                  }else{
                    if(_GN/* siGD */>55){
                      return new F(function(){return _GO/* siGF */(_/* EXTERNAL */, _GG/* siFM */, _GC/* siFI */, _GD/* siFJ */, _GE/* siFK */, _GF/* siFL */);});
                    }else{
                      var _Hl/* siIO */ = function(_Hm/* siIP */){
                        while(1){
                          var _Hn/* siIQ */ = E(_Hm/* siIP */);
                          if(!_Hn/* siIQ */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _Ho/* siIS */ = _Hn/* siIQ */.b,
                            _Hp/* siIT */ = E(_Hn/* siIQ */.a),
                            _Hq/* siIW */ = E(_Hp/* siIT */.b);
                            if(!_Hq/* siIW */._){
                              _Hm/* siIP */ = _Ho/* siIS */;
                              continue;
                            }else{
                              if(_GN/* siGD */!=E(_Hq/* siIW */.a)){
                                _Hm/* siIP */ = _Ho/* siIS */;
                                continue;
                              }else{
                                return new T1(1,_Hp/* siIT */.a);
                              }
                            }
                          }
                        }
                      };
                      return new F(function(){return _GW/* siH6 */(_/* EXTERNAL */, B(_Hl/* siIO */(B(_da/* LudoJS.$s!1 */(_GC/* siFI */, _GE/* siFK */)))), _GG/* siFM */, _GC/* siFI */, _GD/* siFJ */, _GE/* siFK */, _GF/* siFL */);});
                    }
                  }
                }else{
                  if(_GN/* siGD */>( -1)){
                    if(_GN/* siGD */<0){
                      return new F(function(){return _GO/* siGF */(_/* EXTERNAL */, _GG/* siFM */, _GC/* siFI */, _GD/* siFJ */, _GE/* siFK */, _GF/* siFL */);});
                    }else{
                      if(_GN/* siGD */>55){
                        return new F(function(){return _GO/* siGF */(_/* EXTERNAL */, _GG/* siFM */, _GC/* siFI */, _GD/* siFJ */, _GE/* siFK */, _GF/* siFL */);});
                      }else{
                        var _Hr/* siJa */ = function(_Hs/* siJb */){
                          while(1){
                            var _Ht/* siJc */ = E(_Hs/* siJb */);
                            if(!_Ht/* siJc */._){
                              return __Z/* EXTERNAL */;
                            }else{
                              var _Hu/* siJe */ = _Ht/* siJc */.b,
                              _Hv/* siJf */ = E(_Ht/* siJc */.a),
                              _Hw/* siJi */ = E(_Hv/* siJf */.b);
                              if(!_Hw/* siJi */._){
                                _Hs/* siJb */ = _Hu/* siJe */;
                                continue;
                              }else{
                                if(_GN/* siGD */!=E(_Hw/* siJi */.a)){
                                  _Hs/* siJb */ = _Hu/* siJe */;
                                  continue;
                                }else{
                                  return new T1(1,_Hv/* siJf */.a);
                                }
                              }
                            }
                          }
                        };
                        return new F(function(){return _GW/* siH6 */(_/* EXTERNAL */, B(_Hr/* siJa */(B(_da/* LudoJS.$s!1 */(_GC/* siFI */, _GE/* siFK */)))), _GG/* siFM */, _GC/* siFI */, _GD/* siFJ */, _GE/* siFK */, _GF/* siFL */);});
                      }
                    }
                  }else{
                    var _Hx/* siJq */ = _GN/* siGD */+5|0,
                    _Hy/* siJt */ = function(_Hz/* siJu */){
                      while(1){
                        var _HA/* siJv */ = E(_Hz/* siJu */);
                        if(!_HA/* siJv */._){
                          return __Z/* EXTERNAL */;
                        }else{
                          var _HB/* siJx */ = _HA/* siJv */.b,
                          _HC/* siJy */ = E(_HA/* siJv */.a);
                          if(E(_HC/* siJy */.a)!=_Hx/* siJq */){
                            _Hz/* siJu */ = _HB/* siJx */;
                            continue;
                          }else{
                            if(!E(_HC/* siJy */.b)._){
                              return E(new T1(1,_Hx/* siJq */));
                            }else{
                              _Hz/* siJu */ = _HB/* siJx */;
                              continue;
                            }
                          }
                        }
                      }
                    };
                    return new F(function(){return _GW/* siH6 */(_/* EXTERNAL */, B(_Hy/* siJt */(B(_da/* LudoJS.$s!1 */(_GC/* siFI */, _GE/* siFK */)))), _GG/* siFM */, _GC/* siFI */, _GD/* siFJ */, _GE/* siFK */, _GF/* siFL */);});
                  }
                }
                break;
              case 2:
                var _HD/* siJJ */ = _GG/* siFM */.a,
                _HE/* siJK */ = _GG/* siFM */.b,
                _HF/* siJL */ = E(_Gx/* siFq */);
                if(_HF/* siJL */>56){
                  var _HG/* siJT */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(_GB/* siFG */)));
                  return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                }else{
                  if(_HF/* siJL */<0){
                    var _HH/* siK2 */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(_GB/* siFG */)));
                    return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                  }else{
                    var _HI/* siK5 */ = B(_nZ/* LudoJS.a42 */(_HD/* siJJ */, _GB/* siFG */, _/* EXTERNAL */)),
                    _HJ/* siK8 */ = E(_HI/* siK5 */),
                    _HK/* siKa */ = _HJ/* siK8 */.b,
                    _HL/* siKb */ = new T(function(){
                      var _HM/* siKc */ = new T2(1,_HE/* siJK */,_HF/* siJL */),
                      _HN/* siKd */ = B(_da/* LudoJS.$s!1 */(_GC/* siFI */, _GE/* siFK */));
                      if(!_HN/* siKd */._){
                        return E(_FS/* LudoJS.lvl1 */);
                      }else{
                        var _HO/* siKg */ = E(_HN/* siKd */.a),
                        _HP/* siKj */ = E(_HO/* siKg */.a),
                        _HQ/* siKl */ = E(_HE/* siJK */);
                        if(_HP/* siKj */!=_HQ/* siKl */){
                          var _HR/* siKp */ = function(_HS/* siKq */){
                            while(1){
                              var _HT/* siKr */ = E(_HS/* siKq */);
                              if(!_HT/* siKr */._){
                                return E(_FS/* LudoJS.lvl1 */);
                              }else{
                                var _HU/* siKu */ = E(_HT/* siKr */.a),
                                _HV/* siKx */ = E(_HU/* siKu */.a);
                                if(_HV/* siKx */!=_HQ/* siKl */){
                                  _HS/* siKq */ = _HT/* siKr */.b;
                                  continue;
                                }else{
                                  return (E(_HU/* siKu */.b)._==0) ? new T1(0,_HV/* siKx */) : E(_HM/* siKc */);
                                }
                              }
                            }
                          };
                          return B(_HR/* siKp */(_HN/* siKd */.b));
                        }else{
                          if(!E(_HO/* siKg */.b)._){
                            return new T1(0,_HP/* siKj */);
                          }else{
                            return E(_HM/* siKc */);
                          }
                        }
                      }
                    }),
                    _HW/* siKF */ = function(_/* EXTERNAL */, _HX/* siKH */, _HY/* siKI */, _HZ/* siKJ */, _I0/* siKK */, _I1/* siKL */, _I2/* siKM */){
                      if(!E(_HX/* siKH */)){
                        var _I3/* siKO */ = new T1(1,_HD/* siJJ */),
                        _I4/* siKQ */ = B(_oX/* LudoJS.$wa16 */(_I3/* siKO */, _HZ/* siKJ */, _I0/* siKK */, _I1/* siKL */, _I2/* siKM */, new T5(0,_I3/* siKO */,_HZ/* siKJ */,_I0/* siKK */,_I1/* siKL */,_I2/* siKM */), _/* EXTERNAL */)),
                        _I5/* siL6 */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_I4/* siKQ */).b))));
                        return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                      }else{
                        var _I6/* siL9 */ = B(_nC/* LudoJS.$wa15 */(_HY/* siKI */, _HZ/* siKJ */, _I0/* siKK */, _I1/* siKL */, _I2/* siKM */, _/* EXTERNAL */)),
                        _I7/* siLf */ = E(E(_I6/* siL9 */).b),
                        _I8/* siLl */ = B(_oX/* LudoJS.$wa16 */(_I7/* siLf */.a, _I7/* siLf */.b, _I7/* siLf */.c, _I7/* siLf */.d, _I7/* siLf */.e, _I7/* siLf */, _/* EXTERNAL */)),
                        _I9/* siLB */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_I8/* siLl */).b))));
                        return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                      }
                    };
                    if(!B(_uw/* GHC.List.elem */(_mJ/* LudoJS.$fEqOption */, _HL/* siKb */, _HJ/* siK8 */.a))){
                      var _Ia/* siLF */ = E(_HK/* siKa */);
                      return new F(function(){return _HW/* siKF */(_/* EXTERNAL */, _qp/* GHC.Types.False */, _Ia/* siLF */.a, _Ia/* siLF */.b, _Ia/* siLF */.c, _Ia/* siLF */.d, _Ia/* siLF */.e);});
                    }else{
                      var _Ib/* siLL */ = function(_/* EXTERNAL */, _Ic/* siLN */, _Id/* siLO */, _Ie/* siLP */, _If/* siLQ */, _Ig/* siLR */){
                        if(!B(_mL/* GHC.List.$wlenAcc */(B(_da/* LudoJS.$s!1 */(_Id/* siLO */, _If/* siLQ */)), 0))){
                          var _Ih/* siMz */ = B(_q/* GHC.Base.++ */(_Ig/* siLR */, new T2(1,_Id/* siLO */,_4/* GHC.Types.[] */)));
                          if(B(_mL/* GHC.List.$wlenAcc */(_Ih/* siMz */, 0))==3){
                            var _Ii/* siME */ = B(_nd/* LudoJS.$sa1 */(_1O/* LudoJS.Blue */, _FX/* LudoJS.lvl9 */, _Ih/* siMz */, _Ic/* siLN */, _Id/* siLO */, _Ie/* siLP */, _If/* siLQ */, _Ih/* siMz */, _/* EXTERNAL */)),
                            _Ij/* siML */ = B(_nC/* LudoJS.$wa15 */(_cw/* LudoJS.GameFinished */, _Id/* siLO */, _Ie/* siLP */, _If/* siLQ */, new T(function(){
                              return E(E(_Ii/* siME */).a);
                            }), _/* EXTERNAL */)),
                            _Ik/* siMR */ = E(E(_Ij/* siML */).b),
                            _Il/* siMX */ = B(_oX/* LudoJS.$wa16 */(_Ik/* siMR */.a, _Ik/* siMR */.b, _Ik/* siMR */.c, _Ik/* siMR */.d, _Ik/* siMR */.e, _Ik/* siMR */, _/* EXTERNAL */)),
                            _Im/* siNd */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_Il/* siMX */).b))));
                            return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                          }else{
                            return new F(function(){return _HW/* siKF */(_/* EXTERNAL */, _x1/* GHC.Types.True */, new T1(0,new T1(1,_HD/* siJJ */)), _Id/* siLO */, _Ie/* siLP */, _If/* siLQ */, _Ih/* siMz */);});
                          }
                        }else{
                          if(B(_mL/* GHC.List.$wlenAcc */(_Ig/* siLR */, 0))==3){
                            var _In/* siLX */ = B(_nd/* LudoJS.$sa1 */(_1O/* LudoJS.Blue */, _FX/* LudoJS.lvl9 */, _Ig/* siLR */, _Ic/* siLN */, _Id/* siLO */, _Ie/* siLP */, _If/* siLQ */, _Ig/* siLR */, _/* EXTERNAL */)),
                            _Io/* siM4 */ = B(_nC/* LudoJS.$wa15 */(_cw/* LudoJS.GameFinished */, _Id/* siLO */, _Ie/* siLP */, _If/* siLQ */, new T(function(){
                              return E(E(_In/* siLX */).a);
                            }), _/* EXTERNAL */)),
                            _Ip/* siMa */ = E(E(_Io/* siM4 */).b),
                            _Iq/* siMg */ = B(_oX/* LudoJS.$wa16 */(_Ip/* siMa */.a, _Ip/* siMa */.b, _Ip/* siMa */.c, _Ip/* siMa */.d, _Ip/* siMa */.e, _Ip/* siMa */, _/* EXTERNAL */)),
                            _Ir/* siMw */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_Iq/* siMg */).b))));
                            return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                          }else{
                            return new F(function(){return _HW/* siKF */(_/* EXTERNAL */, _x1/* GHC.Types.True */, new T1(0,new T1(1,_HD/* siJJ */)), _Id/* siLO */, _Ie/* siLP */, _If/* siLQ */, _Ig/* siLR */);});
                          }
                        }
                      },
                      _Is/* siNg */ = E(_HL/* siKb */);
                      if(!_Is/* siNg */._){
                        var _It/* siNi */ = B(_vo/* LudoJS.$wa7 */(_Is/* siNg */.a, 0, _HK/* siKa */, _/* EXTERNAL */)),
                        _Iu/* siNo */ = E(E(_It/* siNi */).b);
                        return new F(function(){return _Ib/* siLL */(_/* EXTERNAL */, _Iu/* siNo */.a, _Iu/* siNo */.b, _Iu/* siNo */.c, _Iu/* siNo */.d, _Iu/* siNo */.e);});
                      }else{
                        var _Iv/* siNy */ = B(_vo/* LudoJS.$wa7 */(_Is/* siNg */.a, E(_Is/* siNg */.b), _HK/* siKa */, _/* EXTERNAL */)),
                        _Iw/* siNE */ = E(E(_Iv/* siNy */).b);
                        return new F(function(){return _Ib/* siLL */(_/* EXTERNAL */, _Iw/* siNE */.a, _Iw/* siNE */.b, _Iw/* siNE */.c, _Iw/* siNE */.d, _Iw/* siNE */.e);});
                      }
                    }
                  }
                }
                break;
              default:
                var _Ix/* siNM */ = mMV/* EXTERNAL */(E(_Fp/* System.Random.theStdGen */), _FU/* LudoJS.lvl42 */),
                _Iy/* siNR */ = new T(function(){
                  switch(E(_Ix/* siNM */)){
                    case 1:
                      return 0;
                      break;
                    case 2:
                      return 1;
                      break;
                    case 3:
                      return 3;
                      break;
                    case 4:
                      return 2;
                      break;
                    default:
                      return E(_hk/* LudoJS.numToColor1 */);
                  }
                }),
                _Iz/* siNU */ = B(_oX/* LudoJS.$wa16 */(_mw/* LudoJS.play11 */, _Iy/* siNR */, _63/* LudoJS.play10 */, _Gg/* LudoJS.play5 */, _4/* GHC.Types.[] */, new T5(0,_mw/* LudoJS.play11 */,_Iy/* siNR */,_63/* LudoJS.play10 */,_Gg/* LudoJS.play5 */,_4/* GHC.Types.[] */), _/* EXTERNAL */)),
                _IA/* siOa */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_Iz/* siNU */).b))));
                return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
            }
          }else{
            return _eb/* GHC.Tuple.() */;
          }
        }else{
          return _eb/* GHC.Tuple.() */;
        }
      };
      switch(E(E(_Gq/* siED */).b)){
        case 0:
          return new F(function(){return _Gr/* siEU */(E(_1u/* LudoJS.$fFromAnyGameState14 */));});
          break;
        case 1:
          return new F(function(){return _Gr/* siEU */(E(_1t/* LudoJS.$fFromAnyGameState13 */));});
          break;
        case 2:
          return new F(function(){return _Gr/* siEU */(E(_1s/* LudoJS.$fFromAnyGameState12 */));});
          break;
        default:
          return new F(function(){return _Gr/* siEU */(E(_1r/* LudoJS.$fFromAnyGameState11 */));});
      }
    }else{
      return _eb/* GHC.Tuple.() */;
    }
  }
},
_IB/* play2 */ = function(_IC/* siOi */, _/* EXTERNAL */){
  var _ID/* siOk */ = E(_IC/* siOi */),
  _IE/* siOo */ = E(_ID/* siOk */.a);
  return new F(function(){return _Gi/* LudoJS.$wlvl */(_IE/* siOo */.a, _IE/* siOo */.b, _ID/* siOk */.b, _ID/* siOk */.c, _/* EXTERNAL */);});
},
_IF/* play4 */ = new T(function(){
  return B(_lD/* GHC.Base.map */(_nX/* LudoJS.$fToAnyOption_$ctoAny */, _4/* GHC.Types.[] */));
}),
_IG/* play1 */ = function(_IH/* siOr */, _II/* siOs */, _/* EXTERNAL */){
  var _IJ/* siOv */ = B(_lH/* LudoJS.$w$ctoAny */(new T5(0,_mw/* LudoJS.play11 */,_IH/* siOr */,_63/* LudoJS.play10 */,_Gg/* LudoJS.play5 */,_4/* GHC.Types.[] */))),
  _IK/* siOz */ = __app1/* EXTERNAL */(E(_Gh/* LudoJS.play_f1 */), _IJ/* siOv */),
  _IL/* siOF */ = __lst2arr/* EXTERNAL */(E(_IF/* LudoJS.play4 */)),
  _IM/* siOL */ = eval/* EXTERNAL */(E(_oU/* LudoJS.play3 */)),
  _IN/* siOV */ = __app3/* EXTERNAL */(E(_IM/* siOL */), _IJ/* siOv */, _IL/* siOF */,  -1),
  _IO/* siOY */ = B(A(_m4/* Haste.Events.Core.onEvent */,[_kB/* Haste.Events.Core.$fMonadEventIO */, _jR/* Haste.Events.Core.$fEventSourceElem1 */, _jQ/* Haste.Events.MouseEvents.$fEventMouseEvent */, _II/* siOs */, _lK/* Haste.Events.MouseEvents.Click */, _IB/* LudoJS.play2 */, _/* EXTERNAL */]));
  return _eb/* GHC.Tuple.() */;
},
_IP/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */(" found!"));
}),
_IQ/* withElem1 */ = function(_IR/* svSB */){
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("No element with ID ", new T(function(){
    return B(_q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(E(_IR/* svSB */)), _IP/* Haste.DOM.JSString.lvl */));
  }))));});
},
_IS/* main1 */ = function(_/* EXTERNAL */){
  var _IT/* stGn */ = function(_IU/* stFU */){
    var _IV/* stFV */ = new T(function(){
      var _IW/* stFY */ = B(_cU/* LudoJS.$wa1 */(E(_IU/* stFU */), _/* EXTERNAL */));
      return E(_IW/* stFY */);
    }),
    _IX/* stGm */ = function(_IY/* stG1 */){
      var _IZ/* stG2 */ = new T(function(){
        var _J0/* stG3 */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_IY/* stG1 */, _/* EXTERNAL */));
        return E(_J0/* stG3 */);
      });
      return function(_J1/* stG6 */, _/* EXTERNAL */){
        var _J2/* stGb */ = Number/* EXTERNAL */(E(_J1/* stG6 */)),
        _J3/* stGf */ = jsTrunc/* EXTERNAL */(_J2/* stGb */);
        return new F(function(){return _dj/* LudoJS.$wa6 */(_IV/* stFV */, _IZ/* stG2 */, _J3/* stGf */, _/* EXTERNAL */);});
      };
    };
    return E(_IX/* stGm */);
  },
  _J4/* stGr */ = __createJSFunc/* EXTERNAL */(4, E(_IT/* stGn */)),
  _J5/* stGz */ = __app2/* EXTERNAL */(E(_dz/* Main.f2 */), "numPiecesAt", _J4/* stGr */),
  _J6/* stGE */ = mMV/* EXTERNAL */(E(_Fp/* System.Random.theStdGen */), _hg/* Main.lvl4 */),
  _J7/* stGL */ = "canvas",
  _J8/* stGR */ = __app1/* EXTERNAL */(E(_dy/* Haste.DOM.JSString.elemById_f1 */), _J7/* stGL */),
  _J9/* stGX */ = __eq/* EXTERNAL */(_J8/* stGR */, E(_dF/* Haste.Prim.Any.jsNull */));
  if(!E(_J9/* stGX */)){
    var _Ja/* stH3 */ = __isUndef/* EXTERNAL */(_J8/* stGR */);
    if(!E(_Ja/* stH3 */)){
      return new F(function(){return _IG/* LudoJS.play1 */(new T(function(){
        switch(E(_J6/* stGE */)){
          case 1:
            return 0;
            break;
          case 2:
            return 1;
            break;
          case 3:
            return 3;
            break;
          case 4:
            return 2;
            break;
          default:
            return E(_hk/* LudoJS.numToColor1 */);
        }
      }), _J8/* stGR */, _/* EXTERNAL */);});
    }else{
      return new F(function(){return _IQ/* Haste.DOM.JSString.withElem1 */(_J7/* stGL */);});
    }
  }else{
    return new F(function(){return _IQ/* Haste.DOM.JSString.withElem1 */(_J7/* stGL */);});
  }
},
_Jb/* main */ = function(_/* EXTERNAL */){
  return new F(function(){return _IS/* Main.main1 */(_/* EXTERNAL */);});
};

var hasteMain = function() {B(A(_Jb, [0]));};onHasteStart(); hasteMain();