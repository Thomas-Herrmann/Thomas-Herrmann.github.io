"use strict";
var __haste_prog_id = '0d15ae1790d1e6c7b6231581a020852aa38fc0361c01c4bf24071349e5152a69';
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
_1A/* $fFromAnyGameState9 */ = function(_1B/* shQn */, _/* EXTERNAL */){
  return new T(function(){
    var _1C/* shQs */ = String/* EXTERNAL */(E(_1B/* shQn */)),
    _1D/* shQx */ = fromJSStr/* EXTERNAL */(_1C/* shQs */);
    if(!B(_1v/* GHC.Base.eqString */(_1D/* shQx */, _1u/* LudoJS.$fFromAnyGameState14 */))){
      if(!B(_1v/* GHC.Base.eqString */(_1D/* shQx */, _1t/* LudoJS.$fFromAnyGameState13 */))){
        if(!B(_1v/* GHC.Base.eqString */(_1D/* shQx */, _1s/* LudoJS.$fFromAnyGameState12 */))){
          if(!B(_1v/* GHC.Base.eqString */(_1D/* shQx */, _1r/* LudoJS.$fFromAnyGameState11 */))){
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
_1G/* $fFromAnyGameState4 */ = function(_1H/* siaR */, _/* EXTERNAL */){
  var _1I/* siaT */ = E(_1H/* siaR */);
  if(!_1I/* siaT */._){
    return _4/* GHC.Types.[] */;
  }else{
    var _1J/* siaW */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_1I/* siaT */.a, _/* EXTERNAL */)),
    _1K/* siaZ */ = B(_1G/* LudoJS.$fFromAnyGameState4 */(_1I/* siaT */.b, _/* EXTERNAL */));
    return new T2(1,_1J/* siaW */,_1K/* siaZ */);
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
_3j/* $sinsert_$sgo10 */ = function(_3k/* shJG */, _3l/* shJH */, _3m/* shJI */){
  var _3n/* shJJ */ = E(_3k/* shJG */),
  _3o/* shJK */ = E(_3m/* shJI */);
  if(!_3o/* shJK */._){
    var _3p/* shJL */ = _3o/* shJK */.a,
    _3q/* shJM */ = _3o/* shJK */.b,
    _3r/* shJN */ = _3o/* shJK */.c,
    _3s/* shJO */ = _3o/* shJK */.d,
    _3t/* shJP */ = _3o/* shJK */.e;
    switch(E(_3n/* shJJ */)){
      case 0:
        switch(E(_3q/* shJM */)){
          case 0:
            return new T5(0,_3p/* shJL */,E(_1O/* LudoJS.Blue */),_3l/* shJH */,E(_3s/* shJO */),E(_3t/* shJP */));
          case 1:
            return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1P/* LudoJS.Green */, _3r/* shJN */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1O/* LudoJS.Blue */, _3l/* shJH */, _3s/* shJO */)), _3t/* shJP */);});
            break;
          case 2:
            return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1Q/* LudoJS.Red */, _3r/* shJN */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1O/* LudoJS.Blue */, _3l/* shJH */, _3s/* shJO */)), _3t/* shJP */);});
            break;
          default:
            return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1S/* LudoJS.Yellow */, _3r/* shJN */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1O/* LudoJS.Blue */, _3l/* shJH */, _3s/* shJO */)), _3t/* shJP */);});
        }
        break;
      case 1:
        switch(E(_3q/* shJM */)){
          case 0:
            return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1O/* LudoJS.Blue */, _3r/* shJN */, _3s/* shJO */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1P/* LudoJS.Green */, _3l/* shJH */, _3t/* shJP */)));});
            break;
          case 1:
            return new T5(0,_3p/* shJL */,E(_1P/* LudoJS.Green */),_3l/* shJH */,E(_3s/* shJO */),E(_3t/* shJP */));
          case 2:
            return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1Q/* LudoJS.Red */, _3r/* shJN */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1P/* LudoJS.Green */, _3l/* shJH */, _3s/* shJO */)), _3t/* shJP */);});
            break;
          default:
            return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1S/* LudoJS.Yellow */, _3r/* shJN */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1P/* LudoJS.Green */, _3l/* shJH */, _3s/* shJO */)), _3t/* shJP */);});
        }
        break;
      case 2:
        switch(E(_3q/* shJM */)){
          case 0:
            return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1O/* LudoJS.Blue */, _3r/* shJN */, _3s/* shJO */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1Q/* LudoJS.Red */, _3l/* shJH */, _3t/* shJP */)));});
            break;
          case 1:
            return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1P/* LudoJS.Green */, _3r/* shJN */, _3s/* shJO */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1Q/* LudoJS.Red */, _3l/* shJH */, _3t/* shJP */)));});
            break;
          case 2:
            return new T5(0,_3p/* shJL */,E(_1Q/* LudoJS.Red */),_3l/* shJH */,E(_3s/* shJO */),E(_3t/* shJP */));
          default:
            return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1S/* LudoJS.Yellow */, _3r/* shJN */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1Q/* LudoJS.Red */, _3l/* shJH */, _3s/* shJO */)), _3t/* shJP */);});
        }
        break;
      default:
        switch(E(_3q/* shJM */)){
          case 0:
            return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1O/* LudoJS.Blue */, _3r/* shJN */, _3s/* shJO */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1S/* LudoJS.Yellow */, _3l/* shJH */, _3t/* shJP */)));});
            break;
          case 1:
            return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1P/* LudoJS.Green */, _3r/* shJN */, _3s/* shJO */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1S/* LudoJS.Yellow */, _3l/* shJH */, _3t/* shJP */)));});
            break;
          case 2:
            return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1Q/* LudoJS.Red */, _3r/* shJN */, _3s/* shJO */, B(_3j/* LudoJS.$sinsert_$sgo10 */(_1S/* LudoJS.Yellow */, _3l/* shJH */, _3t/* shJP */)));});
            break;
          default:
            return new T5(0,_3p/* shJL */,E(_1S/* LudoJS.Yellow */),_3l/* shJH */,E(_3s/* shJO */),E(_3t/* shJP */));
        }
    }
  }else{
    return new T5(0,1,E(_3n/* shJJ */),_3l/* shJH */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
  }
},
_3u/* poly_go10 */ = function(_3v/* shKj */, _3w/* shKk */){
  while(1){
    var _3x/* shKl */ = E(_3w/* shKk */);
    if(!_3x/* shKl */._){
      return E(_3v/* shKj */);
    }else{
      var _3y/* shKo */ = E(_3x/* shKl */.a),
      _3z/*  shKj */ = B(_3j/* LudoJS.$sinsert_$sgo10 */(_3y/* shKo */.a, _3y/* shKo */.b, _3v/* shKj */));
      _3v/* shKj */ = _3z/*  shKj */;
      _3w/* shKk */ = _3x/* shKl */.b;
      continue;
    }
  }
},
_3A/* $sfromList_$spoly_go10 */ = function(_3B/* shKe */, _3C/* shKf */, _3D/* shKg */, _3E/* shKh */){
  return new F(function(){return _3u/* LudoJS.poly_go10 */(B(_3j/* LudoJS.$sinsert_$sgo10 */(_3C/* shKf */, _3D/* shKg */, _3B/* shKe */)), _3E/* shKh */);});
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
_4L/* $s$wpoly_create */ = function(_4M/* shHH */, _4N/* shHI */, _4O/* shHJ */, _4P/* shHK */){
  var _4Q/* shHL */ = E(_4M/* shHH */);
  if(_4Q/* shHL */==1){
    var _4R/* shIJ */ = E(_4P/* shHK */);
    if(!_4R/* shIJ */._){
      return new T3(0,new T(function(){
        return new T5(0,1,E(_4N/* shHI */),_4O/* shHJ */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
      }),_4/* GHC.Types.[] */,_4/* GHC.Types.[] */);
    }else{
      var _4S/* shIP */ = E(_4R/* shIJ */.a).a;
      switch(E(_4N/* shHI */)){
        case 0:
          switch(E(_4S/* shIP */)){
            case 0:
              return new T3(0,new T5(0,1,E(_1O/* LudoJS.Blue */),_4O/* shHJ */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_4R/* shIJ */);
            case 1:
              return new T3(0,new T5(0,1,E(_1O/* LudoJS.Blue */),_4O/* shHJ */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4R/* shIJ */,_4/* GHC.Types.[] */);
            case 2:
              return new T3(0,new T5(0,1,E(_1O/* LudoJS.Blue */),_4O/* shHJ */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4R/* shIJ */,_4/* GHC.Types.[] */);
            default:
              return new T3(0,new T5(0,1,E(_1O/* LudoJS.Blue */),_4O/* shHJ */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4R/* shIJ */,_4/* GHC.Types.[] */);
          }
          break;
        case 1:
          switch(E(_4S/* shIP */)){
            case 2:
              return new T3(0,new T5(0,1,E(_1P/* LudoJS.Green */),_4O/* shHJ */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4R/* shIJ */,_4/* GHC.Types.[] */);
            case 3:
              return new T3(0,new T5(0,1,E(_1P/* LudoJS.Green */),_4O/* shHJ */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4R/* shIJ */,_4/* GHC.Types.[] */);
            default:
              return new T3(0,new T5(0,1,E(_1P/* LudoJS.Green */),_4O/* shHJ */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_4R/* shIJ */);
          }
          break;
        case 2:
          return (E(_4S/* shIP */)==3) ? new T3(0,new T5(0,1,E(_1Q/* LudoJS.Red */),_4O/* shHJ */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4R/* shIJ */,_4/* GHC.Types.[] */) : new T3(0,new T5(0,1,E(_1Q/* LudoJS.Red */),_4O/* shHJ */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_4R/* shIJ */);
        default:
          var _4T/* shJ4 */ = E(_4S/* shIP */);
          return new T3(0,new T5(0,1,E(_1S/* LudoJS.Yellow */),_4O/* shHJ */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_4R/* shIJ */);
      }
    }
  }else{
    var _4U/* shHN */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shHL */>>1, _4N/* shHI */, _4O/* shHJ */, _4P/* shHK */)),
    _4V/* shHO */ = _4U/* shHN */.a,
    _4W/* shHQ */ = _4U/* shHN */.c,
    _4X/* shHR */ = E(_4U/* shHN */.b);
    if(!_4X/* shHR */._){
      return new T3(0,_4V/* shHO */,_4/* GHC.Types.[] */,_4W/* shHQ */);
    }else{
      var _4Y/* shHU */ = E(_4X/* shHR */.a),
      _4Z/* shHV */ = _4Y/* shHU */.a,
      _50/* shHW */ = _4Y/* shHU */.b,
      _51/* shHX */ = E(_4X/* shHR */.b);
      if(!_51/* shHX */._){
        return new T3(0,new T(function(){
          return B(_3I/* Data.Map.Base.insertMax */(_4Z/* shHV */, _50/* shHW */, _4V/* shHO */));
        }),_4/* GHC.Types.[] */,_4W/* shHQ */);
      }else{
        var _52/* shI0 */ = _51/* shHX */.b,
        _53/* shI1 */ = E(_51/* shHX */.a),
        _54/* shI2 */ = _53/* shI1 */.a,
        _55/* shI3 */ = _53/* shI1 */.b;
        switch(E(_4Z/* shHV */)){
          case 0:
            switch(E(_54/* shI2 */)){
              case 0:
                return new T3(0,_4V/* shHO */,_4/* GHC.Types.[] */,_4X/* shHR */);
              case 1:
                var _56/* shI7 */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shHL */>>1, _1P/* LudoJS.Green */, _55/* shI3 */, _52/* shI0 */));
                return new T3(0,new T(function(){
                  return B(_4u/* Data.Map.Base.link */(_1O/* LudoJS.Blue */, _50/* shHW */, _4V/* shHO */, _56/* shI7 */.a));
                }),_56/* shI7 */.b,_56/* shI7 */.c);
              case 2:
                var _57/* shId */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shHL */>>1, _1Q/* LudoJS.Red */, _55/* shI3 */, _52/* shI0 */));
                return new T3(0,new T(function(){
                  return B(_4u/* Data.Map.Base.link */(_1O/* LudoJS.Blue */, _50/* shHW */, _4V/* shHO */, _57/* shId */.a));
                }),_57/* shId */.b,_57/* shId */.c);
              default:
                var _58/* shIj */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shHL */>>1, _1S/* LudoJS.Yellow */, _55/* shI3 */, _52/* shI0 */));
                return new T3(0,new T(function(){
                  return B(_4u/* Data.Map.Base.link */(_1O/* LudoJS.Blue */, _50/* shHW */, _4V/* shHO */, _58/* shIj */.a));
                }),_58/* shIj */.b,_58/* shIj */.c);
            }
            break;
          case 1:
            switch(E(_54/* shI2 */)){
              case 2:
                var _59/* shIq */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shHL */>>1, _1Q/* LudoJS.Red */, _55/* shI3 */, _52/* shI0 */));
                return new T3(0,new T(function(){
                  return B(_4u/* Data.Map.Base.link */(_1P/* LudoJS.Green */, _50/* shHW */, _4V/* shHO */, _59/* shIq */.a));
                }),_59/* shIq */.b,_59/* shIq */.c);
              case 3:
                var _5a/* shIw */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shHL */>>1, _1S/* LudoJS.Yellow */, _55/* shI3 */, _52/* shI0 */));
                return new T3(0,new T(function(){
                  return B(_4u/* Data.Map.Base.link */(_1P/* LudoJS.Green */, _50/* shHW */, _4V/* shHO */, _5a/* shIw */.a));
                }),_5a/* shIw */.b,_5a/* shIw */.c);
              default:
                return new T3(0,_4V/* shHO */,_4/* GHC.Types.[] */,_4X/* shHR */);
            }
            break;
          case 2:
            if(E(_54/* shI2 */)==3){
              var _5b/* shID */ = B(_4L/* LudoJS.$s$wpoly_create */(_4Q/* shHL */>>1, _1S/* LudoJS.Yellow */, _55/* shI3 */, _52/* shI0 */));
              return new T3(0,new T(function(){
                return B(_4u/* Data.Map.Base.link */(_1Q/* LudoJS.Red */, _50/* shHW */, _4V/* shHO */, _5b/* shID */.a));
              }),_5b/* shID */.b,_5b/* shID */.c);
            }else{
              return new T3(0,_4V/* shHO */,_4/* GHC.Types.[] */,_4X/* shHR */);
            }
            break;
          default:
            var _5c/* shII */ = E(_54/* shI2 */);
            return new T3(0,_4V/* shHO */,_4/* GHC.Types.[] */,_4X/* shHR */);
        }
      }
    }
  }
},
_5d/* $spoly_go10 */ = function(_5e/* shK7 */, _5f/* shK8 */, _5g/* shK9 */){
  var _5h/* shKa */ = E(_5f/* shK8 */);
  return new F(function(){return _3u/* LudoJS.poly_go10 */(B(_3j/* LudoJS.$sinsert_$sgo10 */(_5h/* shKa */.a, _5h/* shKa */.b, _5e/* shK7 */)), _5g/* shK9 */);});
},
_5i/* $wpoly_go10 */ = function(_5j/* shLa */, _5k/* shLb */, _5l/* shLc */){
  var _5m/* shLd */ = E(_5l/* shLc */);
  if(!_5m/* shLd */._){
    return E(_5k/* shLb */);
  }else{
    var _5n/* shLg */ = E(_5m/* shLd */.a),
    _5o/* shLh */ = _5n/* shLg */.a,
    _5p/* shLi */ = _5n/* shLg */.b,
    _5q/* shLj */ = E(_5m/* shLd */.b);
    if(!_5q/* shLj */._){
      return new F(function(){return _3I/* Data.Map.Base.insertMax */(_5o/* shLh */, _5p/* shLi */, _5k/* shLb */);});
    }else{
      var _5r/* shLm */ = E(_5q/* shLj */.a),
      _5s/* shLn */ = _5r/* shLm */.a,
      _5t/* shLp */ = function(_5u/* shLq */){
        var _5v/* shLr */ = B(_4L/* LudoJS.$s$wpoly_create */(_5j/* shLa */, _5s/* shLn */, _5r/* shLm */.b, _5q/* shLj */.b)),
        _5w/* shLs */ = _5v/* shLr */.a,
        _5x/* shLv */ = E(_5v/* shLr */.c);
        if(!_5x/* shLv */._){
          return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(_5j/* shLa */<<1, B(_4u/* Data.Map.Base.link */(_5o/* shLh */, _5p/* shLi */, _5k/* shLb */, _5w/* shLs */)), _5v/* shLr */.b);});
        }else{
          return new F(function(){return _5d/* LudoJS.$spoly_go10 */(B(_4u/* Data.Map.Base.link */(_5o/* shLh */, _5p/* shLi */, _5k/* shLb */, _5w/* shLs */)), _5x/* shLv */.a, _5x/* shLv */.b);});
        }
      };
      switch(E(_5o/* shLh */)){
        case 0:
          switch(E(_5s/* shLn */)){
            case 0:
              return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(_5k/* shLb */, _1O/* LudoJS.Blue */, _5p/* shLi */, _5q/* shLj */);});
              break;
            case 1:
              return new F(function(){return _5t/* shLp */(_/* EXTERNAL */);});
              break;
            case 2:
              return new F(function(){return _5t/* shLp */(_/* EXTERNAL */);});
              break;
            default:
              return new F(function(){return _5t/* shLp */(_/* EXTERNAL */);});
          }
          break;
        case 1:
          switch(E(_5s/* shLn */)){
            case 2:
              return new F(function(){return _5t/* shLp */(_/* EXTERNAL */);});
              break;
            case 3:
              return new F(function(){return _5t/* shLp */(_/* EXTERNAL */);});
              break;
            default:
              return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(_5k/* shLb */, _1P/* LudoJS.Green */, _5p/* shLi */, _5q/* shLj */);});
          }
          break;
        case 2:
          if(E(_5s/* shLn */)==3){
            return new F(function(){return _5t/* shLp */(_/* EXTERNAL */);});
          }else{
            return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(_5k/* shLb */, _1Q/* LudoJS.Red */, _5p/* shLi */, _5q/* shLj */);});
          }
          break;
        default:
          var _5y/* shLF */ = E(_5s/* shLn */);
          return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(_5k/* shLb */, _1S/* LudoJS.Yellow */, _5p/* shLi */, _5q/* shLj */);});
      }
    }
  }
},
_5z/* $sfromList */ = function(_5A/* shNv */){
  var _5B/* shNw */ = E(_5A/* shNv */);
  if(!_5B/* shNw */._){
    return new T0(1);
  }else{
    var _5C/* shNz */ = E(_5B/* shNw */.a),
    _5D/* shNA */ = _5C/* shNz */.a,
    _5E/* shNB */ = _5C/* shNz */.b,
    _5F/* shNC */ = E(_5B/* shNw */.b);
    if(!_5F/* shNC */._){
      return new T5(0,1,E(_5D/* shNA */),_5E/* shNB */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
    }else{
      var _5G/* shNF */ = _5F/* shNC */.b,
      _5H/* shNG */ = E(_5F/* shNC */.a),
      _5I/* shNH */ = _5H/* shNG */.a,
      _5J/* shNI */ = _5H/* shNG */.b;
      switch(E(_5D/* shNA */)){
        case 0:
          switch(E(_5I/* shNH */)){
            case 0:
              return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_1O/* LudoJS.Blue */),_5E/* shNB */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _1O/* LudoJS.Blue */, _5J/* shNI */, _5G/* shNF */);});
              break;
            case 1:
              return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_1O/* LudoJS.Blue */),_5E/* shNB */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5F/* shNC */);});
              break;
            case 2:
              return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_1O/* LudoJS.Blue */),_5E/* shNB */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5F/* shNC */);});
              break;
            default:
              return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_1O/* LudoJS.Blue */),_5E/* shNB */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5F/* shNC */);});
          }
          break;
        case 1:
          var _5K/* shNP */ = E(_5I/* shNH */);
          switch(_5K/* shNP */){
            case 2:
              return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_1P/* LudoJS.Green */),_5E/* shNB */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5F/* shNC */);});
              break;
            case 3:
              return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_1P/* LudoJS.Green */),_5E/* shNB */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5F/* shNC */);});
              break;
            default:
              return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_1P/* LudoJS.Green */),_5E/* shNB */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5K/* shNP */, _5J/* shNI */, _5G/* shNF */);});
          }
          break;
        case 2:
          var _5L/* shNT */ = E(_5I/* shNH */);
          if(_5L/* shNT */==3){
            return new F(function(){return _5i/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_1Q/* LudoJS.Red */),_5E/* shNB */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5F/* shNC */);});
          }else{
            return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_1Q/* LudoJS.Red */),_5E/* shNB */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), _5L/* shNT */, _5J/* shNI */, _5G/* shNF */);});
          }
          break;
        default:
          return new F(function(){return _3A/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_1S/* LudoJS.Yellow */),_5E/* shNB */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */)), E(_5I/* shNH */), _5J/* shNI */, _5G/* shNF */);});
      }
    }
  }
},
_5M/* $w$cshowsPrec */ = function(_5N/* sidD */, _5O/* sidE */){
  switch(E(_5N/* sidD */)){
    case 0:
      return new F(function(){return _q/* GHC.Base.++ */(_1u/* LudoJS.$fFromAnyGameState14 */, _5O/* sidE */);});
      break;
    case 1:
      return new F(function(){return _q/* GHC.Base.++ */(_1t/* LudoJS.$fFromAnyGameState13 */, _5O/* sidE */);});
      break;
    case 2:
      return new F(function(){return _q/* GHC.Base.++ */(_1s/* LudoJS.$fFromAnyGameState12 */, _5O/* sidE */);});
      break;
    default:
      return new F(function(){return _q/* GHC.Base.++ */(_1r/* LudoJS.$fFromAnyGameState11 */, _5O/* sidE */);});
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
_6c/* $fFromAnyGameState6 */ = function(_6d/* sidK */, _/* EXTERNAL */){
  var _6e/* sidM */ = E(_6d/* sidK */),
  _6f/* sidU */ = __get/* EXTERNAL */(_6e/* sidM */, toJSStr/* EXTERNAL */(B(_q/* GHC.Base.++ */(_1u/* LudoJS.$fFromAnyGameState14 */, _4/* GHC.Types.[] */)))),
  _6g/* sidW */ = _6f/* sidU */,
  _6h/* sidY */ = function(_6i/*  sih8 */, _6j/*  sih9 */, _/* EXTERNAL */){
    while(1){
      var _6k/*  sidY */ = B((function(_6l/* sih8 */, _6m/* sih9 */, _/* EXTERNAL */){
        var _6n/* sihb */ = E(_6l/* sih8 */);
        if(!_6n/* sihb */._){
          return _6m/* sih9 */;
        }else{
          var _6o/* sihd */ = _6n/* sihb */.b,
          _6p/* sihe */ = E(_6n/* sihb */.a),
          _6q/* sihi */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _6p/* sihe */, _4/* GHC.Types.[] */))),
          _6r/* sihm */ = __has/* EXTERNAL */(_6g/* sidW */, _6q/* sihi */);
          if(!E(_6r/* sihm */)){
            var _6s/*   sih9 */ = _6m/* sih9 */;
            _6i/*  sih8 */ = _6o/* sihd */;
            _6j/*  sih9 */ = _6s/*   sih9 */;
            return __continue/* EXTERNAL */;
          }else{
            var _6t/* sihr */ = __get/* EXTERNAL */(_6g/* sidW */, _6q/* sihi */),
            _6u/* sihu */ = E(_1N/* LudoJS.$fToAnyOption5 */),
            _6v/* sihx */ = __get/* EXTERNAL */(_6t/* sihr */, _6u/* sihu */),
            _6w/* sihB */ = String/* EXTERNAL */(_6v/* sihx */),
            _6x/* sihE */ = E(_69/* LudoJS.lvl34 */),
            _6y/* sihH */ = strEq/* EXTERNAL */(_6w/* sihB */, _6x/* sihE */);
            if(!E(_6y/* sihH */)){
              var _6z/* siiS */ = E(_68/* LudoJS.lvl33 */),
              _6A/* siiV */ = strEq/* EXTERNAL */(_6w/* sihB */, _6z/* siiS */);
              if(!E(_6A/* siiV */)){
                return E(_67/* LudoJS.lvl32 */);
              }else{
                var _6B/* siiZ */ = E(_1M/* LudoJS.$fToAnyOption1 */),
                _6C/* sij2 */ = __get/* EXTERNAL */(_6t/* sihr */, _6B/* siiZ */),
                _6D/* sij5 */ = function(_6E/*  sij6 */, _6F/*  sij7 */, _/* EXTERNAL */){
                  while(1){
                    var _6G/*  sij5 */ = B((function(_6H/* sij6 */, _6I/* sij7 */, _/* EXTERNAL */){
                      var _6J/* sij9 */ = E(_6H/* sij6 */);
                      if(!_6J/* sij9 */._){
                        return _6I/* sij7 */;
                      }else{
                        var _6K/* sijb */ = _6J/* sij9 */.b,
                        _6L/* sijc */ = E(_6J/* sij9 */.a),
                        _6M/* sijg */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _6L/* sijc */, _4/* GHC.Types.[] */))),
                        _6N/* sijk */ = __has/* EXTERNAL */(_6g/* sidW */, _6M/* sijg */);
                        if(!E(_6N/* sijk */)){
                          var _6O/*   sij7 */ = _6I/* sij7 */;
                          _6E/*  sij6 */ = _6K/* sijb */;
                          _6F/*  sij7 */ = _6O/*   sij7 */;
                          return __continue/* EXTERNAL */;
                        }else{
                          var _6P/* sijp */ = __get/* EXTERNAL */(_6g/* sidW */, _6M/* sijg */),
                          _6Q/* sijt */ = __get/* EXTERNAL */(_6P/* sijp */, _6u/* sihu */),
                          _6R/* sijx */ = String/* EXTERNAL */(_6Q/* sijt */),
                          _6S/* sijB */ = strEq/* EXTERNAL */(_6R/* sijx */, _6x/* sihE */);
                          if(!E(_6S/* sijB */)){
                            var _6T/* sijJ */ = strEq/* EXTERNAL */(_6R/* sijx */, _6z/* siiS */);
                            if(!E(_6T/* sijJ */)){
                              return E(_67/* LudoJS.lvl32 */);
                            }else{
                              var _6U/* sijO */ = __get/* EXTERNAL */(_6P/* sijp */, _6B/* siiZ */),
                              _6V/* sik3 */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_6I/* sij7 */, new T2(1,new T2(0,_6L/* sijc */,new T1(1,new T(function(){
                                  var _6W/* sijS */ = Number/* EXTERNAL */(_6U/* sijO */);
                                  return jsTrunc/* EXTERNAL */(_6W/* sijS */);
                                }))),_4/* GHC.Types.[] */)));
                              });
                              _6E/*  sij6 */ = _6K/* sijb */;
                              _6F/*  sij7 */ = _6V/* sik3 */;
                              return __continue/* EXTERNAL */;
                            }
                          }else{
                            _6E/*  sij6 */ = _6K/* sijb */;
                            _6F/*  sij7 */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_6I/* sij7 */, new T2(1,new T2(0,_6L/* sijc */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                            });
                            return __continue/* EXTERNAL */;
                          }
                        }
                      }
                    })(_6E/*  sij6 */, _6F/*  sij7 */, _/* EXTERNAL */));
                    if(_6G/*  sij5 */!=__continue/* EXTERNAL */){
                      return _6G/*  sij5 */;
                    }
                  }
                },
                _6X/* sikh */ = new T(function(){
                  return B(_q/* GHC.Base.++ */(_6m/* sih9 */, new T2(1,new T2(0,_6p/* sihe */,new T1(1,new T(function(){
                    var _6Y/* sik6 */ = Number/* EXTERNAL */(_6C/* sij2 */);
                    return jsTrunc/* EXTERNAL */(_6Y/* sik6 */);
                  }))),_4/* GHC.Types.[] */)));
                });
                return new F(function(){return _6D/* sij5 */(_6o/* sihd */, _6X/* sikh */, _/* EXTERNAL */);});
              }
            }else{
              var _6Z/* sihL */ = function(_70/*  sihM */, _71/*  sihN */, _/* EXTERNAL */){
                while(1){
                  var _72/*  sihL */ = B((function(_73/* sihM */, _74/* sihN */, _/* EXTERNAL */){
                    var _75/* sihP */ = E(_73/* sihM */);
                    if(!_75/* sihP */._){
                      return _74/* sihN */;
                    }else{
                      var _76/* sihR */ = _75/* sihP */.b,
                      _77/* sihS */ = E(_75/* sihP */.a),
                      _78/* sihW */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _77/* sihS */, _4/* GHC.Types.[] */))),
                      _79/* sii0 */ = __has/* EXTERNAL */(_6g/* sidW */, _78/* sihW */);
                      if(!E(_79/* sii0 */)){
                        var _7a/*   sihN */ = _74/* sihN */;
                        _70/*  sihM */ = _76/* sihR */;
                        _71/*  sihN */ = _7a/*   sihN */;
                        return __continue/* EXTERNAL */;
                      }else{
                        var _7b/* sii5 */ = __get/* EXTERNAL */(_6g/* sidW */, _78/* sihW */),
                        _7c/* sii9 */ = __get/* EXTERNAL */(_7b/* sii5 */, _6u/* sihu */),
                        _7d/* siid */ = String/* EXTERNAL */(_7c/* sii9 */),
                        _7e/* siih */ = strEq/* EXTERNAL */(_7d/* siid */, _6x/* sihE */);
                        if(!E(_7e/* siih */)){
                          var _7f/* siir */ = strEq/* EXTERNAL */(_7d/* siid */, E(_68/* LudoJS.lvl33 */));
                          if(!E(_7f/* siir */)){
                            return E(_67/* LudoJS.lvl32 */);
                          }else{
                            var _7g/* siiy */ = __get/* EXTERNAL */(_7b/* sii5 */, E(_1M/* LudoJS.$fToAnyOption1 */)),
                            _7h/* siiN */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_74/* sihN */, new T2(1,new T2(0,_77/* sihS */,new T1(1,new T(function(){
                                var _7i/* siiC */ = Number/* EXTERNAL */(_7g/* siiy */);
                                return jsTrunc/* EXTERNAL */(_7i/* siiC */);
                              }))),_4/* GHC.Types.[] */)));
                            });
                            _70/*  sihM */ = _76/* sihR */;
                            _71/*  sihN */ = _7h/* siiN */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          _70/*  sihM */ = _76/* sihR */;
                          _71/*  sihN */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_74/* sihN */, new T2(1,new T2(0,_77/* sihS */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                          });
                          return __continue/* EXTERNAL */;
                        }
                      }
                    }
                  })(_70/*  sihM */, _71/*  sihN */, _/* EXTERNAL */));
                  if(_72/*  sihL */!=__continue/* EXTERNAL */){
                    return _72/*  sihL */;
                  }
                }
              };
              return new F(function(){return _6Z/* sihL */(_6o/* sihd */, new T(function(){
                return B(_q/* GHC.Base.++ */(_6m/* sih9 */, new T2(1,new T2(0,_6p/* sihe */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
              }), _/* EXTERNAL */);});
            }
          }
        }
      })(_6i/*  sih8 */, _6j/*  sih9 */, _/* EXTERNAL */));
      if(_6k/*  sidY */!=__continue/* EXTERNAL */){
        return _6k/*  sidY */;
      }
    }
  },
  _7j/* sidX */ = function(_7k/* sidZ */, _7l/* sie0 */, _7m/* sie1 */, _/* EXTERNAL */){
    var _7n/* sie5 */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _7k/* sidZ */, _4/* GHC.Types.[] */))),
    _7o/* sie9 */ = __has/* EXTERNAL */(_6g/* sidW */, _7n/* sie5 */);
    if(!E(_7o/* sie9 */)){
      return new F(function(){return _6h/* sidY */(_7l/* sie0 */, _7m/* sie1 */, _/* EXTERNAL */);});
    }else{
      var _7p/* siee */ = __get/* EXTERNAL */(_6g/* sidW */, _7n/* sie5 */),
      _7q/* sieh */ = E(_1N/* LudoJS.$fToAnyOption5 */),
      _7r/* siek */ = __get/* EXTERNAL */(_7p/* siee */, _7q/* sieh */),
      _7s/* sieo */ = String/* EXTERNAL */(_7r/* siek */),
      _7t/* sier */ = E(_69/* LudoJS.lvl34 */),
      _7u/* sieu */ = strEq/* EXTERNAL */(_7s/* sieo */, _7t/* sier */);
      if(!E(_7u/* sieu */)){
        var _7v/* sifG */ = E(_68/* LudoJS.lvl33 */),
        _7w/* sifJ */ = strEq/* EXTERNAL */(_7s/* sieo */, _7v/* sifG */);
        if(!E(_7w/* sifJ */)){
          return E(_67/* LudoJS.lvl32 */);
        }else{
          var _7x/* sifN */ = E(_1M/* LudoJS.$fToAnyOption1 */),
          _7y/* sifQ */ = __get/* EXTERNAL */(_7p/* siee */, _7x/* sifN */),
          _7z/* sifT */ = function(_7A/*  sifU */, _7B/*  sifV */, _/* EXTERNAL */){
            while(1){
              var _7C/*  sifT */ = B((function(_7D/* sifU */, _7E/* sifV */, _/* EXTERNAL */){
                var _7F/* sifX */ = E(_7D/* sifU */);
                if(!_7F/* sifX */._){
                  return _7E/* sifV */;
                }else{
                  var _7G/* sifZ */ = _7F/* sifX */.b,
                  _7H/* sig0 */ = E(_7F/* sifX */.a),
                  _7I/* sig4 */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _7H/* sig0 */, _4/* GHC.Types.[] */))),
                  _7J/* sig8 */ = __has/* EXTERNAL */(_6g/* sidW */, _7I/* sig4 */);
                  if(!E(_7J/* sig8 */)){
                    var _7K/*   sifV */ = _7E/* sifV */;
                    _7A/*  sifU */ = _7G/* sifZ */;
                    _7B/*  sifV */ = _7K/*   sifV */;
                    return __continue/* EXTERNAL */;
                  }else{
                    var _7L/* sigd */ = __get/* EXTERNAL */(_6g/* sidW */, _7I/* sig4 */),
                    _7M/* sigh */ = __get/* EXTERNAL */(_7L/* sigd */, _7q/* sieh */),
                    _7N/* sigl */ = String/* EXTERNAL */(_7M/* sigh */),
                    _7O/* sigp */ = strEq/* EXTERNAL */(_7N/* sigl */, _7t/* sier */);
                    if(!E(_7O/* sigp */)){
                      var _7P/* sigx */ = strEq/* EXTERNAL */(_7N/* sigl */, _7v/* sifG */);
                      if(!E(_7P/* sigx */)){
                        return E(_67/* LudoJS.lvl32 */);
                      }else{
                        var _7Q/* sigC */ = __get/* EXTERNAL */(_7L/* sigd */, _7x/* sifN */),
                        _7R/* sigR */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_7E/* sifV */, new T2(1,new T2(0,_7H/* sig0 */,new T1(1,new T(function(){
                            var _7S/* sigG */ = Number/* EXTERNAL */(_7Q/* sigC */);
                            return jsTrunc/* EXTERNAL */(_7S/* sigG */);
                          }))),_4/* GHC.Types.[] */)));
                        });
                        _7A/*  sifU */ = _7G/* sifZ */;
                        _7B/*  sifV */ = _7R/* sigR */;
                        return __continue/* EXTERNAL */;
                      }
                    }else{
                      _7A/*  sifU */ = _7G/* sifZ */;
                      _7B/*  sifV */ = new T(function(){
                        return B(_q/* GHC.Base.++ */(_7E/* sifV */, new T2(1,new T2(0,_7H/* sig0 */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                      });
                      return __continue/* EXTERNAL */;
                    }
                  }
                }
              })(_7A/*  sifU */, _7B/*  sifV */, _/* EXTERNAL */));
              if(_7C/*  sifT */!=__continue/* EXTERNAL */){
                return _7C/*  sifT */;
              }
            }
          },
          _7T/* sih6 */ = new T(function(){
            return B(_q/* GHC.Base.++ */(_7m/* sie1 */, new T2(1,new T2(0,_7k/* sidZ */,new T1(1,new T(function(){
              var _7U/* sigV */ = Number/* EXTERNAL */(_7y/* sifQ */);
              return jsTrunc/* EXTERNAL */(_7U/* sigV */);
            }))),_4/* GHC.Types.[] */)));
          });
          return new F(function(){return _7z/* sifT */(_7l/* sie0 */, _7T/* sih6 */, _/* EXTERNAL */);});
        }
      }else{
        var _7V/* siey */ = function(_7W/*  siez */, _7X/*  sieA */, _/* EXTERNAL */){
          while(1){
            var _7Y/*  siey */ = B((function(_7Z/* siez */, _80/* sieA */, _/* EXTERNAL */){
              var _81/* sieC */ = E(_7Z/* siez */);
              if(!_81/* sieC */._){
                return _80/* sieA */;
              }else{
                var _82/* sieE */ = _81/* sieC */.b,
                _83/* sieF */ = E(_81/* sieC */.a),
                _84/* sieJ */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _83/* sieF */, _4/* GHC.Types.[] */))),
                _85/* sieN */ = __has/* EXTERNAL */(_6g/* sidW */, _84/* sieJ */);
                if(!E(_85/* sieN */)){
                  var _86/*   sieA */ = _80/* sieA */;
                  _7W/*  siez */ = _82/* sieE */;
                  _7X/*  sieA */ = _86/*   sieA */;
                  return __continue/* EXTERNAL */;
                }else{
                  var _87/* sieS */ = __get/* EXTERNAL */(_6g/* sidW */, _84/* sieJ */),
                  _88/* sieW */ = __get/* EXTERNAL */(_87/* sieS */, _7q/* sieh */),
                  _89/* sif0 */ = String/* EXTERNAL */(_88/* sieW */),
                  _8a/* sif4 */ = strEq/* EXTERNAL */(_89/* sif0 */, _7t/* sier */);
                  if(!E(_8a/* sif4 */)){
                    var _8b/* sife */ = strEq/* EXTERNAL */(_89/* sif0 */, E(_68/* LudoJS.lvl33 */));
                    if(!E(_8b/* sife */)){
                      return E(_67/* LudoJS.lvl32 */);
                    }else{
                      var _8c/* sifl */ = __get/* EXTERNAL */(_87/* sieS */, E(_1M/* LudoJS.$fToAnyOption1 */)),
                      _8d/* sifA */ = new T(function(){
                        return B(_q/* GHC.Base.++ */(_80/* sieA */, new T2(1,new T2(0,_83/* sieF */,new T1(1,new T(function(){
                          var _8e/* sifp */ = Number/* EXTERNAL */(_8c/* sifl */);
                          return jsTrunc/* EXTERNAL */(_8e/* sifp */);
                        }))),_4/* GHC.Types.[] */)));
                      });
                      _7W/*  siez */ = _82/* sieE */;
                      _7X/*  sieA */ = _8d/* sifA */;
                      return __continue/* EXTERNAL */;
                    }
                  }else{
                    _7W/*  siez */ = _82/* sieE */;
                    _7X/*  sieA */ = new T(function(){
                      return B(_q/* GHC.Base.++ */(_80/* sieA */, new T2(1,new T2(0,_83/* sieF */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                    });
                    return __continue/* EXTERNAL */;
                  }
                }
              }
            })(_7W/*  siez */, _7X/*  sieA */, _/* EXTERNAL */));
            if(_7Y/*  siey */!=__continue/* EXTERNAL */){
              return _7Y/*  siey */;
            }
          }
        };
        return new F(function(){return _7V/* siey */(_7l/* sie0 */, new T(function(){
          return B(_q/* GHC.Base.++ */(_7m/* sie1 */, new T2(1,new T2(0,_7k/* sidZ */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
        }), _/* EXTERNAL */);});
      }
    }
  },
  _8f/* sikj */ = B(_7j/* sidX */(1, _66/* LudoJS.a34 */, _4/* GHC.Types.[] */, _/* EXTERNAL */)),
  _8g/* sikn */ = function(_8h/* sir4 */, _/* EXTERNAL */){
    var _8i/* sir6 */ = E(_8h/* sir4 */);
    if(!_8i/* sir6 */._){
      return _4/* GHC.Types.[] */;
    }else{
      var _8j/* sir7 */ = _8i/* sir6 */.a,
      _8k/* sirf */ = __get/* EXTERNAL */(_6e/* sidM */, toJSStr/* EXTERNAL */(B(_5M/* LudoJS.$w$cshowsPrec */(_8j/* sir7 */, _4/* GHC.Types.[] */)))),
      _8l/* sirh */ = _8k/* sirf */,
      _8m/* sirj */ = function(_8n/*  siut */, _8o/*  siuu */, _/* EXTERNAL */){
        while(1){
          var _8p/*  sirj */ = B((function(_8q/* siut */, _8r/* siuu */, _/* EXTERNAL */){
            var _8s/* siuw */ = E(_8q/* siut */);
            if(!_8s/* siuw */._){
              return _8r/* siuu */;
            }else{
              var _8t/* siuy */ = _8s/* siuw */.b,
              _8u/* siuz */ = E(_8s/* siuw */.a),
              _8v/* siuD */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _8u/* siuz */, _4/* GHC.Types.[] */))),
              _8w/* siuH */ = __has/* EXTERNAL */(_8l/* sirh */, _8v/* siuD */);
              if(!E(_8w/* siuH */)){
                var _8x/*   siuu */ = _8r/* siuu */;
                _8n/*  siut */ = _8t/* siuy */;
                _8o/*  siuu */ = _8x/*   siuu */;
                return __continue/* EXTERNAL */;
              }else{
                var _8y/* siuM */ = __get/* EXTERNAL */(_8l/* sirh */, _8v/* siuD */),
                _8z/* siuP */ = E(_1N/* LudoJS.$fToAnyOption5 */),
                _8A/* siuS */ = __get/* EXTERNAL */(_8y/* siuM */, _8z/* siuP */),
                _8B/* siuW */ = String/* EXTERNAL */(_8A/* siuS */),
                _8C/* siuZ */ = E(_69/* LudoJS.lvl34 */),
                _8D/* siv2 */ = strEq/* EXTERNAL */(_8B/* siuW */, _8C/* siuZ */);
                if(!E(_8D/* siv2 */)){
                  var _8E/* siwd */ = E(_68/* LudoJS.lvl33 */),
                  _8F/* siwg */ = strEq/* EXTERNAL */(_8B/* siuW */, _8E/* siwd */);
                  if(!E(_8F/* siwg */)){
                    return E(_67/* LudoJS.lvl32 */);
                  }else{
                    var _8G/* siwk */ = E(_1M/* LudoJS.$fToAnyOption1 */),
                    _8H/* siwn */ = __get/* EXTERNAL */(_8y/* siuM */, _8G/* siwk */),
                    _8I/* siwq */ = function(_8J/*  siwr */, _8K/*  siws */, _/* EXTERNAL */){
                      while(1){
                        var _8L/*  siwq */ = B((function(_8M/* siwr */, _8N/* siws */, _/* EXTERNAL */){
                          var _8O/* siwu */ = E(_8M/* siwr */);
                          if(!_8O/* siwu */._){
                            return _8N/* siws */;
                          }else{
                            var _8P/* siww */ = _8O/* siwu */.b,
                            _8Q/* siwx */ = E(_8O/* siwu */.a),
                            _8R/* siwB */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _8Q/* siwx */, _4/* GHC.Types.[] */))),
                            _8S/* siwF */ = __has/* EXTERNAL */(_8l/* sirh */, _8R/* siwB */);
                            if(!E(_8S/* siwF */)){
                              var _8T/*   siws */ = _8N/* siws */;
                              _8J/*  siwr */ = _8P/* siww */;
                              _8K/*  siws */ = _8T/*   siws */;
                              return __continue/* EXTERNAL */;
                            }else{
                              var _8U/* siwK */ = __get/* EXTERNAL */(_8l/* sirh */, _8R/* siwB */),
                              _8V/* siwO */ = __get/* EXTERNAL */(_8U/* siwK */, _8z/* siuP */),
                              _8W/* siwS */ = String/* EXTERNAL */(_8V/* siwO */),
                              _8X/* siwW */ = strEq/* EXTERNAL */(_8W/* siwS */, _8C/* siuZ */);
                              if(!E(_8X/* siwW */)){
                                var _8Y/* six4 */ = strEq/* EXTERNAL */(_8W/* siwS */, _8E/* siwd */);
                                if(!E(_8Y/* six4 */)){
                                  return E(_67/* LudoJS.lvl32 */);
                                }else{
                                  var _8Z/* six9 */ = __get/* EXTERNAL */(_8U/* siwK */, _8G/* siwk */),
                                  _90/* sixo */ = new T(function(){
                                    return B(_q/* GHC.Base.++ */(_8N/* siws */, new T2(1,new T2(0,_8Q/* siwx */,new T1(1,new T(function(){
                                      var _91/* sixd */ = Number/* EXTERNAL */(_8Z/* six9 */);
                                      return jsTrunc/* EXTERNAL */(_91/* sixd */);
                                    }))),_4/* GHC.Types.[] */)));
                                  });
                                  _8J/*  siwr */ = _8P/* siww */;
                                  _8K/*  siws */ = _90/* sixo */;
                                  return __continue/* EXTERNAL */;
                                }
                              }else{
                                _8J/*  siwr */ = _8P/* siww */;
                                _8K/*  siws */ = new T(function(){
                                  return B(_q/* GHC.Base.++ */(_8N/* siws */, new T2(1,new T2(0,_8Q/* siwx */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                                });
                                return __continue/* EXTERNAL */;
                              }
                            }
                          }
                        })(_8J/*  siwr */, _8K/*  siws */, _/* EXTERNAL */));
                        if(_8L/*  siwq */!=__continue/* EXTERNAL */){
                          return _8L/*  siwq */;
                        }
                      }
                    },
                    _92/* sixC */ = new T(function(){
                      return B(_q/* GHC.Base.++ */(_8r/* siuu */, new T2(1,new T2(0,_8u/* siuz */,new T1(1,new T(function(){
                        var _93/* sixr */ = Number/* EXTERNAL */(_8H/* siwn */);
                        return jsTrunc/* EXTERNAL */(_93/* sixr */);
                      }))),_4/* GHC.Types.[] */)));
                    });
                    return new F(function(){return _8I/* siwq */(_8t/* siuy */, _92/* sixC */, _/* EXTERNAL */);});
                  }
                }else{
                  var _94/* siv6 */ = function(_95/*  siv7 */, _96/*  siv8 */, _/* EXTERNAL */){
                    while(1){
                      var _97/*  siv6 */ = B((function(_98/* siv7 */, _99/* siv8 */, _/* EXTERNAL */){
                        var _9a/* siva */ = E(_98/* siv7 */);
                        if(!_9a/* siva */._){
                          return _99/* siv8 */;
                        }else{
                          var _9b/* sivc */ = _9a/* siva */.b,
                          _9c/* sivd */ = E(_9a/* siva */.a),
                          _9d/* sivh */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _9c/* sivd */, _4/* GHC.Types.[] */))),
                          _9e/* sivl */ = __has/* EXTERNAL */(_8l/* sirh */, _9d/* sivh */);
                          if(!E(_9e/* sivl */)){
                            var _9f/*   siv8 */ = _99/* siv8 */;
                            _95/*  siv7 */ = _9b/* sivc */;
                            _96/*  siv8 */ = _9f/*   siv8 */;
                            return __continue/* EXTERNAL */;
                          }else{
                            var _9g/* sivq */ = __get/* EXTERNAL */(_8l/* sirh */, _9d/* sivh */),
                            _9h/* sivu */ = __get/* EXTERNAL */(_9g/* sivq */, _8z/* siuP */),
                            _9i/* sivy */ = String/* EXTERNAL */(_9h/* sivu */),
                            _9j/* sivC */ = strEq/* EXTERNAL */(_9i/* sivy */, _8C/* siuZ */);
                            if(!E(_9j/* sivC */)){
                              var _9k/* sivM */ = strEq/* EXTERNAL */(_9i/* sivy */, E(_68/* LudoJS.lvl33 */));
                              if(!E(_9k/* sivM */)){
                                return E(_67/* LudoJS.lvl32 */);
                              }else{
                                var _9l/* sivT */ = __get/* EXTERNAL */(_9g/* sivq */, E(_1M/* LudoJS.$fToAnyOption1 */)),
                                _9m/* siw8 */ = new T(function(){
                                  return B(_q/* GHC.Base.++ */(_99/* siv8 */, new T2(1,new T2(0,_9c/* sivd */,new T1(1,new T(function(){
                                    var _9n/* sivX */ = Number/* EXTERNAL */(_9l/* sivT */);
                                    return jsTrunc/* EXTERNAL */(_9n/* sivX */);
                                  }))),_4/* GHC.Types.[] */)));
                                });
                                _95/*  siv7 */ = _9b/* sivc */;
                                _96/*  siv8 */ = _9m/* siw8 */;
                                return __continue/* EXTERNAL */;
                              }
                            }else{
                              _95/*  siv7 */ = _9b/* sivc */;
                              _96/*  siv8 */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_99/* siv8 */, new T2(1,new T2(0,_9c/* sivd */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                              });
                              return __continue/* EXTERNAL */;
                            }
                          }
                        }
                      })(_95/*  siv7 */, _96/*  siv8 */, _/* EXTERNAL */));
                      if(_97/*  siv6 */!=__continue/* EXTERNAL */){
                        return _97/*  siv6 */;
                      }
                    }
                  };
                  return new F(function(){return _94/* siv6 */(_8t/* siuy */, new T(function(){
                    return B(_q/* GHC.Base.++ */(_8r/* siuu */, new T2(1,new T2(0,_8u/* siuz */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                  }), _/* EXTERNAL */);});
                }
              }
            }
          })(_8n/*  siut */, _8o/*  siuu */, _/* EXTERNAL */));
          if(_8p/*  sirj */!=__continue/* EXTERNAL */){
            return _8p/*  sirj */;
          }
        }
      },
      _9o/* siri */ = function(_9p/* sirk */, _9q/* sirl */, _9r/* sirm */, _/* EXTERNAL */){
        var _9s/* sirq */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _9p/* sirk */, _4/* GHC.Types.[] */))),
        _9t/* siru */ = __has/* EXTERNAL */(_8l/* sirh */, _9s/* sirq */);
        if(!E(_9t/* siru */)){
          return new F(function(){return _8m/* sirj */(_9q/* sirl */, _9r/* sirm */, _/* EXTERNAL */);});
        }else{
          var _9u/* sirz */ = __get/* EXTERNAL */(_8l/* sirh */, _9s/* sirq */),
          _9v/* sirC */ = E(_1N/* LudoJS.$fToAnyOption5 */),
          _9w/* sirF */ = __get/* EXTERNAL */(_9u/* sirz */, _9v/* sirC */),
          _9x/* sirJ */ = String/* EXTERNAL */(_9w/* sirF */),
          _9y/* sirM */ = E(_69/* LudoJS.lvl34 */),
          _9z/* sirP */ = strEq/* EXTERNAL */(_9x/* sirJ */, _9y/* sirM */);
          if(!E(_9z/* sirP */)){
            var _9A/* sit1 */ = E(_68/* LudoJS.lvl33 */),
            _9B/* sit4 */ = strEq/* EXTERNAL */(_9x/* sirJ */, _9A/* sit1 */);
            if(!E(_9B/* sit4 */)){
              return E(_67/* LudoJS.lvl32 */);
            }else{
              var _9C/* sit8 */ = E(_1M/* LudoJS.$fToAnyOption1 */),
              _9D/* sitb */ = __get/* EXTERNAL */(_9u/* sirz */, _9C/* sit8 */),
              _9E/* site */ = function(_9F/*  sitf */, _9G/*  sitg */, _/* EXTERNAL */){
                while(1){
                  var _9H/*  site */ = B((function(_9I/* sitf */, _9J/* sitg */, _/* EXTERNAL */){
                    var _9K/* siti */ = E(_9I/* sitf */);
                    if(!_9K/* siti */._){
                      return _9J/* sitg */;
                    }else{
                      var _9L/* sitk */ = _9K/* siti */.b,
                      _9M/* sitl */ = E(_9K/* siti */.a),
                      _9N/* sitp */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _9M/* sitl */, _4/* GHC.Types.[] */))),
                      _9O/* sitt */ = __has/* EXTERNAL */(_8l/* sirh */, _9N/* sitp */);
                      if(!E(_9O/* sitt */)){
                        var _9P/*   sitg */ = _9J/* sitg */;
                        _9F/*  sitf */ = _9L/* sitk */;
                        _9G/*  sitg */ = _9P/*   sitg */;
                        return __continue/* EXTERNAL */;
                      }else{
                        var _9Q/* sity */ = __get/* EXTERNAL */(_8l/* sirh */, _9N/* sitp */),
                        _9R/* sitC */ = __get/* EXTERNAL */(_9Q/* sity */, _9v/* sirC */),
                        _9S/* sitG */ = String/* EXTERNAL */(_9R/* sitC */),
                        _9T/* sitK */ = strEq/* EXTERNAL */(_9S/* sitG */, _9y/* sirM */);
                        if(!E(_9T/* sitK */)){
                          var _9U/* sitS */ = strEq/* EXTERNAL */(_9S/* sitG */, _9A/* sit1 */);
                          if(!E(_9U/* sitS */)){
                            return E(_67/* LudoJS.lvl32 */);
                          }else{
                            var _9V/* sitX */ = __get/* EXTERNAL */(_9Q/* sity */, _9C/* sit8 */),
                            _9W/* siuc */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_9J/* sitg */, new T2(1,new T2(0,_9M/* sitl */,new T1(1,new T(function(){
                                var _9X/* siu1 */ = Number/* EXTERNAL */(_9V/* sitX */);
                                return jsTrunc/* EXTERNAL */(_9X/* siu1 */);
                              }))),_4/* GHC.Types.[] */)));
                            });
                            _9F/*  sitf */ = _9L/* sitk */;
                            _9G/*  sitg */ = _9W/* siuc */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          _9F/*  sitf */ = _9L/* sitk */;
                          _9G/*  sitg */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_9J/* sitg */, new T2(1,new T2(0,_9M/* sitl */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                          });
                          return __continue/* EXTERNAL */;
                        }
                      }
                    }
                  })(_9F/*  sitf */, _9G/*  sitg */, _/* EXTERNAL */));
                  if(_9H/*  site */!=__continue/* EXTERNAL */){
                    return _9H/*  site */;
                  }
                }
              },
              _9Y/* siur */ = new T(function(){
                return B(_q/* GHC.Base.++ */(_9r/* sirm */, new T2(1,new T2(0,_9p/* sirk */,new T1(1,new T(function(){
                  var _9Z/* siug */ = Number/* EXTERNAL */(_9D/* sitb */);
                  return jsTrunc/* EXTERNAL */(_9Z/* siug */);
                }))),_4/* GHC.Types.[] */)));
              });
              return new F(function(){return _9E/* site */(_9q/* sirl */, _9Y/* siur */, _/* EXTERNAL */);});
            }
          }else{
            var _a0/* sirT */ = function(_a1/*  sirU */, _a2/*  sirV */, _/* EXTERNAL */){
              while(1){
                var _a3/*  sirT */ = B((function(_a4/* sirU */, _a5/* sirV */, _/* EXTERNAL */){
                  var _a6/* sirX */ = E(_a4/* sirU */);
                  if(!_a6/* sirX */._){
                    return _a5/* sirV */;
                  }else{
                    var _a7/* sirZ */ = _a6/* sirX */.b,
                    _a8/* sis0 */ = E(_a6/* sirX */.a),
                    _a9/* sis4 */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _a8/* sis0 */, _4/* GHC.Types.[] */))),
                    _aa/* sis8 */ = __has/* EXTERNAL */(_8l/* sirh */, _a9/* sis4 */);
                    if(!E(_aa/* sis8 */)){
                      var _ab/*   sirV */ = _a5/* sirV */;
                      _a1/*  sirU */ = _a7/* sirZ */;
                      _a2/*  sirV */ = _ab/*   sirV */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _ac/* sisd */ = __get/* EXTERNAL */(_8l/* sirh */, _a9/* sis4 */),
                      _ad/* sish */ = __get/* EXTERNAL */(_ac/* sisd */, _9v/* sirC */),
                      _ae/* sisl */ = String/* EXTERNAL */(_ad/* sish */),
                      _af/* sisp */ = strEq/* EXTERNAL */(_ae/* sisl */, _9y/* sirM */);
                      if(!E(_af/* sisp */)){
                        var _ag/* sisz */ = strEq/* EXTERNAL */(_ae/* sisl */, E(_68/* LudoJS.lvl33 */));
                        if(!E(_ag/* sisz */)){
                          return E(_67/* LudoJS.lvl32 */);
                        }else{
                          var _ah/* sisG */ = __get/* EXTERNAL */(_ac/* sisd */, E(_1M/* LudoJS.$fToAnyOption1 */)),
                          _ai/* sisV */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_a5/* sirV */, new T2(1,new T2(0,_a8/* sis0 */,new T1(1,new T(function(){
                              var _aj/* sisK */ = Number/* EXTERNAL */(_ah/* sisG */);
                              return jsTrunc/* EXTERNAL */(_aj/* sisK */);
                            }))),_4/* GHC.Types.[] */)));
                          });
                          _a1/*  sirU */ = _a7/* sirZ */;
                          _a2/*  sirV */ = _ai/* sisV */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        _a1/*  sirU */ = _a7/* sirZ */;
                        _a2/*  sirV */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_a5/* sirV */, new T2(1,new T2(0,_a8/* sis0 */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                        });
                        return __continue/* EXTERNAL */;
                      }
                    }
                  }
                })(_a1/*  sirU */, _a2/*  sirV */, _/* EXTERNAL */));
                if(_a3/*  sirT */!=__continue/* EXTERNAL */){
                  return _a3/*  sirT */;
                }
              }
            };
            return new F(function(){return _a0/* sirT */(_9q/* sirl */, new T(function(){
              return B(_q/* GHC.Base.++ */(_9r/* sirm */, new T2(1,new T2(0,_9p/* sirk */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
            }), _/* EXTERNAL */);});
          }
        }
      },
      _ak/* sixE */ = B(_9o/* siri */(1, _66/* LudoJS.a34 */, _4/* GHC.Types.[] */, _/* EXTERNAL */)),
      _al/* sixH */ = B(_8g/* sikn */(_8i/* sir6 */.b, _/* EXTERNAL */));
      return new T2(1,new T2(0,_8j/* sir7 */,_ak/* sixE */),_al/* sixH */);
    }
  },
  _am/* sikm */ = function(_an/* siko */, _ao/* sikp */, _/* EXTERNAL */){
    var _ap/* sikx */ = __get/* EXTERNAL */(_6e/* sidM */, toJSStr/* EXTERNAL */(B(_5M/* LudoJS.$w$cshowsPrec */(_an/* siko */, _4/* GHC.Types.[] */)))),
    _aq/* sikz */ = _ap/* sikx */,
    _ar/* sikB */ = function(_as/*  sinL */, _at/*  sinM */, _/* EXTERNAL */){
      while(1){
        var _au/*  sikB */ = B((function(_av/* sinL */, _aw/* sinM */, _/* EXTERNAL */){
          var _ax/* sinO */ = E(_av/* sinL */);
          if(!_ax/* sinO */._){
            return _aw/* sinM */;
          }else{
            var _ay/* sinQ */ = _ax/* sinO */.b,
            _az/* sinR */ = E(_ax/* sinO */.a),
            _aA/* sinV */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _az/* sinR */, _4/* GHC.Types.[] */))),
            _aB/* sinZ */ = __has/* EXTERNAL */(_aq/* sikz */, _aA/* sinV */);
            if(!E(_aB/* sinZ */)){
              var _aC/*   sinM */ = _aw/* sinM */;
              _as/*  sinL */ = _ay/* sinQ */;
              _at/*  sinM */ = _aC/*   sinM */;
              return __continue/* EXTERNAL */;
            }else{
              var _aD/* sio4 */ = __get/* EXTERNAL */(_aq/* sikz */, _aA/* sinV */),
              _aE/* sio7 */ = E(_1N/* LudoJS.$fToAnyOption5 */),
              _aF/* sioa */ = __get/* EXTERNAL */(_aD/* sio4 */, _aE/* sio7 */),
              _aG/* sioe */ = String/* EXTERNAL */(_aF/* sioa */),
              _aH/* sioh */ = E(_69/* LudoJS.lvl34 */),
              _aI/* siok */ = strEq/* EXTERNAL */(_aG/* sioe */, _aH/* sioh */);
              if(!E(_aI/* siok */)){
                var _aJ/* sipv */ = E(_68/* LudoJS.lvl33 */),
                _aK/* sipy */ = strEq/* EXTERNAL */(_aG/* sioe */, _aJ/* sipv */);
                if(!E(_aK/* sipy */)){
                  return E(_67/* LudoJS.lvl32 */);
                }else{
                  var _aL/* sipC */ = E(_1M/* LudoJS.$fToAnyOption1 */),
                  _aM/* sipF */ = __get/* EXTERNAL */(_aD/* sio4 */, _aL/* sipC */),
                  _aN/* sipI */ = function(_aO/*  sipJ */, _aP/*  sipK */, _/* EXTERNAL */){
                    while(1){
                      var _aQ/*  sipI */ = B((function(_aR/* sipJ */, _aS/* sipK */, _/* EXTERNAL */){
                        var _aT/* sipM */ = E(_aR/* sipJ */);
                        if(!_aT/* sipM */._){
                          return _aS/* sipK */;
                        }else{
                          var _aU/* sipO */ = _aT/* sipM */.b,
                          _aV/* sipP */ = E(_aT/* sipM */.a),
                          _aW/* sipT */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _aV/* sipP */, _4/* GHC.Types.[] */))),
                          _aX/* sipX */ = __has/* EXTERNAL */(_aq/* sikz */, _aW/* sipT */);
                          if(!E(_aX/* sipX */)){
                            var _aY/*   sipK */ = _aS/* sipK */;
                            _aO/*  sipJ */ = _aU/* sipO */;
                            _aP/*  sipK */ = _aY/*   sipK */;
                            return __continue/* EXTERNAL */;
                          }else{
                            var _aZ/* siq2 */ = __get/* EXTERNAL */(_aq/* sikz */, _aW/* sipT */),
                            _b0/* siq6 */ = __get/* EXTERNAL */(_aZ/* siq2 */, _aE/* sio7 */),
                            _b1/* siqa */ = String/* EXTERNAL */(_b0/* siq6 */),
                            _b2/* siqe */ = strEq/* EXTERNAL */(_b1/* siqa */, _aH/* sioh */);
                            if(!E(_b2/* siqe */)){
                              var _b3/* siqm */ = strEq/* EXTERNAL */(_b1/* siqa */, _aJ/* sipv */);
                              if(!E(_b3/* siqm */)){
                                return E(_67/* LudoJS.lvl32 */);
                              }else{
                                var _b4/* siqr */ = __get/* EXTERNAL */(_aZ/* siq2 */, _aL/* sipC */),
                                _b5/* siqG */ = new T(function(){
                                  return B(_q/* GHC.Base.++ */(_aS/* sipK */, new T2(1,new T2(0,_aV/* sipP */,new T1(1,new T(function(){
                                    var _b6/* siqv */ = Number/* EXTERNAL */(_b4/* siqr */);
                                    return jsTrunc/* EXTERNAL */(_b6/* siqv */);
                                  }))),_4/* GHC.Types.[] */)));
                                });
                                _aO/*  sipJ */ = _aU/* sipO */;
                                _aP/*  sipK */ = _b5/* siqG */;
                                return __continue/* EXTERNAL */;
                              }
                            }else{
                              _aO/*  sipJ */ = _aU/* sipO */;
                              _aP/*  sipK */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_aS/* sipK */, new T2(1,new T2(0,_aV/* sipP */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                              });
                              return __continue/* EXTERNAL */;
                            }
                          }
                        }
                      })(_aO/*  sipJ */, _aP/*  sipK */, _/* EXTERNAL */));
                      if(_aQ/*  sipI */!=__continue/* EXTERNAL */){
                        return _aQ/*  sipI */;
                      }
                    }
                  },
                  _b7/* siqU */ = new T(function(){
                    return B(_q/* GHC.Base.++ */(_aw/* sinM */, new T2(1,new T2(0,_az/* sinR */,new T1(1,new T(function(){
                      var _b8/* siqJ */ = Number/* EXTERNAL */(_aM/* sipF */);
                      return jsTrunc/* EXTERNAL */(_b8/* siqJ */);
                    }))),_4/* GHC.Types.[] */)));
                  });
                  return new F(function(){return _aN/* sipI */(_ay/* sinQ */, _b7/* siqU */, _/* EXTERNAL */);});
                }
              }else{
                var _b9/* sioo */ = function(_ba/*  siop */, _bb/*  sioq */, _/* EXTERNAL */){
                  while(1){
                    var _bc/*  sioo */ = B((function(_bd/* siop */, _be/* sioq */, _/* EXTERNAL */){
                      var _bf/* sios */ = E(_bd/* siop */);
                      if(!_bf/* sios */._){
                        return _be/* sioq */;
                      }else{
                        var _bg/* siou */ = _bf/* sios */.b,
                        _bh/* siov */ = E(_bf/* sios */.a),
                        _bi/* sioz */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _bh/* siov */, _4/* GHC.Types.[] */))),
                        _bj/* sioD */ = __has/* EXTERNAL */(_aq/* sikz */, _bi/* sioz */);
                        if(!E(_bj/* sioD */)){
                          var _bk/*   sioq */ = _be/* sioq */;
                          _ba/*  siop */ = _bg/* siou */;
                          _bb/*  sioq */ = _bk/*   sioq */;
                          return __continue/* EXTERNAL */;
                        }else{
                          var _bl/* sioI */ = __get/* EXTERNAL */(_aq/* sikz */, _bi/* sioz */),
                          _bm/* sioM */ = __get/* EXTERNAL */(_bl/* sioI */, _aE/* sio7 */),
                          _bn/* sioQ */ = String/* EXTERNAL */(_bm/* sioM */),
                          _bo/* sioU */ = strEq/* EXTERNAL */(_bn/* sioQ */, _aH/* sioh */);
                          if(!E(_bo/* sioU */)){
                            var _bp/* sip4 */ = strEq/* EXTERNAL */(_bn/* sioQ */, E(_68/* LudoJS.lvl33 */));
                            if(!E(_bp/* sip4 */)){
                              return E(_67/* LudoJS.lvl32 */);
                            }else{
                              var _bq/* sipb */ = __get/* EXTERNAL */(_bl/* sioI */, E(_1M/* LudoJS.$fToAnyOption1 */)),
                              _br/* sipq */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_be/* sioq */, new T2(1,new T2(0,_bh/* siov */,new T1(1,new T(function(){
                                  var _bs/* sipf */ = Number/* EXTERNAL */(_bq/* sipb */);
                                  return jsTrunc/* EXTERNAL */(_bs/* sipf */);
                                }))),_4/* GHC.Types.[] */)));
                              });
                              _ba/*  siop */ = _bg/* siou */;
                              _bb/*  sioq */ = _br/* sipq */;
                              return __continue/* EXTERNAL */;
                            }
                          }else{
                            _ba/*  siop */ = _bg/* siou */;
                            _bb/*  sioq */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_be/* sioq */, new T2(1,new T2(0,_bh/* siov */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                            });
                            return __continue/* EXTERNAL */;
                          }
                        }
                      }
                    })(_ba/*  siop */, _bb/*  sioq */, _/* EXTERNAL */));
                    if(_bc/*  sioo */!=__continue/* EXTERNAL */){
                      return _bc/*  sioo */;
                    }
                  }
                };
                return new F(function(){return _b9/* sioo */(_ay/* sinQ */, new T(function(){
                  return B(_q/* GHC.Base.++ */(_aw/* sinM */, new T2(1,new T2(0,_az/* sinR */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                }), _/* EXTERNAL */);});
              }
            }
          }
        })(_as/*  sinL */, _at/*  sinM */, _/* EXTERNAL */));
        if(_au/*  sikB */!=__continue/* EXTERNAL */){
          return _au/*  sikB */;
        }
      }
    },
    _bt/* sikA */ = function(_bu/* sikC */, _bv/* sikD */, _bw/* sikE */, _/* EXTERNAL */){
      var _bx/* sikI */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _bu/* sikC */, _4/* GHC.Types.[] */))),
      _by/* sikM */ = __has/* EXTERNAL */(_aq/* sikz */, _bx/* sikI */);
      if(!E(_by/* sikM */)){
        return new F(function(){return _ar/* sikB */(_bv/* sikD */, _bw/* sikE */, _/* EXTERNAL */);});
      }else{
        var _bz/* sikR */ = __get/* EXTERNAL */(_aq/* sikz */, _bx/* sikI */),
        _bA/* sikU */ = E(_1N/* LudoJS.$fToAnyOption5 */),
        _bB/* sikX */ = __get/* EXTERNAL */(_bz/* sikR */, _bA/* sikU */),
        _bC/* sil1 */ = String/* EXTERNAL */(_bB/* sikX */),
        _bD/* sil4 */ = E(_69/* LudoJS.lvl34 */),
        _bE/* sil7 */ = strEq/* EXTERNAL */(_bC/* sil1 */, _bD/* sil4 */);
        if(!E(_bE/* sil7 */)){
          var _bF/* simj */ = E(_68/* LudoJS.lvl33 */),
          _bG/* simm */ = strEq/* EXTERNAL */(_bC/* sil1 */, _bF/* simj */);
          if(!E(_bG/* simm */)){
            return E(_67/* LudoJS.lvl32 */);
          }else{
            var _bH/* simq */ = E(_1M/* LudoJS.$fToAnyOption1 */),
            _bI/* simt */ = __get/* EXTERNAL */(_bz/* sikR */, _bH/* simq */),
            _bJ/* simw */ = function(_bK/*  simx */, _bL/*  simy */, _/* EXTERNAL */){
              while(1){
                var _bM/*  simw */ = B((function(_bN/* simx */, _bO/* simy */, _/* EXTERNAL */){
                  var _bP/* simA */ = E(_bN/* simx */);
                  if(!_bP/* simA */._){
                    return _bO/* simy */;
                  }else{
                    var _bQ/* simC */ = _bP/* simA */.b,
                    _bR/* simD */ = E(_bP/* simA */.a),
                    _bS/* simH */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _bR/* simD */, _4/* GHC.Types.[] */))),
                    _bT/* simL */ = __has/* EXTERNAL */(_aq/* sikz */, _bS/* simH */);
                    if(!E(_bT/* simL */)){
                      var _bU/*   simy */ = _bO/* simy */;
                      _bK/*  simx */ = _bQ/* simC */;
                      _bL/*  simy */ = _bU/*   simy */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _bV/* simQ */ = __get/* EXTERNAL */(_aq/* sikz */, _bS/* simH */),
                      _bW/* simU */ = __get/* EXTERNAL */(_bV/* simQ */, _bA/* sikU */),
                      _bX/* simY */ = String/* EXTERNAL */(_bW/* simU */),
                      _bY/* sin2 */ = strEq/* EXTERNAL */(_bX/* simY */, _bD/* sil4 */);
                      if(!E(_bY/* sin2 */)){
                        var _bZ/* sina */ = strEq/* EXTERNAL */(_bX/* simY */, _bF/* simj */);
                        if(!E(_bZ/* sina */)){
                          return E(_67/* LudoJS.lvl32 */);
                        }else{
                          var _c0/* sinf */ = __get/* EXTERNAL */(_bV/* simQ */, _bH/* simq */),
                          _c1/* sinu */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_bO/* simy */, new T2(1,new T2(0,_bR/* simD */,new T1(1,new T(function(){
                              var _c2/* sinj */ = Number/* EXTERNAL */(_c0/* sinf */);
                              return jsTrunc/* EXTERNAL */(_c2/* sinj */);
                            }))),_4/* GHC.Types.[] */)));
                          });
                          _bK/*  simx */ = _bQ/* simC */;
                          _bL/*  simy */ = _c1/* sinu */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        _bK/*  simx */ = _bQ/* simC */;
                        _bL/*  simy */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_bO/* simy */, new T2(1,new T2(0,_bR/* simD */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                        });
                        return __continue/* EXTERNAL */;
                      }
                    }
                  }
                })(_bK/*  simx */, _bL/*  simy */, _/* EXTERNAL */));
                if(_bM/*  simw */!=__continue/* EXTERNAL */){
                  return _bM/*  simw */;
                }
              }
            },
            _c3/* sinJ */ = new T(function(){
              return B(_q/* GHC.Base.++ */(_bw/* sikE */, new T2(1,new T2(0,_bu/* sikC */,new T1(1,new T(function(){
                var _c4/* siny */ = Number/* EXTERNAL */(_bI/* simt */);
                return jsTrunc/* EXTERNAL */(_c4/* siny */);
              }))),_4/* GHC.Types.[] */)));
            });
            return new F(function(){return _bJ/* simw */(_bv/* sikD */, _c3/* sinJ */, _/* EXTERNAL */);});
          }
        }else{
          var _c5/* silb */ = function(_c6/*  silc */, _c7/*  sild */, _/* EXTERNAL */){
            while(1){
              var _c8/*  silb */ = B((function(_c9/* silc */, _ca/* sild */, _/* EXTERNAL */){
                var _cb/* silf */ = E(_c9/* silc */);
                if(!_cb/* silf */._){
                  return _ca/* sild */;
                }else{
                  var _cc/* silh */ = _cb/* silf */.b,
                  _cd/* sili */ = E(_cb/* silf */.a),
                  _ce/* silm */ = toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, _cd/* sili */, _4/* GHC.Types.[] */))),
                  _cf/* silq */ = __has/* EXTERNAL */(_aq/* sikz */, _ce/* silm */);
                  if(!E(_cf/* silq */)){
                    var _cg/*   sild */ = _ca/* sild */;
                    _c6/*  silc */ = _cc/* silh */;
                    _c7/*  sild */ = _cg/*   sild */;
                    return __continue/* EXTERNAL */;
                  }else{
                    var _ch/* silv */ = __get/* EXTERNAL */(_aq/* sikz */, _ce/* silm */),
                    _ci/* silz */ = __get/* EXTERNAL */(_ch/* silv */, _bA/* sikU */),
                    _cj/* silD */ = String/* EXTERNAL */(_ci/* silz */),
                    _ck/* silH */ = strEq/* EXTERNAL */(_cj/* silD */, _bD/* sil4 */);
                    if(!E(_ck/* silH */)){
                      var _cl/* silR */ = strEq/* EXTERNAL */(_cj/* silD */, E(_68/* LudoJS.lvl33 */));
                      if(!E(_cl/* silR */)){
                        return E(_67/* LudoJS.lvl32 */);
                      }else{
                        var _cm/* silY */ = __get/* EXTERNAL */(_ch/* silv */, E(_1M/* LudoJS.$fToAnyOption1 */)),
                        _cn/* simd */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_ca/* sild */, new T2(1,new T2(0,_cd/* sili */,new T1(1,new T(function(){
                            var _co/* sim2 */ = Number/* EXTERNAL */(_cm/* silY */);
                            return jsTrunc/* EXTERNAL */(_co/* sim2 */);
                          }))),_4/* GHC.Types.[] */)));
                        });
                        _c6/*  silc */ = _cc/* silh */;
                        _c7/*  sild */ = _cn/* simd */;
                        return __continue/* EXTERNAL */;
                      }
                    }else{
                      _c6/*  silc */ = _cc/* silh */;
                      _c7/*  sild */ = new T(function(){
                        return B(_q/* GHC.Base.++ */(_ca/* sild */, new T2(1,new T2(0,_cd/* sili */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                      });
                      return __continue/* EXTERNAL */;
                    }
                  }
                }
              })(_c6/*  silc */, _c7/*  sild */, _/* EXTERNAL */));
              if(_c8/*  silb */!=__continue/* EXTERNAL */){
                return _c8/*  silb */;
              }
            }
          };
          return new F(function(){return _c5/* silb */(_bv/* sikD */, new T(function(){
            return B(_q/* GHC.Base.++ */(_bw/* sikE */, new T2(1,new T2(0,_bu/* sikC */,_60/* LudoJS.Out */),_4/* GHC.Types.[] */)));
          }), _/* EXTERNAL */);});
        }
      }
    },
    _cp/* siqW */ = B(_bt/* sikA */(1, _66/* LudoJS.a34 */, _4/* GHC.Types.[] */, _/* EXTERNAL */)),
    _cq/* siqZ */ = B(_8g/* sikn */(_ao/* sikp */, _/* EXTERNAL */));
    return new T2(1,new T2(0,_an/* siko */,_cp/* siqW */),_cq/* siqZ */);
  },
  _cr/* sixM */ = B(_am/* sikm */(_1P/* LudoJS.Green */, _6b/* LudoJS.lvl8 */, _/* EXTERNAL */));
  return new T(function(){
    return B(_5z/* LudoJS.$sfromList */(new T2(1,new T2(0,_1O/* LudoJS.Blue */,_8f/* sikj */),_cr/* sixM */)));
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
_cC/* $wa2 */ = function(_cD/* shLJ */, _/* EXTERNAL */){
  var _cE/* shLO */ = __get/* EXTERNAL */(_cD/* shLJ */, E(_1F/* LudoJS.$fFromAnyGameState16 */)),
  _cF/* shLS */ = String/* EXTERNAL */(_cE/* shLO */),
  _cG/* shLY */ = strEq/* EXTERNAL */(_cF/* shLS */, E(_cB/* LudoJS.lvl31 */));
  if(!E(_cG/* shLY */)){
    var _cH/* shMn */ = strEq/* EXTERNAL */(_cF/* shLS */, E(_cA/* LudoJS.lvl30 */));
    if(!E(_cH/* shMn */)){
      var _cI/* shMK */ = strEq/* EXTERNAL */(_cF/* shLS */, E(_cz/* LudoJS.lvl29 */));
      if(!E(_cI/* shMK */)){
        var _cJ/* shNm */ = strEq/* EXTERNAL */(_cF/* shLS */, E(_cy/* LudoJS.lvl28 */));
        return (E(_cJ/* shNm */)==0) ? E(_cx/* LudoJS.lvl27 */) : _cw/* LudoJS.GameFinished */;
      }else{
        var _cK/* shMR */ = __get/* EXTERNAL */(_cD/* shLJ */, E(_cu/* LudoJS.$fToAnyGameState11 */)),
        _cL/* shMX */ = __get/* EXTERNAL */(_cD/* shLJ */, E(_cv/* LudoJS.$fToAnyGameState6 */));
        return new T2(2,new T(function(){
          var _cM/* shN1 */ = Number/* EXTERNAL */(_cK/* shMR */);
          return jsTrunc/* EXTERNAL */(_cM/* shN1 */);
        }),new T(function(){
          var _cN/* shNa */ = Number/* EXTERNAL */(_cL/* shMX */);
          return jsTrunc/* EXTERNAL */(_cN/* shNa */);
        }));
      }
    }else{
      var _cO/* shMu */ = __get/* EXTERNAL */(_cD/* shLJ */, E(_cu/* LudoJS.$fToAnyGameState11 */));
      return new T1(1,new T(function(){
        var _cP/* shMy */ = Number/* EXTERNAL */(_cO/* shMu */);
        return jsTrunc/* EXTERNAL */(_cP/* shMy */);
      }));
    }
  }else{
    var _cQ/* shM5 */ = __get/* EXTERNAL */(_cD/* shLJ */, E(_cu/* LudoJS.$fToAnyGameState11 */));
    return new T1(0,new T(function(){
      var _cR/* shM9 */ = Number/* EXTERNAL */(_cQ/* shM5 */),
      _cS/* shMd */ = jsTrunc/* EXTERNAL */(_cR/* shM9 */),
      _cT/* shMg */ = E(_cS/* shMd */);
      if(_cT/* shMg */==( -1)){
        return __Z/* EXTERNAL */;
      }else{
        return new T1(1,_cT/* shMg */);
      }
    }));
  }
},
_cU/* $wa1 */ = function(_cV/* siyf */, _/* EXTERNAL */){
  var _cW/* siyk */ = __get/* EXTERNAL */(_cV/* siyf */, E(_1F/* LudoJS.$fFromAnyGameState16 */)),
  _cX/* siyn */ = B(_cC/* LudoJS.$wa2 */(_cW/* siyk */, _/* EXTERNAL */)),
  _cY/* siyt */ = __get/* EXTERNAL */(_cV/* siyf */, E(_1E/* LudoJS.$fFromAnyGameState15 */)),
  _cZ/* siyx */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_cY/* siyt */, _/* EXTERNAL */)),
  _d0/* siyD */ = __get/* EXTERNAL */(_cV/* siyf */, E(_ct/* LudoJS.$fFromAnyGameState8 */)),
  _d1/* siyJ */ = __get/* EXTERNAL */(_cV/* siyf */, E(_cs/* LudoJS.$fFromAnyGameState7 */)),
  _d2/* siyN */ = B(_6c/* LudoJS.$fFromAnyGameState6 */(_d1/* siyJ */, _/* EXTERNAL */)),
  _d3/* siyT */ = __get/* EXTERNAL */(_cV/* siyf */, E(_1L/* LudoJS.$fFromAnyGameState5 */)),
  _d4/* siyX */ = __arr2lst/* EXTERNAL */(0, _d3/* siyT */),
  _d5/* siz1 */ = B(_1G/* LudoJS.$fFromAnyGameState4 */(_d4/* siyX */, _/* EXTERNAL */));
  return new T5(0,_cX/* siyn */,_cZ/* siyx */,new T(function(){
    var _d6/* siz5 */ = Number/* EXTERNAL */(_d0/* siyD */);
    return jsTrunc/* EXTERNAL */(_d6/* siz5 */);
  }),_d2/* siyN */,_d5/* siz1 */);
},
_d7/* $fShowStage2 */ = 0,
_d8/* lvl25 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Map.!: given key is not an element in the map"));
}),
_d9/* lvl26 */ = new T(function(){
  return B(err/* EXTERNAL */(_d8/* LudoJS.lvl25 */));
}),
_da/* $s!1 */ = function(_db/* shHu */, _dc/* shHv */){
  while(1){
    var _dd/* shHw */ = E(_dc/* shHv */);
    if(!_dd/* shHw */._){
      var _de/* shHy */ = _dd/* shHw */.b,
      _df/* shHz */ = _dd/* shHw */.c,
      _dg/* shHA */ = _dd/* shHw */.d,
      _dh/* shHB */ = _dd/* shHw */.e;
      switch(E(_db/* shHu */)){
        case 0:
          switch(E(_de/* shHy */)){
            case 0:
              return E(_df/* shHz */);
            case 1:
              _db/* shHu */ = _1O/* LudoJS.Blue */;
              _dc/* shHv */ = _dg/* shHA */;
              continue;
            case 2:
              _db/* shHu */ = _1O/* LudoJS.Blue */;
              _dc/* shHv */ = _dg/* shHA */;
              continue;
            default:
              _db/* shHu */ = _1O/* LudoJS.Blue */;
              _dc/* shHv */ = _dg/* shHA */;
              continue;
          }
          break;
        case 1:
          switch(E(_de/* shHy */)){
            case 0:
              _db/* shHu */ = _1P/* LudoJS.Green */;
              _dc/* shHv */ = _dh/* shHB */;
              continue;
            case 1:
              return E(_df/* shHz */);
            case 2:
              _db/* shHu */ = _1P/* LudoJS.Green */;
              _dc/* shHv */ = _dg/* shHA */;
              continue;
            default:
              _db/* shHu */ = _1P/* LudoJS.Green */;
              _dc/* shHv */ = _dg/* shHA */;
              continue;
          }
          break;
        case 2:
          switch(E(_de/* shHy */)){
            case 2:
              return E(_df/* shHz */);
            case 3:
              _db/* shHu */ = _1Q/* LudoJS.Red */;
              _dc/* shHv */ = _dg/* shHA */;
              continue;
            default:
              _db/* shHu */ = _1Q/* LudoJS.Red */;
              _dc/* shHv */ = _dh/* shHB */;
              continue;
          }
          break;
        default:
          if(E(_de/* shHy */)==3){
            return E(_df/* shHz */);
          }else{
            _db/* shHu */ = _1S/* LudoJS.Yellow */;
            _dc/* shHv */ = _dh/* shHB */;
            continue;
          }
      }
    }else{
      return E(_d9/* LudoJS.lvl26 */);
    }
  }
},
_di/* numPiecesAt2 */ = 1,
_dj/* $wa6 */ = function(_dk/* si7Y */, _dl/* si7Z */, _dm/* si80 */, _/* EXTERNAL */){
  var _dn/* si82 */ = function(_/* EXTERNAL */){
    return new T(function(){
      var _do/* si8a */ = function(_dp/* si8b */){
        while(1){
          var _dq/* si8c */ = E(_dp/* si8b */);
          if(!_dq/* si8c */._){
            return 0;
          }else{
            var _dr/* si8e */ = _dq/* si8c */.b,
            _ds/* si8i */ = E(E(_dq/* si8c */.a).b);
            if(!_ds/* si8i */._){
              _dp/* si8b */ = _dr/* si8e */;
              continue;
            }else{
              if(_dm/* si80 */!=E(_ds/* si8i */.a)){
                _dp/* si8b */ = _dr/* si8e */;
                continue;
              }else{
                return B(_do/* si8a */(_dr/* si8e */))+1|0;
              }
            }
          }
        }
      };
      return B(_do/* si8a */(B(_da/* LudoJS.$s!1 */(_dl/* si7Z */, E(_dk/* si7Y */).d))));
    });
  };
  if(_dm/* si80 */>( -1)){
    return new F(function(){return _dn/* si82 */(_/* EXTERNAL */);});
  }else{
    if(_dm/* si80 */<( -4)){
      return new F(function(){return _dn/* si82 */(_/* EXTERNAL */);});
    }else{
      return new T(function(){
        var _dt/* si8D */ = function(_du/* si8E */){
          while(1){
            var _dv/* si8F */ = E(_du/* si8E */);
            if(!_dv/* si8F */._){
              return false;
            }else{
              var _dw/* si8H */ = _dv/* si8F */.b,
              _dx/* si8I */ = E(_dv/* si8F */.a);
              if(!E(_dx/* si8I */.b)._){
                if((_dm/* si80 */+5|0)!=E(_dx/* si8I */.a)){
                  _du/* si8E */ = _dw/* si8H */;
                  continue;
                }else{
                  return true;
                }
              }else{
                _du/* si8E */ = _dw/* si8H */;
                continue;
              }
            }
          }
        };
        if(!B(_dt/* si8D */(B(_da/* LudoJS.$s!1 */(_dl/* si7Z */, E(_dk/* si7Y */).d))))){
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
_kC/* $w$ctoAny1 */ = function(_kD/* sizH */){
  switch(E(_kD/* sizH */)){
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
_kE/* $fToAnyGameState_$ctoAny1 */ = function(_kF/* siA3 */){
  return new F(function(){return _kC/* LudoJS.$w$ctoAny1 */(_kF/* siA3 */);});
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
_l9/* $fToAnyGameState_$ctoAny2 */ = function(_la/* sia2 */){
  var _lb/* sia3 */ = E(_la/* sia2 */);
  switch(_lb/* sia3 */._){
    case 0:
      var _lc/* sia5 */ = E(_lb/* sia3 */.a);
      if(!_lc/* sia5 */._){
        return E(_l4/* LudoJS.$fToAnyGameState14 */);
      }else{
        return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,_kW/* LudoJS.$fToAnyGameState12 */,new T2(1,new T2(0,_cu/* LudoJS.$fToAnyGameState11 */,_lc/* sia5 */.a),_4/* GHC.Types.[] */)));});
      }
      break;
    case 1:
      return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,_l8/* LudoJS.$fToAnyGameState9 */,new T2(1,new T2(0,_cu/* LudoJS.$fToAnyGameState11 */,_lb/* sia3 */.a),_4/* GHC.Types.[] */)));});
      break;
    case 2:
      return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,_l6/* LudoJS.$fToAnyGameState7 */,new T2(1,new T2(0,_cu/* LudoJS.$fToAnyGameState11 */,_lb/* sia3 */.a),new T2(1,new T2(0,_cv/* LudoJS.$fToAnyGameState6 */,_lb/* sia3 */.b),_4/* GHC.Types.[] */))));});
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
_lk/* $fToAnyPiece_$ctoAny */ = function(_ll/* si9O */){
  var _lm/* si9P */ = E(_ll/* si9O */);
  if(!_lm/* si9P */._){
    return E(_lj/* LudoJS.$fToAnyPiece3 */);
  }else{
    return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,_le/* LudoJS.$fToAnyPiece1 */,new T2(1,new T2(0,_1M/* LudoJS.$fToAnyOption1 */,_lm/* si9P */.a),_4/* GHC.Types.[] */)));});
  }
},
_ln/* go1 */ = function(_lo/* siAc */){
  var _lp/* siAd */ = E(_lo/* siAc */);
  if(!_lp/* siAd */._){
    return __Z/* EXTERNAL */;
  }else{
    var _lq/* siAg */ = E(_lp/* siAd */.a);
    return new T2(1,new T2(0,new T(function(){
      return toJSStr/* EXTERNAL */(B(_5V/* GHC.Show.$wshowSignedInt */(0, E(_lq/* siAg */.a), _4/* GHC.Types.[] */)));
    }),new T(function(){
      return B(_lk/* LudoJS.$fToAnyPiece_$ctoAny */(_lq/* siAg */.b));
    })),new T(function(){
      return B(_ln/* LudoJS.go1 */(_lp/* siAd */.b));
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
_lv/* $fToAnyGameState_go10 */ = function(_lw/*  siAs */, _lx/*  siAt */){
  while(1){
    var _ly/*  $fToAnyGameState_go10 */ = B((function(_lz/* siAs */, _lA/* siAt */){
      var _lB/* siAu */ = E(_lA/* siAt */);
      if(!_lB/* siAu */._){
        var _lC/* siAQ */ = new T(function(){
          return B(_kK/* Haste.Prim.Any.$wtoObject */(new T(function(){
            return B(_ln/* LudoJS.go1 */(_lB/* siAu */.c));
          },1)));
        });
        _lw/*  siAs */ = new T2(1,new T2(0,new T(function(){
          switch(E(_lB/* siAu */.b)){
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
        }),_lC/* siAQ */),new T(function(){
          return B(_lv/* LudoJS.$fToAnyGameState_go10 */(_lz/* siAs */, _lB/* siAu */.e));
        }));
        _lx/*  siAt */ = _lB/* siAu */.d;
        return __continue/* EXTERNAL */;
      }else{
        return E(_lz/* siAs */);
      }
    })(_lw/*  siAs */, _lx/*  siAt */));
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
_lH/* $w$ctoAny */ = function(_lI/* siB4 */){
  var _lJ/* siBO */ = new T(function(){
    return B(_kK/* Haste.Prim.Any.$wtoObject */(new T(function(){
      return B(_lv/* LudoJS.$fToAnyGameState_go10 */(_4/* GHC.Types.[] */, E(_lI/* siB4 */).d));
    },1)));
  });
  return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,new T2(0,_1F/* LudoJS.$fFromAnyGameState16 */,new T(function(){
    return B(_l9/* LudoJS.$fToAnyGameState_$ctoAny2 */(E(_lI/* siB4 */).a));
  })),new T2(1,new T2(0,_1E/* LudoJS.$fFromAnyGameState15 */,new T(function(){
    switch(E(E(_lI/* siB4 */).b)){
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
    return E(E(_lI/* siB4 */).c);
  })),new T2(1,new T2(0,_cs/* LudoJS.$fFromAnyGameState7 */,_lJ/* siBO */),new T2(1,new T2(0,_1L/* LudoJS.$fFromAnyGameState5 */,new T(function(){
    return __lst2arr/* EXTERNAL */(B(_lD/* GHC.Base.map */(_kE/* LudoJS.$fToAnyGameState_$ctoAny1 */, E(_lI/* siB4 */).e)));
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
_mA/* $fEqOption_$c== */ = function(_mB/* sidg */, _mC/* sidh */){
  var _mD/* sidi */ = E(_mB/* sidg */);
  if(!_mD/* sidi */._){
    var _mE/* sidk */ = E(_mC/* sidh */);
    if(!_mE/* sidk */._){
      return new F(function(){return _mx/* GHC.Classes.eqInt */(_mD/* sidi */.a, _mE/* sidk */.a);});
    }else{
      return false;
    }
  }else{
    var _mF/* sidq */ = E(_mC/* sidh */);
    if(!_mF/* sidq */._){
      return false;
    }else{
      if(E(_mD/* sidi */.a)!=E(_mF/* sidq */.a)){
        return false;
      }else{
        return new F(function(){return _mx/* GHC.Classes.eqInt */(_mD/* sidi */.b, _mF/* sidq */.b);});
      }
    }
  }
},
_mG/* $fEqOption_$c/= */ = function(_mH/* sidA */, _mI/* sidB */){
  return (!B(_mA/* LudoJS.$fEqOption_$c== */(_mH/* sidA */, _mI/* sidB */))) ? true : false;
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
_mQ/* $sa */ = function(_mR/*  siIM */, _mS/*  siIN */, _mT/*  siIO */, _mU/*  siIP */, _mV/*  siIQ */, _mW/*  siIR */, _mX/*  siIS */, _/* EXTERNAL */){
  while(1){
    var _mY/*  $sa */ = B((function(_mZ/* siIM */, _n0/* siIN */, _n1/* siIO */, _n2/* siIP */, _n3/* siIQ */, _n4/* siIR */, _n5/* siIS */, _/* EXTERNAL */){
      var _n6/* siIU */ = E(_mZ/* siIM */);
      if(!_n6/* siIU */._){
        return new T2(0,_n0/* siIN */,new T5(0,_n1/* siIO */,_n2/* siIP */,_n3/* siIQ */,_n4/* siIR */,_n5/* siIS */));
      }else{
        var _n7/* siIX */ = _n6/* siIU */.a,
        _n8/*   siIO */ = _n1/* siIO */,
        _n9/*   siIP */ = _n2/* siIP */,
        _na/*   siIQ */ = _n3/* siIQ */,
        _nb/*   siIR */ = _n4/* siIR */,
        _nc/*   siIS */ = _n5/* siIS */;
        _mR/*  siIM */ = _n6/* siIU */.b;
        _mS/*  siIN */ = new T(function(){
          if(!B(_mL/* GHC.List.$wlenAcc */(B(_da/* LudoJS.$s!1 */(_n7/* siIX */, _n4/* siIR */)), 0))){
            return E(_n0/* siIN */);
          }else{
            return B(_q/* GHC.Base.++ */(_n0/* siIN */, new T2(1,_n7/* siIX */,_4/* GHC.Types.[] */)));
          }
        });
        _mT/*  siIO */ = _n8/*   siIO */;
        _mU/*  siIP */ = _n9/*   siIP */;
        _mV/*  siIQ */ = _na/*   siIQ */;
        _mW/*  siIR */ = _nb/*   siIR */;
        _mX/*  siIS */ = _nc/*   siIS */;
        return __continue/* EXTERNAL */;
      }
    })(_mR/*  siIM */, _mS/*  siIN */, _mT/*  siIO */, _mU/*  siIP */, _mV/*  siIQ */, _mW/*  siIR */, _mX/*  siIS */, _/* EXTERNAL */));
    if(_mY/*  $sa */!=__continue/* EXTERNAL */){
      return _mY/*  $sa */;
    }
  }
},
_nd/* $sa1 */ = function(_ne/* siJ3 */, _nf/* siJ4 */, _ng/* siJ5 */, _nh/* siJ6 */, _ni/* siJ7 */, _nj/* siJ8 */, _nk/* siJ9 */, _nl/* siJa */, _/* EXTERNAL */){
  return new F(function(){return _mQ/* LudoJS.$sa */(_nf/* siJ4 */, new T(function(){
    if(!B(_mL/* GHC.List.$wlenAcc */(B(_da/* LudoJS.$s!1 */(_ne/* siJ3 */, _nk/* siJ9 */)), 0))){
      return E(_ng/* siJ5 */);
    }else{
      return B(_q/* GHC.Base.++ */(_ng/* siJ5 */, new T2(1,_ne/* siJ3 */,_4/* GHC.Types.[] */)));
    }
  }), _nh/* siJ6 */, _ni/* siJ7 */, _nj/* siJ8 */, _nk/* siJ9 */, _nl/* siJa */, _/* EXTERNAL */);});
},
_nm/* go2 */ = function(_nn/* siDh */){
  while(1){
    var _no/* siDi */ = E(_nn/* siDh */);
    if(!_no/* siDi */._){
      return true;
    }else{
      if(!E(E(_no/* siDi */.a).b)._){
        _nn/* siDh */ = _no/* siDi */.b;
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
_nw/* $wa13 */ = function(_nx/* siDq */, _/* EXTERNAL */){
  var _ny/* siDL */ = new T(function(){
    var _nz/* siDs */ = E(_nx/* siDq */),
    _nA/* siDw */ = _nz/* siDs */.d,
    _nB/* siDy */ = new T(function(){
      switch(E(_nz/* siDs */.b)){
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
    return new T5(0,_mw/* LudoJS.play11 */,_nB/* siDy */,new T(function(){
      if(!B(_nm/* LudoJS.go2 */(B(_da/* LudoJS.$s!1 */(_nB/* siDy */, _nA/* siDw */))))){
        return E(_di/* LudoJS.numPiecesAt2 */);
      }else{
        return E(_63/* LudoJS.play10 */);
      }
    }),_nA/* siDw */,_nz/* siDs */.e);
  });
  return new T2(0,_eb/* GHC.Tuple.() */,_ny/* siDL */);
},
_nC/* $wa15 */ = function(_nD/* siEt */, _nE/* siEu */, _nF/* siEv */, _nG/* siEw */, _nH/* siEx */, _/* EXTERNAL */){
  var _nI/* siEB */ = new T5(0,_nD/* siEt */,_nE/* siEu */,_nF/* siEv */,_nG/* siEw */,_nH/* siEx */),
  _nJ/* siEC */ = function(_nK/* siED */){
    var _nL/* siEE */ = B(_nw/* LudoJS.$wa13 */(_nI/* siEB */, _/* EXTERNAL */)),
    _nM/* siEH */ = E(_nL/* siEE */),
    _nN/* siEK */ = E(_nM/* siEH */.b),
    _nO/* siEQ */ = B(_nC/* LudoJS.$wa15 */(_nN/* siEK */.a, _nN/* siEK */.b, _nN/* siEK */.c, _nN/* siEK */.d, _nN/* siEK */.e, _/* EXTERNAL */));
    return new T2(0,new T2(1,_nM/* siEH */.a,new T(function(){
      return E(E(_nO/* siEQ */).a);
    })),new T(function(){
      return E(E(_nO/* siEQ */).b);
    }));
  };
  if(!E(B(_mL/* GHC.List.$wlenAcc */(B(_da/* LudoJS.$s!1 */(_nE/* siEu */, _nG/* siEw */)), 0)))){
    return new F(function(){return _nJ/* siEC */(_/* EXTERNAL */);});
  }else{
    if(E(_nF/* siEv */)>=1){
      return new T2(0,_4/* GHC.Types.[] */,_nI/* siEB */);
    }else{
      return new F(function(){return _nJ/* siEC */(_/* EXTERNAL */);});
    }
  }
},
_nP/* $fToAnyOption3 */ = "Move",
_nQ/* $fToAnyOption4 */ = "option",
_nR/* $fToAnyOption2 */ = new T2(0,_nQ/* LudoJS.$fToAnyOption4 */,_nP/* LudoJS.$fToAnyOption3 */),
_nS/* $fToAnyOption7 */ = "Play",
_nT/* $fToAnyOption6 */ = new T2(0,_nQ/* LudoJS.$fToAnyOption4 */,_nS/* LudoJS.$fToAnyOption7 */),
_nU/* $w$ctoAny2 */ = function(_nV/* siav */){
  var _nW/* siaw */ = E(_nV/* siav */);
  if(!_nW/* siaw */._){
    return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,_nT/* LudoJS.$fToAnyOption6 */,new T2(1,new T2(0,_1N/* LudoJS.$fToAnyOption5 */,_nW/* siaw */.a),_4/* GHC.Types.[] */)));});
  }else{
    return new F(function(){return _kK/* Haste.Prim.Any.$wtoObject */(new T2(1,_nR/* LudoJS.$fToAnyOption2 */,new T2(1,new T2(0,_1N/* LudoJS.$fToAnyOption5 */,_nW/* siaw */.a),new T2(1,new T2(0,_1M/* LudoJS.$fToAnyOption1 */,_nW/* siaw */.b),_4/* GHC.Types.[] */))));});
  }
},
_nX/* $fToAnyOption_$ctoAny */ = function(_nY/* siaI */){
  return new F(function(){return _nU/* LudoJS.$w$ctoAny2 */(_nY/* siaI */);});
},
_nZ/* a42 */ = function(_o0/* shO8 */, _o1/* shO9 */, _/* EXTERNAL */){
  var _o2/* shQl */ = new T(function(){
    var _o3/* shOb */ = E(_o1/* shO9 */),
    _o4/* shOh */ = function(_o5/* shOi */){
      var _o6/* shOj */ = E(_o5/* shOi */);
      if(!_o6/* shOj */._){
        return __Z/* EXTERNAL */;
      }else{
        var _o7/* shOl */ = _o6/* shOj */.b,
        _o8/* shOm */ = E(_o6/* shOj */.a),
        _o9/* shOn */ = _o8/* shOm */.a,
        _oa/* shOp */ = E(_o8/* shOm */.b);
        if(!_oa/* shOp */._){
          var _ob/* shOs */ = E(_o0/* shO8 */);
          if(_ob/* shOs */==6){
            var _oc/* shPe */ = new T(function(){
              var _od/* shOQ */ = function(_oe/*  shOR */){
                while(1){
                  var _of/*  shOQ */ = B((function(_og/* shOR */){
                    var _oh/* shOS */ = E(_og/* shOR */);
                    if(!_oh/* shOS */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _oi/* shOU */ = _oh/* shOS */.b,
                      _oj/* shOV */ = E(_oh/* shOS */.a),
                      _ok/* shOW */ = _oj/* shOV */.a,
                      _ol/* shOY */ = E(_oj/* shOV */.b);
                      if(!_ol/* shOY */._){
                        return new T2(1,new T1(0,_ok/* shOW */),new T(function(){
                          return B(_od/* shOQ */(_oi/* shOU */));
                        }));
                      }else{
                        var _om/* shP2 */ = E(_ol/* shOY */.a);
                        if(_om/* shP2 */>=51){
                          if((6+_om/* shP2 */|0)==56){
                            return new T2(1,new T2(1,_ok/* shOW */,56),new T(function(){
                              return B(_od/* shOQ */(_oi/* shOU */));
                            }));
                          }else{
                            _oe/*  shOR */ = _oi/* shOU */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          return new T2(1,new T2(1,_ok/* shOW */,6+_om/* shP2 */|0),new T(function(){
                            return B(_od/* shOQ */(_oi/* shOU */));
                          }));
                        }
                      }
                    }
                  })(_oe/*  shOR */));
                  if(_of/*  shOQ */!=__continue/* EXTERNAL */){
                    return _of/*  shOQ */;
                  }
                }
              };
              return B(_od/* shOQ */(_o7/* shOl */));
            });
            return new T2(1,new T1(0,_o9/* shOn */),_oc/* shPe */);
          }else{
            var _on/* shOt */ = function(_oo/*  shOu */){
              while(1){
                var _op/*  shOt */ = B((function(_oq/* shOu */){
                  var _or/* shOv */ = E(_oq/* shOu */);
                  if(!_or/* shOv */._){
                    return __Z/* EXTERNAL */;
                  }else{
                    var _os/* shOx */ = _or/* shOv */.b,
                    _ot/* shOy */ = E(_or/* shOv */.a),
                    _ou/* shOz */ = _ot/* shOy */.a,
                    _ov/* shOB */ = E(_ot/* shOy */.b);
                    if(!_ov/* shOB */._){
                      _oo/*  shOu */ = _os/* shOx */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _ow/* shOD */ = E(_ov/* shOB */.a);
                      if(_ow/* shOD */>=51){
                        if((_ob/* shOs */+_ow/* shOD */|0)==56){
                          return new T2(1,new T2(1,_ou/* shOz */,56),new T(function(){
                            return B(_on/* shOt */(_os/* shOx */));
                          }));
                        }else{
                          _oo/*  shOu */ = _os/* shOx */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        return new T2(1,new T2(1,_ou/* shOz */,_ob/* shOs */+_ow/* shOD */|0),new T(function(){
                          return B(_on/* shOt */(_os/* shOx */));
                        }));
                      }
                    }
                  }
                })(_oo/*  shOu */));
                if(_op/*  shOt */!=__continue/* EXTERNAL */){
                  return _op/*  shOt */;
                }
              }
            };
            return new F(function(){return _on/* shOt */(_o7/* shOl */);});
          }
        }else{
          var _ox/* shPg */ = E(_oa/* shOp */.a);
          if(_ox/* shPg */>=51){
            var _oy/* shPk */ = E(_o0/* shO8 */);
            if((_oy/* shPk */+_ox/* shPg */|0)==56){
              var _oz/* shQd */ = new T(function(){
                var _oA/* shPO */ = function(_oB/*  shPP */){
                  while(1){
                    var _oC/*  shPO */ = B((function(_oD/* shPP */){
                      var _oE/* shPQ */ = E(_oD/* shPP */);
                      if(!_oE/* shPQ */._){
                        return __Z/* EXTERNAL */;
                      }else{
                        var _oF/* shPS */ = _oE/* shPQ */.b,
                        _oG/* shPT */ = E(_oE/* shPQ */.a),
                        _oH/* shPU */ = _oG/* shPT */.a,
                        _oI/* shPW */ = E(_oG/* shPT */.b);
                        if(!_oI/* shPW */._){
                          if(E(_oy/* shPk */)==6){
                            return new T2(1,new T1(0,_oH/* shPU */),new T(function(){
                              return B(_oA/* shPO */(_oF/* shPS */));
                            }));
                          }else{
                            _oB/*  shPP */ = _oF/* shPS */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          var _oJ/* shQ1 */ = E(_oI/* shPW */.a);
                          if(_oJ/* shQ1 */>=51){
                            if((_oy/* shPk */+_oJ/* shQ1 */|0)==56){
                              return new T2(1,new T2(1,_oH/* shPU */,56),new T(function(){
                                return B(_oA/* shPO */(_oF/* shPS */));
                              }));
                            }else{
                              _oB/*  shPP */ = _oF/* shPS */;
                              return __continue/* EXTERNAL */;
                            }
                          }else{
                            return new T2(1,new T2(1,_oH/* shPU */,_oy/* shPk */+_oJ/* shQ1 */|0),new T(function(){
                              return B(_oA/* shPO */(_oF/* shPS */));
                            }));
                          }
                        }
                      }
                    })(_oB/*  shPP */));
                    if(_oC/*  shPO */!=__continue/* EXTERNAL */){
                      return _oC/*  shPO */;
                    }
                  }
                };
                return B(_oA/* shPO */(_o7/* shOl */));
              });
              return new T2(1,new T2(1,_o9/* shOn */,56),_oz/* shQd */);
            }else{
              var _oK/* shPn */ = function(_oL/*  shPo */){
                while(1){
                  var _oM/*  shPn */ = B((function(_oN/* shPo */){
                    var _oO/* shPp */ = E(_oN/* shPo */);
                    if(!_oO/* shPp */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _oP/* shPr */ = _oO/* shPp */.b,
                      _oQ/* shPs */ = E(_oO/* shPp */.a),
                      _oR/* shPt */ = _oQ/* shPs */.a,
                      _oS/* shPv */ = E(_oQ/* shPs */.b);
                      if(!_oS/* shPv */._){
                        if(E(_oy/* shPk */)==6){
                          return new T2(1,new T1(0,_oR/* shPt */),new T(function(){
                            return B(_oK/* shPn */(_oP/* shPr */));
                          }));
                        }else{
                          _oL/*  shPo */ = _oP/* shPr */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        var _oT/* shPA */ = E(_oS/* shPv */.a);
                        if(_oT/* shPA */>=51){
                          if((_oy/* shPk */+_oT/* shPA */|0)==56){
                            return new T2(1,new T2(1,_oR/* shPt */,56),new T(function(){
                              return B(_oK/* shPn */(_oP/* shPr */));
                            }));
                          }else{
                            _oL/*  shPo */ = _oP/* shPr */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          return new T2(1,new T2(1,_oR/* shPt */,_oy/* shPk */+_oT/* shPA */|0),new T(function(){
                            return B(_oK/* shPn */(_oP/* shPr */));
                          }));
                        }
                      }
                    }
                  })(_oL/*  shPo */));
                  if(_oM/*  shPn */!=__continue/* EXTERNAL */){
                    return _oM/*  shPn */;
                  }
                }
              };
              return new F(function(){return _oK/* shPn */(_o7/* shOl */);});
            }
          }else{
            return new T2(1,new T2(1,_o9/* shOn */,new T(function(){
              return E(_o0/* shO8 */)+_ox/* shPg */|0;
            })),new T(function(){
              return B(_o4/* shOh */(_o7/* shOl */));
            }));
          }
        }
      }
    };
    return B(_o4/* shOh */(B(_da/* LudoJS.$s!1 */(_o3/* shOb */.b, _o3/* shOb */.d))));
  });
  return new T2(0,_o2/* shQl */,_o1/* shO9 */);
},
_oU/* play3 */ = "((gs, opts, rolls) => drawBoard(gs, opts, rolls))",
_oV/* f2 */ = new T(function(){
  return eval/* EXTERNAL */(E(_oU/* LudoJS.play3 */));
}),
_oW/* lvl13 */ = new T(function(){
  return B(_1o/* Control.Exception.Base.patError */("LudoJS.hs:(312,5)-(316,32)|case"));
}),
_oX/* $wa16 */ = function(_oY/* siHQ */, _oZ/* siHR */, _p0/* siHS */, _p1/* siHT */, _p2/* siHU */, _p3/* siHV */, _/* EXTERNAL */){
  var _p4/* siHX */ = function(_p5/* siHY */, _p6/* siHZ */){
    var _p7/* siI2 */ = function(_/* EXTERNAL */, _p8/* siI4 */, _p9/* siI5 */){
      var _pa/* siIb */ = __lst2arr/* EXTERNAL */(B(_lD/* GHC.Base.map */(_nX/* LudoJS.$fToAnyOption_$ctoAny */, _p8/* siI4 */))),
      _pb/* siIh */ = __app3/* EXTERNAL */(E(_oV/* LudoJS.f2 */), B(_lH/* LudoJS.$w$ctoAny */(new T5(0,_oY/* siHQ */,_oZ/* siHR */,_p0/* siHS */,_p1/* siHT */,_p2/* siHU */))), _pa/* siIb */, _p5/* siHY */);
      return new T2(0,_eb/* GHC.Tuple.() */,_p9/* siI5 */);
    };
    if(E(_p5/* siHY */)==( -1)){
      return new F(function(){return _p7/* siI2 */(_/* EXTERNAL */, _4/* GHC.Types.[] */, _p3/* siHV */);});
    }else{
      var _pc/* siIm */ = B(_nZ/* LudoJS.a42 */(_p6/* siHZ */, _p3/* siHV */, _/* EXTERNAL */)),
      _pd/* siIp */ = E(_pc/* siIm */);
      return new F(function(){return _p7/* siI2 */(_/* EXTERNAL */, _pd/* siIp */.a, _pd/* siIp */.b);});
    }
  },
  _pe/* siIs */ = E(_oY/* siHQ */);
  switch(_pe/* siIs */._){
    case 0:
      var _pf/* siIu */ = E(_pe/* siIs */.a);
      if(!_pf/* siIu */._){
        return new F(function(){return _p4/* siHX */( -1, _kX/* LudoJS.$fToAnyGameState18 */);});
      }else{
        var _pg/* siIw */ = E(_pf/* siIu */.a);
        return new F(function(){return _p4/* siHX */(_pg/* siIw */, _pg/* siIw */);});
      }
      break;
    case 1:
      var _ph/* siIz */ = E(_pe/* siIs */.a);
      return new F(function(){return _p4/* siHX */(_ph/* siIz */, _ph/* siIz */);});
      break;
    case 2:
      var _pi/* siID */ = E(_pe/* siIs */.a);
      return new F(function(){return _p4/* siHX */(_pi/* siID */, _pi/* siID */);});
      break;
    default:
      return E(_oW/* LudoJS.lvl13 */);
  }
},
_pj/* neInt */ = function(_pk/* scEM */, _pl/* scEN */){
  return E(_pk/* scEM */)!=E(_pl/* scEN */);
},
_pm/* $fEqInt */ = new T2(0,_mx/* GHC.Classes.eqInt */,_pj/* GHC.Classes.neInt */),
_pn/* $soutByCell */ = function(_po/* shQE */, _pp/* shQF */){
  var _pq/* shQG */ = E(_po/* shQE */);
  if(!_pq/* shQG */._){
    return __Z/* EXTERNAL */;
  }else{
    var _pr/* shQI */ = _pq/* shQG */.b,
    _ps/* shQJ */ = E(_pq/* shQG */.a),
    _pt/* shQM */ = E(_ps/* shQJ */.b);
    return (_pt/* shQM */._==0) ? new T2(1,_ps/* shQJ */,new T(function(){
      return B(_pn/* LudoJS.$soutByCell */(_pr/* shQI */, _pp/* shQF */));
    })) : (_pp/* shQF */!=E(_pt/* shQM */.a)) ? new T2(1,_ps/* shQJ */,new T(function(){
      return B(_pn/* LudoJS.$soutByCell */(_pr/* shQI */, _pp/* shQF */));
    })) : new T2(1,new T2(0,_ps/* shQJ */.a,_60/* LudoJS.Out */),new T(function(){
      return B(_pn/* LudoJS.$soutByCell */(_pr/* shQI */, _pp/* shQF */));
    }));
  }
},
_pu/* $sremoveFrom */ = function(_pv/* shRg */, _pw/* shRh */){
  var _px/* shRi */ = E(_pv/* shRg */);
  if(!_px/* shRi */._){
    return __Z/* EXTERNAL */;
  }else{
    var _py/* shRk */ = _px/* shRi */.b,
    _pz/* shRl */ = E(_px/* shRi */.a);
    return (_pw/* shRh */!=E(_pz/* shRl */.a)) ? new T2(1,_pz/* shRl */,new T(function(){
      return B(_pu/* LudoJS.$sremoveFrom */(_py/* shRk */, _pw/* shRh */));
    })) : E(_py/* shRk */);
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
_pP/* $wconvertCell */ = function(_pQ/* shS2 */, _pR/* shS3 */, _pS/* shS4 */){
  switch(E(_pS/* shS4 */)){
    case 0:
      switch(E(_pQ/* shS2 */)){
        case 0:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
      break;
    case 1:
      switch(E(_pQ/* shS2 */)){
        case 0:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
      break;
    case 2:
      switch(E(_pQ/* shS2 */)){
        case 0:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
      break;
    default:
      switch(E(_pQ/* shS2 */)){
        case 0:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _np/* GHC.Classes.modInt# */(_pR/* shS3 */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
  }
},
_pT/* $s!_$spoly_go1 */ = function(_pU/* shHe */){
  while(1){
    var _pV/* shHf */ = E(_pU/* shHe */);
    if(!_pV/* shHf */._){
      var _pW/* shHj */ = _pV/* shHf */.d;
      switch(E(_pV/* shHf */.b)){
        case 0:
          _pU/* shHe */ = _pV/* shHf */.e;
          continue;
        case 1:
          return E(_pV/* shHf */.c);
        case 2:
          _pU/* shHe */ = _pW/* shHj */;
          continue;
        default:
          _pU/* shHe */ = _pW/* shHj */;
          continue;
      }
    }else{
      return E(_d9/* LudoJS.lvl26 */);
    }
  }
},
_pX/* $s!_$spoly_go10 */ = function(_pY/* shH6 */){
  while(1){
    var _pZ/* shH7 */ = E(_pY/* shH6 */);
    if(!_pZ/* shH7 */._){
      if(E(_pZ/* shH7 */.b)==3){
        return E(_pZ/* shH7 */.c);
      }else{
        _pY/* shH6 */ = _pZ/* shH7 */.e;
        continue;
      }
    }else{
      return E(_d9/* LudoJS.lvl26 */);
    }
  }
},
_q0/* $s!_$spoly_go2 */ = function(_q1/* shHm */){
  while(1){
    var _q2/* shHn */ = E(_q1/* shHm */);
    if(!_q2/* shHn */._){
      var _q3/* shHr */ = _q2/* shHn */.d;
      switch(E(_q2/* shHn */.b)){
        case 0:
          return E(_q2/* shHn */.c);
        case 1:
          _q1/* shHm */ = _q3/* shHr */;
          continue;
        case 2:
          _q1/* shHm */ = _q3/* shHr */;
          continue;
        default:
          _q1/* shHm */ = _q3/* shHr */;
          continue;
      }
    }else{
      return E(_d9/* LudoJS.lvl26 */);
    }
  }
},
_q4/* $sinsert_$s$sgo1 */ = function(_q5/* shJi */, _q6/* shJj */){
  var _q7/* shJk */ = E(_q6/* shJj */);
  if(!_q7/* shJk */._){
    var _q8/* shJn */ = _q7/* shJk */.c,
    _q9/* shJo */ = _q7/* shJk */.d,
    _qa/* shJp */ = _q7/* shJk */.e;
    switch(E(_q7/* shJk */.b)){
      case 0:
        return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1O/* LudoJS.Blue */, _q8/* shJn */, _q9/* shJo */, B(_q4/* LudoJS.$sinsert_$s$sgo1 */(_q5/* shJi */, _qa/* shJp */)));});
        break;
      case 1:
        return new T5(0,_q7/* shJk */.a,E(_1P/* LudoJS.Green */),_q5/* shJi */,E(_q9/* shJo */),E(_qa/* shJp */));
      case 2:
        return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1Q/* LudoJS.Red */, _q8/* shJn */, B(_q4/* LudoJS.$sinsert_$s$sgo1 */(_q5/* shJi */, _q9/* shJo */)), _qa/* shJp */);});
        break;
      default:
        return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1S/* LudoJS.Yellow */, _q8/* shJn */, B(_q4/* LudoJS.$sinsert_$s$sgo1 */(_q5/* shJi */, _q9/* shJo */)), _qa/* shJp */);});
    }
  }else{
    return new T5(0,1,E(_1P/* LudoJS.Green */),_q5/* shJi */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
  }
},
_qb/* $sinsert_$s$sgo10 */ = function(_qc/* shJ6 */, _qd/* shJ7 */){
  var _qe/* shJ8 */ = E(_qd/* shJ7 */);
  if(!_qe/* shJ8 */._){
    var _qf/* shJb */ = _qe/* shJ8 */.c,
    _qg/* shJc */ = _qe/* shJ8 */.d,
    _qh/* shJd */ = _qe/* shJ8 */.e;
    switch(E(_qe/* shJ8 */.b)){
      case 0:
        return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1O/* LudoJS.Blue */, _qf/* shJb */, _qg/* shJc */, B(_qb/* LudoJS.$sinsert_$s$sgo10 */(_qc/* shJ6 */, _qh/* shJd */)));});
        break;
      case 1:
        return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1P/* LudoJS.Green */, _qf/* shJb */, _qg/* shJc */, B(_qb/* LudoJS.$sinsert_$s$sgo10 */(_qc/* shJ6 */, _qh/* shJd */)));});
        break;
      case 2:
        return new F(function(){return _2E/* Data.Map.Base.balanceR */(_1Q/* LudoJS.Red */, _qf/* shJb */, _qg/* shJc */, B(_qb/* LudoJS.$sinsert_$s$sgo10 */(_qc/* shJ6 */, _qh/* shJd */)));});
        break;
      default:
        return new T5(0,_qe/* shJ8 */.a,E(_1S/* LudoJS.Yellow */),_qc/* shJ6 */,E(_qg/* shJc */),E(_qh/* shJd */));
    }
  }else{
    return new T5(0,1,E(_1S/* LudoJS.Yellow */),_qc/* shJ6 */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
  }
},
_qi/* $sinsert_$s$sgo2 */ = function(_qj/* shJu */, _qk/* shJv */){
  var _ql/* shJw */ = E(_qk/* shJv */);
  if(!_ql/* shJw */._){
    var _qm/* shJz */ = _ql/* shJw */.c,
    _qn/* shJA */ = _ql/* shJw */.d,
    _qo/* shJB */ = _ql/* shJw */.e;
    switch(E(_ql/* shJw */.b)){
      case 0:
        return new T5(0,_ql/* shJw */.a,E(_1O/* LudoJS.Blue */),_qj/* shJu */,E(_qn/* shJA */),E(_qo/* shJB */));
      case 1:
        return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1P/* LudoJS.Green */, _qm/* shJz */, B(_qi/* LudoJS.$sinsert_$s$sgo2 */(_qj/* shJu */, _qn/* shJA */)), _qo/* shJB */);});
        break;
      case 2:
        return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1Q/* LudoJS.Red */, _qm/* shJz */, B(_qi/* LudoJS.$sinsert_$s$sgo2 */(_qj/* shJu */, _qn/* shJA */)), _qo/* shJB */);});
        break;
      default:
        return new F(function(){return _1X/* Data.Map.Base.balanceL */(_1S/* LudoJS.Yellow */, _qm/* shJz */, B(_qi/* LudoJS.$sinsert_$s$sgo2 */(_qj/* shJu */, _qn/* shJA */)), _qo/* shJB */);});
    }
  }else{
    return new T5(0,1,E(_1O/* LudoJS.Blue */),_qj/* shJu */,E(_1R/* Data.Map.Base.Tip */),E(_1R/* Data.Map.Base.Tip */));
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
_qB/* outByCell */ = function(_qC/* shQW */, _qD/* shQX */){
  var _qE/* shQY */ = E(_qC/* shQW */);
  if(!_qE/* shQY */._){
    return __Z/* EXTERNAL */;
  }else{
    var _qF/* shR0 */ = _qE/* shQY */.b,
    _qG/* shR1 */ = E(_qE/* shQY */.a),
    _qH/* shR4 */ = E(_qG/* shR1 */.b);
    if(!_qH/* shR4 */._){
      return new T2(1,_qG/* shR1 */,new T(function(){
        return B(_qB/* LudoJS.outByCell */(_qF/* shR0 */, _qD/* shQX */));
      }));
    }else{
      var _qI/* shR7 */ = E(_qD/* shQX */);
      return (_qI/* shR7 */!=E(_qH/* shR4 */.a)) ? new T2(1,_qG/* shR1 */,new T(function(){
        return B(_pn/* LudoJS.$soutByCell */(_qF/* shR0 */, _qI/* shR7 */));
      })) : new T2(1,new T2(0,_qG/* shR1 */.a,_60/* LudoJS.Out */),new T(function(){
        return B(_pn/* LudoJS.$soutByCell */(_qF/* shR0 */, _qI/* shR7 */));
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
_qS/* a43 */ = function(_qT/* shTy */, _qU/* shTz */, _qV/* shTA */, _/* EXTERNAL */){
  var _qW/* shTC */ = function(_qX/* shTD */){
    var _qY/* shTE */ = E(_qX/* shTD */);
    if(!_qY/* shTE */._){
      return E(_qz/* LudoJS.lvl11 */);
    }else{
      var _qZ/* shTG */ = _qY/* shTE */.b,
      _r0/* shTH */ = E(_qU/* shTz */);
      if(_r0/* shTH */!=E(_qY/* shTE */.a)){
        var _r1/* shTN */ = function(_r2/* shTO */){
          while(1){
            var _r3/* shTP */ = E(_r2/* shTO */);
            if(!_r3/* shTP */._){
              return E(_qz/* LudoJS.lvl11 */);
            }else{
              var _r4/* shTR */ = _r3/* shTP */.b;
              if(_r0/* shTH */!=E(_r3/* shTP */.a)){
                _r2/* shTO */ = _r4/* shTR */;
                continue;
              }else{
                return new F(function(){return _qJ/* GHC.Integer.Type.plusInteger */(B(_r1/* shTN */(_r4/* shTR */)), _qA/* LudoJS.lvl12 */);});
              }
            }
          }
        };
        return new F(function(){return _r1/* shTN */(_qZ/* shTG */);});
      }else{
        var _r5/* shTX */ = function(_r6/* shTY */){
          while(1){
            var _r7/* shTZ */ = E(_r6/* shTY */);
            if(!_r7/* shTZ */._){
              return E(_qz/* LudoJS.lvl11 */);
            }else{
              var _r8/* shU1 */ = _r7/* shTZ */.b;
              if(_r0/* shTH */!=E(_r7/* shTZ */.a)){
                _r6/* shTY */ = _r8/* shU1 */;
                continue;
              }else{
                return new F(function(){return _qJ/* GHC.Integer.Type.plusInteger */(B(_r5/* shTX */(_r8/* shU1 */)), _qA/* LudoJS.lvl12 */);});
              }
            }
          }
        };
        return new F(function(){return _qJ/* GHC.Integer.Type.plusInteger */(B(_r5/* shTX */(_qZ/* shTG */)), _qA/* LudoJS.lvl12 */);});
      }
    }
  },
  _r9/* shU8 */ = function(_ra/* shU9 */, _/* EXTERNAL */){
    var _rb/* shUz */ = new T(function(){
      var _rc/* shUh */ = function(_rd/*  shUi */){
        while(1){
          var _re/*  shUh */ = B((function(_rf/* shUi */){
            var _rg/* shUj */ = E(_rf/* shUi */);
            if(!_rg/* shUj */._){
              return __Z/* EXTERNAL */;
            }else{
              var _rh/* shUl */ = _rg/* shUj */.b,
              _ri/* shUp */ = E(E(_rg/* shUj */.a).b);
              if(!_ri/* shUp */._){
                _rd/*  shUi */ = _rh/* shUl */;
                return __continue/* EXTERNAL */;
              }else{
                return new T2(1,new T(function(){
                  return B(_pP/* LudoJS.$wconvertCell */(_1O/* LudoJS.Blue */, E(_ri/* shUp */.a), _qT/* shTy */));
                }),new T(function(){
                  return B(_rc/* shUh */(_rh/* shUl */));
                }));
              }
            }
          })(_rd/*  shUi */));
          if(_re/*  shUh */!=__continue/* EXTERNAL */){
            return _re/*  shUh */;
          }
        }
      };
      return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shTC */(B(_rc/* shUh */(B(_q0/* LudoJS.$s!_$spoly_go2 */(E(_ra/* shU9 */).d)))))), _qy/* LudoJS.lvl10 */));
    });
    return new T2(0,_rb/* shUz */,_ra/* shU9 */);
  },
  _rj/* shUB */ = function(_/* EXTERNAL */, _rk/* shUD */){
    var _rl/* shUE */ = E(_rk/* shUD */),
    _rm/* shUG */ = _rl/* shUE */.b;
    if(!E(_rl/* shUE */.a)){
      var _rn/* shUI */ = function(_/* EXTERNAL */, _ro/* shUK */, _rp/* shUL */){
        var _rq/* shUM */ = function(_/* EXTERNAL */, _rr/* shUO */, _rs/* shUP */){
          var _rt/* shUQ */ = function(_/* EXTERNAL */, _ru/* shUS */){
            var _rv/* shUT */ = E(_qT/* shTy */);
            if(_rv/* shUT */==2){
              return new T2(0,_eb/* GHC.Tuple.() */,new T(function(){
                return E(E(_ru/* shUS */).b);
              }));
            }else{
              var _rw/* shVs */ = new T(function(){
                var _rx/* shUX */ = E(E(_ru/* shUS */).b),
                _ry/* shV1 */ = _rx/* shUX */.d,
                _rz/* shVr */ = new T(function(){
                  var _rA/* shVq */ = new T(function(){
                    return B(_qB/* LudoJS.outByCell */(B(_da/* LudoJS.$s!1 */(_1Q/* LudoJS.Red */, _ry/* shV1 */)), new T(function(){
                      var _rB/* shV4 */ = E(_qU/* shTz */);
                      switch(E(_rv/* shUT */)){
                        case 0:
                          return B(_np/* GHC.Classes.modInt# */(_rB/* shV4 */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52));
                          break;
                        case 1:
                          return B(_np/* GHC.Classes.modInt# */(_rB/* shV4 */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52));
                          break;
                        default:
                          return B(_np/* GHC.Classes.modInt# */(_rB/* shV4 */+(imul/* EXTERNAL */(13, 4-(4-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52));
                      }
                    },1)));
                  });
                  return B(_3j/* LudoJS.$sinsert_$sgo10 */(_1Q/* LudoJS.Red */, _rA/* shVq */, _ry/* shV1 */));
                });
                return new T5(0,_rx/* shUX */.a,_rx/* shUX */.b,_rx/* shUX */.c,_rz/* shVr */,_rx/* shUX */.e);
              });
              return new T2(0,_eb/* GHC.Tuple.() */,_rw/* shVs */);
            }
          },
          _rC/* shVz */ = E(_qT/* shTy */);
          if(_rC/* shVz */==3){
            return new F(function(){return _rt/* shUQ */(_/* EXTERNAL */, new T2(0,_eb/* GHC.Tuple.() */,_rs/* shUP */));});
          }else{
            var _rD/* shW5 */ = new T(function(){
              var _rE/* shVA */ = E(_rs/* shUP */),
              _rF/* shVE */ = _rE/* shVA */.d,
              _rG/* shW4 */ = new T(function(){
                var _rH/* shW3 */ = new T(function(){
                  return B(_qB/* LudoJS.outByCell */(B(_pX/* LudoJS.$s!_$spoly_go10 */(_rF/* shVE */)), new T(function(){
                    var _rI/* shVH */ = E(_qU/* shTz */);
                    switch(E(_rC/* shVz */)){
                      case 0:
                        return B(_np/* GHC.Classes.modInt# */(_rI/* shVH */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52));
                        break;
                      case 1:
                        return B(_np/* GHC.Classes.modInt# */(_rI/* shVH */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52));
                        break;
                      default:
                        return B(_np/* GHC.Classes.modInt# */(_rI/* shVH */+(imul/* EXTERNAL */(13, 4-(3-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52));
                    }
                  },1)));
                });
                return B(_qb/* LudoJS.$sinsert_$s$sgo10 */(_rH/* shW3 */, _rF/* shVE */));
              });
              return new T5(0,_rE/* shVA */.a,_rE/* shVA */.b,_rE/* shVA */.c,_rG/* shW4 */,_rE/* shVA */.e);
            });
            return new F(function(){return _rt/* shUQ */(_/* EXTERNAL */, new T2(0,_eb/* GHC.Tuple.() */,_rD/* shW5 */));});
          }
        },
        _rJ/* shW8 */ = E(_qT/* shTy */);
        if(_rJ/* shW8 */==1){
          return new F(function(){return _rq/* shUM */(_/* EXTERNAL */, _eb/* GHC.Tuple.() */, _rp/* shUL */);});
        }else{
          var _rK/* shWE */ = new T(function(){
            var _rL/* shW9 */ = E(_rp/* shUL */),
            _rM/* shWd */ = _rL/* shW9 */.d,
            _rN/* shWD */ = new T(function(){
              var _rO/* shWC */ = new T(function(){
                return B(_qB/* LudoJS.outByCell */(B(_pT/* LudoJS.$s!_$spoly_go1 */(_rM/* shWd */)), new T(function(){
                  var _rP/* shWg */ = E(_qU/* shTz */);
                  switch(E(_rJ/* shW8 */)){
                    case 0:
                      return B(_np/* GHC.Classes.modInt# */(_rP/* shWg */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52));
                      break;
                    case 2:
                      return B(_np/* GHC.Classes.modInt# */(_rP/* shWg */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52));
                      break;
                    default:
                      return B(_np/* GHC.Classes.modInt# */(_rP/* shWg */+(imul/* EXTERNAL */(13, 4-(2-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52));
                  }
                },1)));
              });
              return B(_q4/* LudoJS.$sinsert_$s$sgo1 */(_rO/* shWC */, _rM/* shWd */));
            });
            return new T5(0,_rL/* shW9 */.a,_rL/* shW9 */.b,_rL/* shW9 */.c,_rN/* shWD */,_rL/* shW9 */.e);
          },1);
          return new F(function(){return _rq/* shUM */(_/* EXTERNAL */, _eb/* GHC.Tuple.() */, _rK/* shWE */);});
        }
      },
      _rQ/* shWF */ = E(_qT/* shTy */);
      if(!_rQ/* shWF */){
        return new F(function(){return _rn/* shUI */(_/* EXTERNAL */, _eb/* GHC.Tuple.() */, _rm/* shUG */);});
      }else{
        var _rR/* shXb */ = new T(function(){
          var _rS/* shWG */ = E(_rm/* shUG */),
          _rT/* shWK */ = _rS/* shWG */.d,
          _rU/* shXa */ = new T(function(){
            var _rV/* shX9 */ = new T(function(){
              return B(_qB/* LudoJS.outByCell */(B(_q0/* LudoJS.$s!_$spoly_go2 */(_rT/* shWK */)), new T(function(){
                var _rW/* shWN */ = E(_qU/* shTz */);
                switch(E(_rQ/* shWF */)){
                  case 1:
                    return B(_np/* GHC.Classes.modInt# */(_rW/* shWN */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52));
                    break;
                  case 2:
                    return B(_np/* GHC.Classes.modInt# */(_rW/* shWN */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52));
                    break;
                  default:
                    return B(_np/* GHC.Classes.modInt# */(_rW/* shWN */+(imul/* EXTERNAL */(13, 4-(1-B(_np/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52));
                }
              },1)));
            });
            return B(_qi/* LudoJS.$sinsert_$s$sgo2 */(_rV/* shX9 */, _rT/* shWK */));
          });
          return new T5(0,_rS/* shWG */.a,_rS/* shWG */.b,_rS/* shWG */.c,_rU/* shXa */,_rS/* shWG */.e);
        },1);
        return new F(function(){return _rn/* shUI */(_/* EXTERNAL */, _eb/* GHC.Tuple.() */, _rR/* shXb */);});
      }
    }else{
      var _rX/* shXl */ = new T(function(){
        var _rY/* shXc */ = E(_rm/* shUG */),
        _rZ/* shXg */ = _rY/* shXc */.d,
        _s0/* shXk */ = new T(function(){
          return B(_3j/* LudoJS.$sinsert_$sgo10 */(_qT/* shTy */, new T(function(){
            return B(_qB/* LudoJS.outByCell */(B(_da/* LudoJS.$s!1 */(_qT/* shTy */, _rZ/* shXg */)), _qU/* shTz */));
          }), _rZ/* shXg */));
        });
        return new T5(0,_rY/* shXc */.a,_rY/* shXc */.b,_rY/* shXc */.c,_s0/* shXk */,_rY/* shXc */.e);
      });
      return new T2(0,_eb/* GHC.Tuple.() */,_rX/* shXl */);
    }
  };
  switch(E(_qT/* shTy */)){
    case 0:
      var _s1/* shXq */ = function(_s2/*  shYs */, _s3/*  shYt */, _s4/*  shYu */, _/* EXTERNAL */){
        while(1){
          var _s5/*  shXq */ = B((function(_s6/* shYs */, _s7/* shYt */, _s8/* shYu */, _/* EXTERNAL */){
            var _s9/* shYw */ = E(_s6/* shYs */);
            if(!_s9/* shYw */._){
              return new T2(0,_s7/* shYt */,_s8/* shYu */);
            }else{
              var _sa/* shYz */ = _s9/* shYw */.b,
              _sb/* shYA */ = E(_s9/* shYw */.a);
              if(!_sb/* shYA */){
                var _sc/*   shYt */ = _s7/* shYt */,
                _sd/*   shYu */ = _s8/* shYu */;
                _s2/*  shYs */ = _sa/* shYz */;
                _s3/*  shYt */ = _sc/*   shYt */;
                _s4/*  shYu */ = _sd/*   shYu */;
                return __continue/* EXTERNAL */;
              }else{
                var _se/* shZ0 */ = new T(function(){
                  if(!E(_s7/* shYt */)){
                    var _sf/* shYI */ = function(_sg/*  shYJ */){
                      while(1){
                        var _sh/*  shYI */ = B((function(_si/* shYJ */){
                          var _sj/* shYK */ = E(_si/* shYJ */);
                          if(!_sj/* shYK */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _sk/* shYM */ = _sj/* shYK */.b,
                            _sl/* shYQ */ = E(E(_sj/* shYK */.a).b);
                            if(!_sl/* shYQ */._){
                              _sg/*  shYJ */ = _sk/* shYM */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_pP/* LudoJS.$wconvertCell */(_sb/* shYA */, E(_sl/* shYQ */.a), _1O/* LudoJS.Blue */));
                              }),new T(function(){
                                return B(_sf/* shYI */(_sk/* shYM */));
                              }));
                            }
                          }
                        })(_sg/*  shYJ */));
                        if(_sh/*  shYI */!=__continue/* EXTERNAL */){
                          return _sh/*  shYI */;
                        }
                      }
                    };
                    return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shTC */(B(_sf/* shYI */(B(_da/* LudoJS.$s!1 */(_sb/* shYA */, E(_s8/* shYu */).d)))))), _qy/* LudoJS.lvl10 */));
                  }else{
                    return true;
                  }
                }),
                _sd/*   shYu */ = _s8/* shYu */;
                _s2/*  shYs */ = _sa/* shYz */;
                _s3/*  shYt */ = _se/* shZ0 */;
                _s4/*  shYu */ = _sd/*   shYu */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_s2/*  shYs */, _s3/*  shYt */, _s4/*  shYu */, _/* EXTERNAL */));
          if(_s5/*  shXq */!=__continue/* EXTERNAL */){
            return _s5/*  shXq */;
          }
        }
      },
      _sm/* shXo */ = function(_sn/*  shXr */, _so/*  shXs */, _/* EXTERNAL */){
        while(1){
          var _sp/*  shXo */ = B((function(_sq/* shXr */, _sr/* shXs */, _/* EXTERNAL */){
            var _ss/* shXu */ = E(_sq/* shXr */);
            if(!_ss/* shXu */._){
              return new T2(0,_qp/* GHC.Types.False */,_sr/* shXs */);
            }else{
              var _st/* shXx */ = _ss/* shXu */.b,
              _su/* shXy */ = E(_ss/* shXu */.a);
              if(!_su/* shXy */){
                var _sv/*   shXs */ = _sr/* shXs */;
                _sn/*  shXr */ = _st/* shXx */;
                _so/*  shXs */ = _sv/*   shXs */;
                return __continue/* EXTERNAL */;
              }else{
                var _sw/* shXX */ = new T(function(){
                  var _sx/* shXF */ = function(_sy/*  shXG */){
                    while(1){
                      var _sz/*  shXF */ = B((function(_sA/* shXG */){
                        var _sB/* shXH */ = E(_sA/* shXG */);
                        if(!_sB/* shXH */._){
                          return __Z/* EXTERNAL */;
                        }else{
                          var _sC/* shXJ */ = _sB/* shXH */.b,
                          _sD/* shXN */ = E(E(_sB/* shXH */.a).b);
                          if(!_sD/* shXN */._){
                            _sy/*  shXG */ = _sC/* shXJ */;
                            return __continue/* EXTERNAL */;
                          }else{
                            return new T2(1,new T(function(){
                              return B(_pP/* LudoJS.$wconvertCell */(_su/* shXy */, E(_sD/* shXN */.a), _1O/* LudoJS.Blue */));
                            }),new T(function(){
                              return B(_sx/* shXF */(_sC/* shXJ */));
                            }));
                          }
                        }
                      })(_sy/*  shXG */));
                      if(_sz/*  shXF */!=__continue/* EXTERNAL */){
                        return _sz/*  shXF */;
                      }
                    }
                  };
                  return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shTC */(B(_sx/* shXF */(B(_da/* LudoJS.$s!1 */(_su/* shXy */, E(_sr/* shXs */).d)))))), _qy/* LudoJS.lvl10 */));
                });
                return new F(function(){return _s1/* shXq */(_st/* shXx */, _sw/* shXX */, _sr/* shXs */, _/* EXTERNAL */);});
              }
            }
          })(_sn/*  shXr */, _so/*  shXs */, _/* EXTERNAL */));
          if(_sp/*  shXo */!=__continue/* EXTERNAL */){
            return _sp/*  shXo */;
          }
        }
      },
      _sE/* shXp */ = function(_sF/* shXY */, _sG/* shXZ */, _sH/* shY0 */, _/* EXTERNAL */){
        var _sI/* shY2 */ = E(_sF/* shXY */);
        if(!_sI/* shY2 */){
          return new F(function(){return _sm/* shXo */(_sG/* shXZ */, _sH/* shY0 */, _/* EXTERNAL */);});
        }else{
          var _sJ/* shYr */ = new T(function(){
            var _sK/* shY9 */ = function(_sL/*  shYa */){
              while(1){
                var _sM/*  shY9 */ = B((function(_sN/* shYa */){
                  var _sO/* shYb */ = E(_sN/* shYa */);
                  if(!_sO/* shYb */._){
                    return __Z/* EXTERNAL */;
                  }else{
                    var _sP/* shYd */ = _sO/* shYb */.b,
                    _sQ/* shYh */ = E(E(_sO/* shYb */.a).b);
                    if(!_sQ/* shYh */._){
                      _sL/*  shYa */ = _sP/* shYd */;
                      return __continue/* EXTERNAL */;
                    }else{
                      return new T2(1,new T(function(){
                        return B(_pP/* LudoJS.$wconvertCell */(_sI/* shY2 */, E(_sQ/* shYh */.a), _1O/* LudoJS.Blue */));
                      }),new T(function(){
                        return B(_sK/* shY9 */(_sP/* shYd */));
                      }));
                    }
                  }
                })(_sL/*  shYa */));
                if(_sM/*  shY9 */!=__continue/* EXTERNAL */){
                  return _sM/*  shY9 */;
                }
              }
            };
            return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shTC */(B(_sK/* shY9 */(B(_da/* LudoJS.$s!1 */(_sI/* shY2 */, E(_sH/* shY0 */).d)))))), _qy/* LudoJS.lvl10 */));
          });
          return new F(function(){return _s1/* shXq */(_sG/* shXZ */, _sJ/* shYr */, _sH/* shY0 */, _/* EXTERNAL */);});
        }
      },
      _sR/* shZ1 */ = B(_sE/* shXp */(_1P/* LudoJS.Green */, _6b/* LudoJS.lvl8 */, _qV/* shTA */, _/* EXTERNAL */));
      return new F(function(){return _rj/* shUB */(_/* EXTERNAL */, _sR/* shZ1 */);});
      break;
    case 1:
      var _sS/* shZ4 */ = B(_r9/* shU8 */(_qV/* shTA */, _/* EXTERNAL */)),
      _sT/* shZ8 */ = function(_sU/*  shZd */, _sV/*  shZe */, _sW/*  shZf */, _/* EXTERNAL */){
        while(1){
          var _sX/*  shZ8 */ = B((function(_sY/* shZd */, _sZ/* shZe */, _t0/* shZf */, _/* EXTERNAL */){
            var _t1/* shZh */ = E(_sY/* shZd */);
            if(!_t1/* shZh */._){
              return new T2(0,_sZ/* shZe */,_t0/* shZf */);
            }else{
              var _t2/* shZk */ = _t1/* shZh */.b,
              _t3/* shZl */ = E(_t1/* shZh */.a);
              if(_t3/* shZl */==1){
                var _t4/*   shZe */ = _sZ/* shZe */,
                _t5/*   shZf */ = _t0/* shZf */;
                _sU/*  shZd */ = _t2/* shZk */;
                _sV/*  shZe */ = _t4/*   shZe */;
                _sW/*  shZf */ = _t5/*   shZf */;
                return __continue/* EXTERNAL */;
              }else{
                var _t6/* shZL */ = new T(function(){
                  if(!E(_sZ/* shZe */)){
                    var _t7/* shZt */ = function(_t8/*  shZu */){
                      while(1){
                        var _t9/*  shZt */ = B((function(_ta/* shZu */){
                          var _tb/* shZv */ = E(_ta/* shZu */);
                          if(!_tb/* shZv */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _tc/* shZx */ = _tb/* shZv */.b,
                            _td/* shZB */ = E(E(_tb/* shZv */.a).b);
                            if(!_td/* shZB */._){
                              _t8/*  shZu */ = _tc/* shZx */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_pP/* LudoJS.$wconvertCell */(_t3/* shZl */, E(_td/* shZB */.a), _1P/* LudoJS.Green */));
                              }),new T(function(){
                                return B(_t7/* shZt */(_tc/* shZx */));
                              }));
                            }
                          }
                        })(_t8/*  shZu */));
                        if(_t9/*  shZt */!=__continue/* EXTERNAL */){
                          return _t9/*  shZt */;
                        }
                      }
                    };
                    return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shTC */(B(_t7/* shZt */(B(_da/* LudoJS.$s!1 */(_t3/* shZl */, E(_t0/* shZf */).d)))))), _qy/* LudoJS.lvl10 */));
                  }else{
                    return true;
                  }
                }),
                _t5/*   shZf */ = _t0/* shZf */;
                _sU/*  shZd */ = _t2/* shZk */;
                _sV/*  shZe */ = _t6/* shZL */;
                _sW/*  shZf */ = _t5/*   shZf */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_sU/*  shZd */, _sV/*  shZe */, _sW/*  shZf */, _/* EXTERNAL */));
          if(_sX/*  shZ8 */!=__continue/* EXTERNAL */){
            return _sX/*  shZ8 */;
          }
        }
      },
      _te/* shZU */ = B((function(_tf/* shZ9 */, _tg/* shZa */, _th/* shZb */, _/* EXTERNAL */){
        return new F(function(){return _sT/* shZ8 */(_tf/* shZ9 */, _tg/* shZa */, _th/* shZb */, _/* EXTERNAL */);});
      })(_6b/* LudoJS.lvl8 */, new T(function(){
        return E(E(_sS/* shZ4 */).a);
      }), new T(function(){
        return E(E(_sS/* shZ4 */).b);
      }), _/* EXTERNAL */));
      return new F(function(){return _rj/* shUB */(_/* EXTERNAL */, _te/* shZU */);});
      break;
    case 2:
      var _ti/* shZX */ = B(_r9/* shU8 */(_qV/* shTA */, _/* EXTERNAL */)),
      _tj/* si01 */ = function(_tk/*  si0y */, _tl/*  si0z */, _tm/*  si0A */, _/* EXTERNAL */){
        while(1){
          var _tn/*  si01 */ = B((function(_to/* si0y */, _tp/* si0z */, _tq/* si0A */, _/* EXTERNAL */){
            var _tr/* si0C */ = E(_to/* si0y */);
            if(!_tr/* si0C */._){
              return new T2(0,_tp/* si0z */,_tq/* si0A */);
            }else{
              var _ts/* si0F */ = _tr/* si0C */.b,
              _tt/* si0G */ = E(_tr/* si0C */.a);
              if(_tt/* si0G */==2){
                var _tu/*   si0z */ = _tp/* si0z */,
                _tv/*   si0A */ = _tq/* si0A */;
                _tk/*  si0y */ = _ts/* si0F */;
                _tl/*  si0z */ = _tu/*   si0z */;
                _tm/*  si0A */ = _tv/*   si0A */;
                return __continue/* EXTERNAL */;
              }else{
                var _tw/* si16 */ = new T(function(){
                  if(!E(_tp/* si0z */)){
                    var _tx/* si0O */ = function(_ty/*  si0P */){
                      while(1){
                        var _tz/*  si0O */ = B((function(_tA/* si0P */){
                          var _tB/* si0Q */ = E(_tA/* si0P */);
                          if(!_tB/* si0Q */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _tC/* si0S */ = _tB/* si0Q */.b,
                            _tD/* si0W */ = E(E(_tB/* si0Q */.a).b);
                            if(!_tD/* si0W */._){
                              _ty/*  si0P */ = _tC/* si0S */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_pP/* LudoJS.$wconvertCell */(_tt/* si0G */, E(_tD/* si0W */.a), _1Q/* LudoJS.Red */));
                              }),new T(function(){
                                return B(_tx/* si0O */(_tC/* si0S */));
                              }));
                            }
                          }
                        })(_ty/*  si0P */));
                        if(_tz/*  si0O */!=__continue/* EXTERNAL */){
                          return _tz/*  si0O */;
                        }
                      }
                    };
                    return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shTC */(B(_tx/* si0O */(B(_da/* LudoJS.$s!1 */(_tt/* si0G */, E(_tq/* si0A */).d)))))), _qy/* LudoJS.lvl10 */));
                  }else{
                    return true;
                  }
                }),
                _tv/*   si0A */ = _tq/* si0A */;
                _tk/*  si0y */ = _ts/* si0F */;
                _tl/*  si0z */ = _tw/* si16 */;
                _tm/*  si0A */ = _tv/*   si0A */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_tk/*  si0y */, _tl/*  si0z */, _tm/*  si0A */, _/* EXTERNAL */));
          if(_tn/*  si01 */!=__continue/* EXTERNAL */){
            return _tn/*  si01 */;
          }
        }
      },
      _tE/* si00 */ = function(_tF/* si02 */, _tG/* si03 */, _tH/* si04 */, _tI/* si05 */, _/* EXTERNAL */){
        var _tJ/* si07 */ = E(_tF/* si02 */);
        if(_tJ/* si07 */==2){
          return new F(function(){return _tj/* si01 */(_tG/* si03 */, _tH/* si04 */, _tI/* si05 */, _/* EXTERNAL */);});
        }else{
          var _tK/* si0x */ = new T(function(){
            if(!E(_tH/* si04 */)){
              var _tL/* si0f */ = function(_tM/*  si0g */){
                while(1){
                  var _tN/*  si0f */ = B((function(_tO/* si0g */){
                    var _tP/* si0h */ = E(_tO/* si0g */);
                    if(!_tP/* si0h */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _tQ/* si0j */ = _tP/* si0h */.b,
                      _tR/* si0n */ = E(E(_tP/* si0h */.a).b);
                      if(!_tR/* si0n */._){
                        _tM/*  si0g */ = _tQ/* si0j */;
                        return __continue/* EXTERNAL */;
                      }else{
                        return new T2(1,new T(function(){
                          return B(_pP/* LudoJS.$wconvertCell */(_tJ/* si07 */, E(_tR/* si0n */.a), _1Q/* LudoJS.Red */));
                        }),new T(function(){
                          return B(_tL/* si0f */(_tQ/* si0j */));
                        }));
                      }
                    }
                  })(_tM/*  si0g */));
                  if(_tN/*  si0f */!=__continue/* EXTERNAL */){
                    return _tN/*  si0f */;
                  }
                }
              };
              return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shTC */(B(_tL/* si0f */(B(_da/* LudoJS.$s!1 */(_tJ/* si07 */, E(_tI/* si05 */).d)))))), _qy/* LudoJS.lvl10 */));
            }else{
              return true;
            }
          });
          return new F(function(){return _tj/* si01 */(_tG/* si03 */, _tK/* si0x */, _tI/* si05 */, _/* EXTERNAL */);});
        }
      },
      _tS/* si1f */ = B(_tE/* si00 */(_1P/* LudoJS.Green */, _6b/* LudoJS.lvl8 */, new T(function(){
        return E(E(_ti/* shZX */).a);
      }), new T(function(){
        return E(E(_ti/* shZX */).b);
      }), _/* EXTERNAL */));
      return new F(function(){return _rj/* shUB */(_/* EXTERNAL */, _tS/* si1f */);});
      break;
    default:
      var _tT/* si1i */ = B(_r9/* shU8 */(_qV/* shTA */, _/* EXTERNAL */)),
      _tU/* si1m */ = function(_tV/*  si1T */, _tW/*  si1U */, _tX/*  si1V */, _/* EXTERNAL */){
        while(1){
          var _tY/*  si1m */ = B((function(_tZ/* si1T */, _u0/* si1U */, _u1/* si1V */, _/* EXTERNAL */){
            var _u2/* si1X */ = E(_tZ/* si1T */);
            if(!_u2/* si1X */._){
              return new T2(0,_u0/* si1U */,_u1/* si1V */);
            }else{
              var _u3/* si20 */ = _u2/* si1X */.b,
              _u4/* si21 */ = E(_u2/* si1X */.a);
              if(_u4/* si21 */==3){
                var _u5/*   si1U */ = _u0/* si1U */,
                _u6/*   si1V */ = _u1/* si1V */;
                _tV/*  si1T */ = _u3/* si20 */;
                _tW/*  si1U */ = _u5/*   si1U */;
                _tX/*  si1V */ = _u6/*   si1V */;
                return __continue/* EXTERNAL */;
              }else{
                var _u7/* si2r */ = new T(function(){
                  if(!E(_u0/* si1U */)){
                    var _u8/* si29 */ = function(_u9/*  si2a */){
                      while(1){
                        var _ua/*  si29 */ = B((function(_ub/* si2a */){
                          var _uc/* si2b */ = E(_ub/* si2a */);
                          if(!_uc/* si2b */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _ud/* si2d */ = _uc/* si2b */.b,
                            _ue/* si2h */ = E(E(_uc/* si2b */.a).b);
                            if(!_ue/* si2h */._){
                              _u9/*  si2a */ = _ud/* si2d */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_pP/* LudoJS.$wconvertCell */(_u4/* si21 */, E(_ue/* si2h */.a), _1S/* LudoJS.Yellow */));
                              }),new T(function(){
                                return B(_u8/* si29 */(_ud/* si2d */));
                              }));
                            }
                          }
                        })(_u9/*  si2a */));
                        if(_ua/*  si29 */!=__continue/* EXTERNAL */){
                          return _ua/*  si29 */;
                        }
                      }
                    };
                    return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shTC */(B(_u8/* si29 */(B(_da/* LudoJS.$s!1 */(_u4/* si21 */, E(_u1/* si1V */).d)))))), _qy/* LudoJS.lvl10 */));
                  }else{
                    return true;
                  }
                }),
                _u6/*   si1V */ = _u1/* si1V */;
                _tV/*  si1T */ = _u3/* si20 */;
                _tW/*  si1U */ = _u7/* si2r */;
                _tX/*  si1V */ = _u6/*   si1V */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_tV/*  si1T */, _tW/*  si1U */, _tX/*  si1V */, _/* EXTERNAL */));
          if(_tY/*  si1m */!=__continue/* EXTERNAL */){
            return _tY/*  si1m */;
          }
        }
      },
      _uf/* si1l */ = function(_ug/* si1n */, _uh/* si1o */, _ui/* si1p */, _uj/* si1q */, _/* EXTERNAL */){
        var _uk/* si1s */ = E(_ug/* si1n */);
        if(_uk/* si1s */==3){
          return new F(function(){return _tU/* si1m */(_uh/* si1o */, _ui/* si1p */, _uj/* si1q */, _/* EXTERNAL */);});
        }else{
          var _ul/* si1S */ = new T(function(){
            if(!E(_ui/* si1p */)){
              var _um/* si1A */ = function(_un/*  si1B */){
                while(1){
                  var _uo/*  si1A */ = B((function(_up/* si1B */){
                    var _uq/* si1C */ = E(_up/* si1B */);
                    if(!_uq/* si1C */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _ur/* si1E */ = _uq/* si1C */.b,
                      _us/* si1I */ = E(E(_uq/* si1C */.a).b);
                      if(!_us/* si1I */._){
                        _un/*  si1B */ = _ur/* si1E */;
                        return __continue/* EXTERNAL */;
                      }else{
                        return new T2(1,new T(function(){
                          return B(_pP/* LudoJS.$wconvertCell */(_uk/* si1s */, E(_us/* si1I */.a), _1S/* LudoJS.Yellow */));
                        }),new T(function(){
                          return B(_um/* si1A */(_ur/* si1E */));
                        }));
                      }
                    }
                  })(_un/*  si1B */));
                  if(_uo/*  si1A */!=__continue/* EXTERNAL */){
                    return _uo/*  si1A */;
                  }
                }
              };
              return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* shTC */(B(_um/* si1A */(B(_da/* LudoJS.$s!1 */(_uk/* si1s */, E(_uj/* si1q */).d)))))), _qy/* LudoJS.lvl10 */));
            }else{
              return true;
            }
          });
          return new F(function(){return _tU/* si1m */(_uh/* si1o */, _ul/* si1S */, _uj/* si1q */, _/* EXTERNAL */);});
        }
      },
      _ut/* si2A */ = B(_uf/* si1l */(_1P/* LudoJS.Green */, _6b/* LudoJS.lvl8 */, new T(function(){
        return E(E(_tT/* si1i */).a);
      }), new T(function(){
        return E(E(_tT/* si1i */).b);
      }), _/* EXTERNAL */));
      return new F(function(){return _rj/* shUB */(_/* EXTERNAL */, _ut/* si2A */);});
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
_vo/* $wa7 */ = function(_vp/* si2D */, _vq/* si2E */, _vr/* si2F */, _/* EXTERNAL */){
  var _vs/* si2H */ = new T(function(){
    return E(E(_vr/* si2F */).b);
  }),
  _vt/* si2O */ = new T(function(){
    return E(E(_vr/* si2F */).d);
  }),
  _vu/* si2V */ = E(_vq/* si2E */);
  if(_vu/* si2V */==56){
    var _vv/* si7W */ = new T(function(){
      var _vw/* si7B */ = E(_vr/* si2F */),
      _vx/* si7V */ = new T(function(){
        var _vy/* si7U */ = new T(function(){
          var _vz/* si7H */ = B(_da/* LudoJS.$s!1 */(_vs/* si2H */, _vt/* si2O */));
          if(!_vz/* si7H */._){
            return __Z/* EXTERNAL */;
          }else{
            var _vA/* si7J */ = _vz/* si7H */.b,
            _vB/* si7K */ = E(_vz/* si7H */.a),
            _vC/* si7N */ = E(_vp/* si2D */);
            if(_vC/* si7N */!=E(_vB/* si7K */.a)){
              return new T2(1,_vB/* si7K */,new T(function(){
                return B(_pu/* LudoJS.$sremoveFrom */(_vA/* si7J */, _vC/* si7N */));
              }));
            }else{
              return E(_vA/* si7J */);
            }
          }
        });
        return B(_3j/* LudoJS.$sinsert_$sgo10 */(_vs/* si2H */, _vy/* si7U */, _vt/* si2O */));
      });
      return new T5(0,_vw/* si7B */.a,_vw/* si7B */.b,_vw/* si7B */.c,_vx/* si7V */,_vw/* si7B */.e);
    });
    return new T2(0,_eb/* GHC.Tuple.() */,_vv/* si7W */);
  }else{
    if(!B(_uw/* GHC.List.elem */(_pm/* GHC.Classes.$fEqInt */, _vu/* si2V */, _vl/* LudoJS.starCells */))){
      if(!B(_uw/* GHC.List.elem */(_pm/* GHC.Classes.$fEqInt */, _vu/* si2V */, _v3/* LudoJS.globeCells */))){
        if(_vu/* si2V */<51){
          var _vD/* si3p */ = new T(function(){
            var _vE/* si31 */ = E(_vr/* si2F */),
            _vF/* si3o */ = new T(function(){
              var _vG/* si3m */ = new T(function(){
                var _vH/* si39 */ = B(_da/* LudoJS.$s!1 */(_vs/* si2H */, _vt/* si2O */));
                if(!_vH/* si39 */._){
                  return __Z/* EXTERNAL */;
                }else{
                  var _vI/* si3b */ = _vH/* si39 */.b,
                  _vJ/* si3c */ = E(_vH/* si39 */.a),
                  _vK/* si3f */ = E(_vp/* si2D */);
                  if(_vK/* si3f */!=E(_vJ/* si3c */.a)){
                    return new T2(1,_vJ/* si3c */,new T(function(){
                      return B(_pu/* LudoJS.$sremoveFrom */(_vI/* si3b */, _vK/* si3f */));
                    }));
                  }else{
                    return E(_vI/* si3b */);
                  }
                }
              });
              return B(_3j/* LudoJS.$sinsert_$sgo10 */(_vs/* si2H */, new T2(1,new T2(0,_vp/* si2D */,new T1(1,_vu/* si2V */)),_vG/* si3m */), _vt/* si2O */));
            });
            return new T5(0,_vE/* si31 */.a,_vE/* si31 */.b,_vE/* si31 */.c,_vF/* si3o */,_vE/* si31 */.e);
          });
          return new F(function(){return _qS/* LudoJS.a43 */(_vs/* si2H */, _vu/* si2V */, _vD/* si3p */, _/* EXTERNAL */);});
        }else{
          var _vL/* si3O */ = new T(function(){
            var _vM/* si3q */ = E(_vr/* si2F */),
            _vN/* si3N */ = new T(function(){
              var _vO/* si3L */ = new T(function(){
                var _vP/* si3y */ = B(_da/* LudoJS.$s!1 */(_vs/* si2H */, _vt/* si2O */));
                if(!_vP/* si3y */._){
                  return __Z/* EXTERNAL */;
                }else{
                  var _vQ/* si3A */ = _vP/* si3y */.b,
                  _vR/* si3B */ = E(_vP/* si3y */.a),
                  _vS/* si3E */ = E(_vp/* si2D */);
                  if(_vS/* si3E */!=E(_vR/* si3B */.a)){
                    return new T2(1,_vR/* si3B */,new T(function(){
                      return B(_pu/* LudoJS.$sremoveFrom */(_vQ/* si3A */, _vS/* si3E */));
                    }));
                  }else{
                    return E(_vQ/* si3A */);
                  }
                }
              });
              return B(_3j/* LudoJS.$sinsert_$sgo10 */(_vs/* si2H */, new T2(1,new T2(0,_vp/* si2D */,new T1(1,_vu/* si2V */)),_vO/* si3L */), _vt/* si2O */));
            });
            return new T5(0,_vM/* si3q */.a,_vM/* si3q */.b,_vM/* si3q */.c,_vN/* si3N */,_vM/* si3q */.e);
          });
          return new T2(0,_eb/* GHC.Tuple.() */,_vL/* si3O */);
        }
      }else{
        var _vT/* si3Q */ = E(_vr/* si2F */),
        _vU/* si3R */ = _vT/* si3Q */.a,
        _vV/* si3S */ = _vT/* si3Q */.b,
        _vW/* si3T */ = _vT/* si3Q */.c,
        _vX/* si3V */ = _vT/* si3Q */.e,
        _vY/* si3W */ = function(_vZ/* si3X */, _w0/* si3Y */, _w1/* si3Z */, _w2/* si40 */, _w3/* si41 */, _w4/* si42 */, _/* EXTERNAL */){
          var _w5/* si44 */ = new T(function(){
            return B(_pP/* LudoJS.$wconvertCell */(_w1/* si3Z */, _vu/* si2V */, _vZ/* si3X */));
          }),
          _w6/* si46 */ = function(_w7/*  si47 */){
            while(1){
              var _w8/*  si46 */ = B((function(_w9/* si47 */){
                var _wa/* si48 */ = E(_w9/* si47 */);
                if(!_wa/* si48 */._){
                  return false;
                }else{
                  var _wb/* si4a */ = _wa/* si48 */.b,
                  _wc/* si4e */ = E(E(_wa/* si48 */.a).b);
                  if(!_wc/* si4e */._){
                    _w7/*  si47 */ = _wb/* si4a */;
                    return __continue/* EXTERNAL */;
                  }else{
                    var _wd/* si4g */ = E(_w5/* si44 */);
                    if(_wd/* si4g */!=E(_wc/* si4e */.a)){
                      var _we/* si4m */ = function(_wf/* si4n */){
                        while(1){
                          var _wg/* si4o */ = E(_wf/* si4n */);
                          if(!_wg/* si4o */._){
                            return false;
                          }else{
                            var _wh/* si4q */ = _wg/* si4o */.b,
                            _wi/* si4u */ = E(E(_wg/* si4o */.a).b);
                            if(!_wi/* si4u */._){
                              _wf/* si4n */ = _wh/* si4q */;
                              continue;
                            }else{
                              if(_wd/* si4g */!=E(_wi/* si4u */.a)){
                                _wf/* si4n */ = _wh/* si4q */;
                                continue;
                              }else{
                                return true;
                              }
                            }
                          }
                        }
                      };
                      return new F(function(){return _we/* si4m */(_wb/* si4a */);});
                    }else{
                      return true;
                    }
                  }
                }
              })(_w7/*  si47 */));
              if(_w8/*  si46 */!=__continue/* EXTERNAL */){
                return _w8/*  si46 */;
              }
            }
          };
          if(!B(_w6/* si46 */(B(_da/* LudoJS.$s!1 */(_vZ/* si3X */, _w3/* si41 */))))){
            return new T2(0,_eb/* GHC.Tuple.() */,new T5(0,_w0/* si3Y */,_w1/* si3Z */,_w2/* si40 */,_w3/* si41 */,_w4/* si42 */));
          }else{
            var _wj/* si4G */ = new T(function(){
              return B(_3j/* LudoJS.$sinsert_$sgo10 */(_w1/* si3Z */, new T(function(){
                return B(_pn/* LudoJS.$soutByCell */(B(_da/* LudoJS.$s!1 */(_w1/* si3Z */, _w3/* si41 */)), _vu/* si2V */));
              }), _w3/* si41 */));
            });
            return new T2(0,_eb/* GHC.Tuple.() */,new T5(0,_w0/* si3Y */,_w1/* si3Z */,_w2/* si40 */,_wj/* si4G */,_w4/* si42 */));
          }
        },
        _wk/* si4J */ = function(_wl/* si4K */, _wm/* si4L */, _wn/* si4M */, _wo/* si4N */, _wp/* si4O */, _/* EXTERNAL */){
          var _wq/* si4Q */ = function(_wr/* si4R */, _ws/* si4S */, _wt/* si4T */, _wu/* si4U */, _wv/* si4V */, _/* EXTERNAL */){
            var _ww/* si4X */ = E(_vs/* si2H */);
            if(_ww/* si4X */==3){
              return new F(function(){return _vY/* si3W */(_1Q/* LudoJS.Red */, _wr/* si4R */, _ws/* si4S */, _wt/* si4T */, _wu/* si4U */, _wv/* si4V */, _/* EXTERNAL */);});
            }else{
              var _wx/* si4Y */ = B(_vY/* si3W */(_1S/* LudoJS.Yellow */, _wr/* si4R */, _ws/* si4S */, _wt/* si4T */, _wu/* si4U */, _wv/* si4V */, _/* EXTERNAL */));
              if(E(_ww/* si4X */)==2){
                return new T2(0,_eb/* GHC.Tuple.() */,new T(function(){
                  return E(E(_wx/* si4Y */).b);
                }));
              }else{
                var _wy/* si55 */ = E(E(_wx/* si4Y */).b);
                return new F(function(){return _vY/* si3W */(_1Q/* LudoJS.Red */, _wy/* si55 */.a, _wy/* si55 */.b, _wy/* si55 */.c, _wy/* si55 */.d, _wy/* si55 */.e, _/* EXTERNAL */);});
              }
            }
          };
          if(E(_vs/* si2H */)==1){
            return new F(function(){return _wq/* si4Q */(_wl/* si4K */, _wm/* si4L */, _wn/* si4M */, _wo/* si4N */, _wp/* si4O */, _/* EXTERNAL */);});
          }else{
            var _wz/* si5h */ = B(_vY/* si3W */(_1P/* LudoJS.Green */, _wl/* si4K */, _wm/* si4L */, _wn/* si4M */, _wo/* si4N */, _wp/* si4O */, _/* EXTERNAL */)),
            _wA/* si5n */ = E(E(_wz/* si5h */).b);
            return new F(function(){return _wq/* si4Q */(_wA/* si5n */.a, _wA/* si5n */.b, _wA/* si5n */.c, _wA/* si5n */.d, _wA/* si5n */.e, _/* EXTERNAL */);});
          }
        },
        _wB/* si5t */ = new T(function(){
          var _wC/* si5J */ = new T(function(){
            var _wD/* si5w */ = B(_da/* LudoJS.$s!1 */(_vs/* si2H */, _vt/* si2O */));
            if(!_wD/* si5w */._){
              return __Z/* EXTERNAL */;
            }else{
              var _wE/* si5y */ = _wD/* si5w */.b,
              _wF/* si5z */ = E(_wD/* si5w */.a),
              _wG/* si5C */ = E(_vp/* si2D */);
              if(_wG/* si5C */!=E(_wF/* si5z */.a)){
                return new T2(1,_wF/* si5z */,new T(function(){
                  return B(_pu/* LudoJS.$sremoveFrom */(_wE/* si5y */, _wG/* si5C */));
                }));
              }else{
                return E(_wE/* si5y */);
              }
            }
          });
          return B(_3j/* LudoJS.$sinsert_$sgo10 */(_vs/* si2H */, new T2(1,new T2(0,_vp/* si2D */,new T1(1,_vu/* si2V */)),_wC/* si5J */), _vt/* si2O */));
        });
        if(!E(_vs/* si2H */)){
          return new F(function(){return _wk/* si4J */(_vU/* si3R */, _vV/* si3S */, _vW/* si3T */, _wB/* si5t */, _vX/* si3V */, _/* EXTERNAL */);});
        }else{
          var _wH/* si5M */ = B(_vY/* si3W */(_1O/* LudoJS.Blue */, _vU/* si3R */, _vV/* si3S */, _vW/* si3T */, _wB/* si5t */, _vX/* si3V */, _/* EXTERNAL */)),
          _wI/* si5S */ = E(E(_wH/* si5M */).b);
          return new F(function(){return _wk/* si4J */(_wI/* si5S */.a, _wI/* si5S */.b, _wI/* si5S */.c, _wI/* si5S */.d, _wI/* si5S */.e, _/* EXTERNAL */);});
        }
      }
    }else{
      var _wJ/* si6m */ = new T(function(){
        var _wK/* si5Y */ = E(_vr/* si2F */),
        _wL/* si6l */ = new T(function(){
          var _wM/* si6j */ = new T(function(){
            var _wN/* si66 */ = B(_da/* LudoJS.$s!1 */(_vs/* si2H */, _vt/* si2O */));
            if(!_wN/* si66 */._){
              return __Z/* EXTERNAL */;
            }else{
              var _wO/* si68 */ = _wN/* si66 */.b,
              _wP/* si69 */ = E(_wN/* si66 */.a),
              _wQ/* si6c */ = E(_vp/* si2D */);
              if(_wQ/* si6c */!=E(_wP/* si69 */.a)){
                return new T2(1,_wP/* si69 */,new T(function(){
                  return B(_pu/* LudoJS.$sremoveFrom */(_wO/* si68 */, _wQ/* si6c */));
                }));
              }else{
                return E(_wO/* si68 */);
              }
            }
          });
          return B(_3j/* LudoJS.$sinsert_$sgo10 */(_vs/* si2H */, new T2(1,new T2(0,_vp/* si2D */,new T1(1,_vu/* si2V */)),_wM/* si6j */), _vt/* si2O */));
        });
        return new T5(0,_wK/* si5Y */.a,_wK/* si5Y */.b,_wK/* si5Y */.c,_wL/* si6l */,_wK/* si5Y */.e);
      }),
      _wR/* si6n */ = B(_qS/* LudoJS.a43 */(_vs/* si2H */, _vu/* si2V */, _wJ/* si6m */, _/* EXTERNAL */)),
      _wS/* si6t */ = E(E(_wR/* si6n */).b),
      _wT/* si6x */ = _wS/* si6t */.d,
      _wU/* si6z */ = function(_wV/* si6A */){
        while(1){
          var _wW/* si6B */ = E(_wV/* si6A */);
          if(!_wW/* si6B */._){
            return false;
          }else{
            var _wX/* si6D */ = _wW/* si6B */.b,
            _wY/* si6H */ = E(E(_wW/* si6B */.a).b);
            if(!_wY/* si6H */._){
              _wV/* si6A */ = _wX/* si6D */;
              continue;
            }else{
              if(_vu/* si2V */!=E(_wY/* si6H */.a)){
                _wV/* si6A */ = _wX/* si6D */;
                continue;
              }else{
                return true;
              }
            }
          }
        }
      };
      if(!B(_wU/* si6z */(B(_da/* LudoJS.$s!1 */(_vs/* si2H */, _wT/* si6x */))))){
        return new T2(0,_eb/* GHC.Tuple.() */,_wS/* si6t */);
      }else{
        var _wZ/* si6Q */ = new T(function(){
          var _x0/* si6S */ = B(_uB/* Data.OldList.findIndex */(function(_x1/* B1 */){
            return new F(function(){return _mx/* GHC.Classes.eqInt */(_vu/* si2V */, _x1/* B1 */);});
          }, _vl/* LudoJS.starCells */));
          if(!_x0/* si6S */._){
            return E(_uP/* Data.Maybe.fromJust1 */);
          }else{
            return E(_x0/* si6S */.a);
          }
        }),
        _x2/* si6U */ = new T(function(){
          return B(_pM/* GHC.List.$w!! */(_vm/* LudoJS.lvl5 */, E(_wZ/* si6Q */)+1|0));
        }),
        _x3/* si7z */ = new T(function(){
          var _x4/* si7y */ = new T(function(){
            if((E(_wZ/* si6Q */)+1|0)!=E(_vn/* LudoJS.lvl6 */)){
              var _x5/* si7k */ = new T(function(){
                var _x6/* si77 */ = B(_da/* LudoJS.$s!1 */(_vs/* si2H */, _wT/* si6x */));
                if(!_x6/* si77 */._){
                  return __Z/* EXTERNAL */;
                }else{
                  var _x7/* si79 */ = _x6/* si77 */.b,
                  _x8/* si7a */ = E(_x6/* si77 */.a),
                  _x9/* si7d */ = E(_vp/* si2D */);
                  if(_x9/* si7d */!=E(_x8/* si7a */.a)){
                    return new T2(1,_x8/* si7a */,new T(function(){
                      return B(_pu/* LudoJS.$sremoveFrom */(_x7/* si79 */, _x9/* si7d */));
                    }));
                  }else{
                    return E(_x7/* si79 */);
                  }
                }
              });
              return new T2(1,new T2(0,_vp/* si2D */,new T1(1,_x2/* si6U */)),_x5/* si7k */);
            }else{
              var _xa/* si7l */ = B(_da/* LudoJS.$s!1 */(_vs/* si2H */, _wT/* si6x */));
              if(!_xa/* si7l */._){
                return __Z/* EXTERNAL */;
              }else{
                var _xb/* si7n */ = _xa/* si7l */.b,
                _xc/* si7o */ = E(_xa/* si7l */.a),
                _xd/* si7r */ = E(_vp/* si2D */);
                if(_xd/* si7r */!=E(_xc/* si7o */.a)){
                  return new T2(1,_xc/* si7o */,new T(function(){
                    return B(_pu/* LudoJS.$sremoveFrom */(_xb/* si7n */, _xd/* si7r */));
                  }));
                }else{
                  return E(_xb/* si7n */);
                }
              }
            }
          });
          return B(_3j/* LudoJS.$sinsert_$sgo10 */(_vs/* si2H */, _x4/* si7y */, _wT/* si6x */));
        });
        return new F(function(){return _qS/* LudoJS.a43 */(_vs/* si2H */, _x2/* si6U */, new T5(0,_wS/* si6t */.a,_wS/* si6t */.b,_wS/* si6t */.c,_x3/* si7z */,_wS/* si6t */.e), _/* EXTERNAL */);});
      }
    }
  }
},
_xe/* True */ = true,
_xf/* $wa14 */ = function(_xg/* siDN */, _xh/* siDO */, _xi/* siDP */, _xj/* siDQ */, _xk/* siDR */, _/* EXTERNAL */){
  var _xl/* siDV */ = new T5(0,_xg/* siDN */,_xh/* siDO */,_xi/* siDP */,_xj/* siDQ */,_xk/* siDR */),
  _xm/* siDW */ = function(_xn/* siDX */){
    var _xo/* siDY */ = B(_nw/* LudoJS.$wa13 */(_xl/* siDV */, _/* EXTERNAL */)),
    _xp/* siE1 */ = E(_xo/* siDY */),
    _xq/* siE4 */ = E(_xp/* siE1 */.b),
    _xr/* siEa */ = B(_xf/* LudoJS.$wa14 */(_xq/* siE4 */.a, _xq/* siE4 */.b, _xq/* siE4 */.c, _xq/* siE4 */.d, _xq/* siE4 */.e, _/* EXTERNAL */));
    return new T2(0,new T2(1,_xp/* siE1 */.a,new T(function(){
      return E(E(_xr/* siEa */).a);
    })),new T(function(){
      return E(E(_xr/* siEa */).b);
    }));
  };
  if(!E(B(_mL/* GHC.List.$wlenAcc */(B(_da/* LudoJS.$s!1 */(_xh/* siDO */, _xj/* siDQ */)), 0)))){
    return new F(function(){return _xm/* siDW */(_/* EXTERNAL */);});
  }else{
    if(E(_xi/* siDP */)>=1){
      return new T2(0,_4/* GHC.Types.[] */,_xl/* siDV */);
    }else{
      return new F(function(){return _xm/* siDW */(_/* EXTERNAL */);});
    }
  }
},
_xs/* f1 */ = new T(function(){
  return eval/* EXTERNAL */("((gs, opts, rolls, prevRolls) => drawDiceAnimation(gs, opts, rolls, prevRolls))");
}),
_xt/* lvl41 */ = function(_xu/* siFf */){
  var _xv/* siFg */ = B(_gY/* System.Random.$w$crandomR12 */(_gV/* System.Random.Internal.$fRandomGenStdGen */, 1, 6, _xu/* siFf */));
  return new T2(0,E(_xv/* siFg */.b),_xv/* siFg */.a);
},
_xw/* $fFractionalFixed1 */ = new T1(0,0),
_xx/* lvl */ = new T1(0,0),
_xy/* orInteger */ = function(_xz/* s1KS */, _xA/* s1KT */){
  while(1){
    var _xB/* s1KU */ = E(_xz/* s1KS */);
    if(!_xB/* s1KU */._){
      var _xC/* s1KV */ = _xB/* s1KU */.a,
      _xD/* s1KW */ = E(_xA/* s1KT */);
      if(!_xD/* s1KW */._){
        return new T1(0,(_xC/* s1KV */>>>0|_xD/* s1KW */.a>>>0)>>>0&4294967295);
      }else{
        _xz/* s1KS */ = new T1(1,I_fromInt/* EXTERNAL */(_xC/* s1KV */));
        _xA/* s1KT */ = _xD/* s1KW */;
        continue;
      }
    }else{
      var _xE/* s1L7 */ = E(_xA/* s1KT */);
      if(!_xE/* s1L7 */._){
        _xz/* s1KS */ = _xB/* s1KU */;
        _xA/* s1KT */ = new T1(1,I_fromInt/* EXTERNAL */(_xE/* s1L7 */.a));
        continue;
      }else{
        return new T1(1,I_or/* EXTERNAL */(_xB/* s1KU */.a, _xE/* s1L7 */.a));
      }
    }
  }
},
_xF/* shiftLInteger */ = function(_xG/* s1Jk */, _xH/* s1Jl */){
  while(1){
    var _xI/* s1Jm */ = E(_xG/* s1Jk */);
    if(!_xI/* s1Jm */._){
      _xG/* s1Jk */ = new T1(1,I_fromInt/* EXTERNAL */(_xI/* s1Jm */.a));
      continue;
    }else{
      return new T1(1,I_shiftLeft/* EXTERNAL */(_xI/* s1Jm */.a, _xH/* s1Jl */));
    }
  }
},
_xJ/* mkInteger_f */ = function(_xK/* s1S6 */){
  var _xL/* s1S7 */ = E(_xK/* s1S6 */);
  if(!_xL/* s1S7 */._){
    return E(_xx/* GHC.Integer.Type.lvl */);
  }else{
    return new F(function(){return _xy/* GHC.Integer.Type.orInteger */(new T1(0,E(_xL/* s1S7 */.a)), B(_xF/* GHC.Integer.Type.shiftLInteger */(B(_xJ/* GHC.Integer.Type.mkInteger_f */(_xL/* s1S7 */.b)), 31)));});
  }
},
_xM/* log2I1 */ = new T1(0,1),
_xN/* lvl2 */ = new T1(0,2147483647),
_xO/* lvl3 */ = new T(function(){
  return B(_qJ/* GHC.Integer.Type.plusInteger */(_xN/* GHC.Integer.Type.lvl2 */, _xM/* GHC.Integer.Type.log2I1 */));
}),
_xP/* negateInteger */ = function(_xQ/* s1QH */){
  var _xR/* s1QI */ = E(_xQ/* s1QH */);
  if(!_xR/* s1QI */._){
    var _xS/* s1QK */ = E(_xR/* s1QI */.a);
    return (_xS/* s1QK */==( -2147483648)) ? E(_xO/* GHC.Integer.Type.lvl3 */) : new T1(0, -_xS/* s1QK */);
  }else{
    return new T1(1,I_negate/* EXTERNAL */(_xR/* s1QI */.a));
  }
},
_xT/* mkInteger */ = function(_xU/* s1Sf */, _xV/* s1Sg */){
  if(!E(_xU/* s1Sf */)){
    return new F(function(){return _xP/* GHC.Integer.Type.negateInteger */(B(_xJ/* GHC.Integer.Type.mkInteger_f */(_xV/* s1Sg */)));});
  }else{
    return new F(function(){return _xJ/* GHC.Integer.Type.mkInteger_f */(_xV/* s1Sg */);});
  }
},
_xW/* s6TCi */ = 1420103680,
_xX/* s6TCj */ = 465,
_xY/* s6TCk */ = new T2(1,_xX/* s6TCj */,_4/* GHC.Types.[] */),
_xZ/* s6TCl */ = new T2(1,_xW/* s6TCi */,_xY/* s6TCk */),
_y0/* $fHasResolutionE5 */ = new T(function(){
  return B(_xT/* GHC.Integer.Type.mkInteger */(_xe/* GHC.Types.True */, _xZ/* s6TCl */));
}),
_y1/* $wa1 */ = function(_y2/* s3vU */, _/* EXTERNAL */){
  var _y3/* s3vZ */ = __get/* EXTERNAL */(_y2/* s3vU */, 0),
  _y4/* s3w5 */ = __get/* EXTERNAL */(_y2/* s3vU */, 1),
  _y5/* s3w9 */ = Number/* EXTERNAL */(_y3/* s3vZ */),
  _y6/* s3wd */ = jsTrunc/* EXTERNAL */(_y5/* s3w9 */),
  _y7/* s3wh */ = Number/* EXTERNAL */(_y4/* s3w5 */),
  _y8/* s3wl */ = jsTrunc/* EXTERNAL */(_y7/* s3wh */);
  return new T2(0,_y6/* s3wd */,_y8/* s3wl */);
},
_y9/* divInt# */ = function(_ya/* scDT */, _yb/* scDU */){
  if(_ya/* scDT */<=0){
    if(_ya/* scDT */>=0){
      return new F(function(){return quot/* EXTERNAL */(_ya/* scDT */, _yb/* scDU */);});
    }else{
      if(_yb/* scDU */<=0){
        return new F(function(){return quot/* EXTERNAL */(_ya/* scDT */, _yb/* scDU */);});
      }else{
        return quot/* EXTERNAL */(_ya/* scDT */+1|0, _yb/* scDU */)-1|0;
      }
    }
  }else{
    if(_yb/* scDU */>=0){
      if(_ya/* scDT */>=0){
        return new F(function(){return quot/* EXTERNAL */(_ya/* scDT */, _yb/* scDU */);});
      }else{
        if(_yb/* scDU */<=0){
          return new F(function(){return quot/* EXTERNAL */(_ya/* scDT */, _yb/* scDU */);});
        }else{
          return quot/* EXTERNAL */(_ya/* scDT */+1|0, _yb/* scDU */)-1|0;
        }
      }
    }else{
      return quot/* EXTERNAL */(_ya/* scDT */-1|0, _yb/* scDU */)-1|0;
    }
  }
},
_yc/* divInteger */ = function(_yd/* s1Nz */, _ye/* s1NA */){
  while(1){
    var _yf/* s1NB */ = E(_yd/* s1Nz */);
    if(!_yf/* s1NB */._){
      var _yg/* s1ND */ = E(_yf/* s1NB */.a);
      if(_yg/* s1ND */==( -2147483648)){
        _yd/* s1Nz */ = new T1(1,I_fromInt/* EXTERNAL */( -2147483648));
        continue;
      }else{
        var _yh/* s1NE */ = E(_ye/* s1NA */);
        if(!_yh/* s1NE */._){
          return new T1(0,B(_y9/* GHC.Classes.divInt# */(_yg/* s1ND */, _yh/* s1NE */.a)));
        }else{
          _yd/* s1Nz */ = new T1(1,I_fromInt/* EXTERNAL */(_yg/* s1ND */));
          _ye/* s1NA */ = _yh/* s1NE */;
          continue;
        }
      }
    }else{
      var _yi/* s1NO */ = _yf/* s1NB */.a,
      _yj/* s1NP */ = E(_ye/* s1NA */);
      return (_yj/* s1NP */._==0) ? new T1(1,I_div/* EXTERNAL */(_yi/* s1NO */, I_fromInt/* EXTERNAL */(_yj/* s1NP */.a))) : new T1(1,I_div/* EXTERNAL */(_yi/* s1NO */, _yj/* s1NP */.a));
    }
  }
},
_yk/* eqInteger */ = function(_yl/* s1Fo */, _ym/* s1Fp */){
  var _yn/* s1Fq */ = E(_yl/* s1Fo */);
  if(!_yn/* s1Fq */._){
    var _yo/* s1Fr */ = _yn/* s1Fq */.a,
    _yp/* s1Fs */ = E(_ym/* s1Fp */);
    return (_yp/* s1Fs */._==0) ? _yo/* s1Fr */==_yp/* s1Fs */.a : (I_compareInt/* EXTERNAL */(_yp/* s1Fs */.a, _yo/* s1Fr */)==0) ? true : false;
  }else{
    var _yq/* s1Fy */ = _yn/* s1Fq */.a,
    _yr/* s1Fz */ = E(_ym/* s1Fp */);
    return (_yr/* s1Fz */._==0) ? (I_compareInt/* EXTERNAL */(_yq/* s1Fy */, _yr/* s1Fz */.a)==0) ? true : false : (I_compare/* EXTERNAL */(_yq/* s1Fy */, _yr/* s1Fz */.a)==0) ? true : false;
  }
},
_ys/* getCTimeval_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function(){var ms = new Date().getTime();                   return [(ms/1000)|0, ((ms % 1000)*1000)|0];})");
}),
_yt/* slti */ = 660865024,
_yu/* sltj */ = 465661287,
_yv/* sltk */ = new T2(1,_yu/* sltj */,_4/* GHC.Types.[] */),
_yw/* sltl */ = new T2(1,_yt/* slti */,_yv/* sltk */),
_yx/* getPOSIXTime2 */ = new T(function(){
  return B(_xT/* GHC.Integer.Type.mkInteger */(_xe/* GHC.Types.True */, _yw/* sltl */));
}),
_yy/* smallInteger */ = function(_yz/* B1 */){
  return new T1(0,_yz/* B1 */);
},
_yA/* timesInteger */ = function(_yB/* s1PN */, _yC/* s1PO */){
  while(1){
    var _yD/* s1PP */ = E(_yB/* s1PN */);
    if(!_yD/* s1PP */._){
      var _yE/* s1PQ */ = _yD/* s1PP */.a,
      _yF/* s1PR */ = E(_yC/* s1PO */);
      if(!_yF/* s1PR */._){
        var _yG/* s1PS */ = _yF/* s1PR */.a;
        if(!(imul/* EXTERNAL */(_yE/* s1PQ */, _yG/* s1PS */)|0)){
          return new T1(0,imul/* EXTERNAL */(_yE/* s1PQ */, _yG/* s1PS */)|0);
        }else{
          _yB/* s1PN */ = new T1(1,I_fromInt/* EXTERNAL */(_yE/* s1PQ */));
          _yC/* s1PO */ = new T1(1,I_fromInt/* EXTERNAL */(_yG/* s1PS */));
          continue;
        }
      }else{
        _yB/* s1PN */ = new T1(1,I_fromInt/* EXTERNAL */(_yE/* s1PQ */));
        _yC/* s1PO */ = _yF/* s1PR */;
        continue;
      }
    }else{
      var _yH/* s1Q6 */ = E(_yC/* s1PO */);
      if(!_yH/* s1Q6 */._){
        _yB/* s1PN */ = _yD/* s1PP */;
        _yC/* s1PO */ = new T1(1,I_fromInt/* EXTERNAL */(_yH/* s1Q6 */.a));
        continue;
      }else{
        return new T1(1,I_mul/* EXTERNAL */(_yD/* s1PP */.a, _yH/* s1Q6 */.a));
      }
    }
  }
},
_yI/* getPOSIXTime1 */ = function(_/* EXTERNAL */){
  var _yJ/* sltq */ = __app0/* EXTERNAL */(E(_ys/* Data.Time.Clock.CTimeval.getCTimeval_f1 */)),
  _yK/* sltt */ = B(_y1/* Data.Time.Clock.CTimeval.$wa1 */(_yJ/* sltq */, _/* EXTERNAL */));
  return new T(function(){
    var _yL/* sltw */ = E(_yK/* sltt */);
    if(!B(_yk/* GHC.Integer.Type.eqInteger */(_yx/* Data.Time.Clock.POSIX.getPOSIXTime2 */, _xw/* Data.Fixed.$fFractionalFixed1 */))){
      return B(_qJ/* GHC.Integer.Type.plusInteger */(B(_yA/* GHC.Integer.Type.timesInteger */(B(_yy/* GHC.Integer.Type.smallInteger */(E(_yL/* sltw */.a))), _y0/* Data.Fixed.$fHasResolutionE5 */)), B(_yc/* GHC.Integer.Type.divInteger */(B(_yA/* GHC.Integer.Type.timesInteger */(B(_yA/* GHC.Integer.Type.timesInteger */(B(_yy/* GHC.Integer.Type.smallInteger */(E(_yL/* sltw */.b))), _y0/* Data.Fixed.$fHasResolutionE5 */)), _y0/* Data.Fixed.$fHasResolutionE5 */)), _yx/* Data.Time.Clock.POSIX.getPOSIXTime2 */))));
    }else{
      return E(_ft/* GHC.Real.divZeroError */);
    }
  });
},
_yM/* $fBitsWord4 */ = 0,
_yN/* $fBoundedWord32_$cmaxBound */ = 4294967295,
_yO/* $fBoundedWord32 */ = new T2(0,_yM/* GHC.Word.$fBitsWord4 */,_yN/* GHC.Word.$fBoundedWord32_$cmaxBound */),
_yP/* $fEnumRatio1 */ = new T1(0,1),
_yQ/* $p1Integral */ = function(_yR/* sv9T */){
  return E(E(_yR/* sv9T */).a);
},
_yS/* $p1Real */ = function(_yT/* svbu */){
  return E(E(_yT/* svbu */).a);
},
_yU/* fromInteger */ = function(_yV/* s6Go */){
  return E(E(_yV/* s6Go */).g);
},
_yW/* gtInteger */ = function(_yX/* s1G1 */, _yY/* s1G2 */){
  var _yZ/* s1G3 */ = E(_yX/* s1G1 */);
  if(!_yZ/* s1G3 */._){
    var _z0/* s1G4 */ = _yZ/* s1G3 */.a,
    _z1/* s1G5 */ = E(_yY/* s1G2 */);
    return (_z1/* s1G5 */._==0) ? _z0/* s1G4 */>_z1/* s1G5 */.a : I_compareInt/* EXTERNAL */(_z1/* s1G5 */.a, _z0/* s1G4 */)<0;
  }else{
    var _z2/* s1Gc */ = _yZ/* s1G3 */.a,
    _z3/* s1Gd */ = E(_yY/* s1G2 */);
    return (_z3/* s1Gd */._==0) ? I_compareInt/* EXTERNAL */(_z2/* s1Gc */, _z3/* s1Gd */.a)>0 : I_compare/* EXTERNAL */(_z2/* s1Gc */, _z3/* s1Gd */.a)>0;
  }
},
_z4/* maxBound */ = function(_z5/* smih */){
  return E(E(_z5/* smih */).b);
},
_z6/* toInteger */ = function(_z7/* svbj */){
  return E(E(_z7/* svbj */).i);
},
_z8/* integralEnumFrom */ = function(_z9/* svrj */, _za/* svrk */, _zb/* svrl */){
  var _zc/* svro */ = new T(function(){
    return B(_yU/* GHC.Num.fromInteger */(new T(function(){
      return B(_yS/* GHC.Real.$p1Real */(new T(function(){
        return B(_yQ/* GHC.Real.$p1Integral */(_z9/* svrj */));
      })));
    })));
  }),
  _zd/* svrq */ = new T(function(){
    return B(_z4/* GHC.Enum.maxBound */(_za/* svrk */));
  }),
  _ze/* svrr */ = function(_zf/* svrs */){
    return (!B(_yW/* GHC.Integer.Type.gtInteger */(_zf/* svrs */, B(A2(_z6/* GHC.Real.toInteger */,_z9/* svrj */, _zd/* svrq */))))) ? new T2(1,new T(function(){
      return B(A1(_zc/* svro */,_zf/* svrs */));
    }),new T(function(){
      return B(_ze/* svrr */(B(_qJ/* GHC.Integer.Type.plusInteger */(_zf/* svrs */, _yP/* GHC.Real.$fEnumRatio1 */))));
    })) : __Z/* EXTERNAL */;
  };
  return new F(function(){return _ze/* svrr */(B(A2(_z6/* GHC.Real.toInteger */,_z9/* svrj */, _zb/* svrl */)));});
},
_zg/* $fEnumWord32_$cenumFrom */ = function(_zh/* B1 */){
  return new F(function(){return _z8/* GHC.Real.integralEnumFrom */(_zi/* GHC.Word.$fIntegralWord32 */, _yO/* GHC.Word.$fBoundedWord32 */, _zh/* B1 */);});
},
_zj/* $fEnumInteger1 */ = new T1(0,0),
_zk/* ltInteger */ = function(_zl/* s1GH */, _zm/* s1GI */){
  var _zn/* s1GJ */ = E(_zl/* s1GH */);
  if(!_zn/* s1GJ */._){
    var _zo/* s1GK */ = _zn/* s1GJ */.a,
    _zp/* s1GL */ = E(_zm/* s1GI */);
    return (_zp/* s1GL */._==0) ? _zo/* s1GK */<_zp/* s1GL */.a : I_compareInt/* EXTERNAL */(_zp/* s1GL */.a, _zo/* s1GK */)>0;
  }else{
    var _zq/* s1GS */ = _zn/* s1GJ */.a,
    _zr/* s1GT */ = E(_zm/* s1GI */);
    return (_zr/* s1GT */._==0) ? I_compareInt/* EXTERNAL */(_zq/* s1GS */, _zr/* s1GT */.a)<0 : I_compare/* EXTERNAL */(_zq/* s1GS */, _zr/* s1GT */.a)<0;
  }
},
_zs/* up_fb */ = function(_zt/* smjD */, _zu/* smjE */, _zv/* smjF */, _zw/* smjG */, _zx/* smjH */){
  var _zy/* smjI */ = function(_zz/* smjJ */){
    if(!B(_yW/* GHC.Integer.Type.gtInteger */(_zz/* smjJ */, _zx/* smjH */))){
      return new F(function(){return A2(_zt/* smjD */,_zz/* smjJ */, new T(function(){
        return B(_zy/* smjI */(B(_qJ/* GHC.Integer.Type.plusInteger */(_zz/* smjJ */, _zw/* smjG */))));
      }));});
    }else{
      return E(_zu/* smjE */);
    }
  };
  return new F(function(){return _zy/* smjI */(_zv/* smjF */);});
},
_zA/* enumDeltaToIntegerFB */ = function(_zB/* smFL */, _zC/* smFM */, _zD/* smFN */, _zE/* smFO */, _zF/* smFP */){
  if(!B(_qq/* GHC.Integer.Type.geInteger */(_zE/* smFO */, _zj/* GHC.Enum.$fEnumInteger1 */))){
    var _zG/* smFR */ = function(_zH/* smFS */){
      if(!B(_zk/* GHC.Integer.Type.ltInteger */(_zH/* smFS */, _zF/* smFP */))){
        return new F(function(){return A2(_zB/* smFL */,_zH/* smFS */, new T(function(){
          return B(_zG/* smFR */(B(_qJ/* GHC.Integer.Type.plusInteger */(_zH/* smFS */, _zE/* smFO */))));
        }));});
      }else{
        return E(_zC/* smFM */);
      }
    };
    return new F(function(){return _zG/* smFR */(_zD/* smFN */);});
  }else{
    return new F(function(){return _zs/* GHC.Enum.up_fb */(_zB/* smFL */, _zC/* smFM */, _zD/* smFN */, _zE/* smFO */, _zF/* smFP */);});
  }
},
_zI/* minBound */ = function(_zJ/* smid */){
  return E(E(_zJ/* smid */).a);
},
_zK/* minusInteger */ = function(_zL/* s1P0 */, _zM/* s1P1 */){
  while(1){
    var _zN/* s1P2 */ = E(_zL/* s1P0 */);
    if(!_zN/* s1P2 */._){
      var _zO/* s1P3 */ = _zN/* s1P2 */.a,
      _zP/* s1P4 */ = E(_zM/* s1P1 */);
      if(!_zP/* s1P4 */._){
        var _zQ/* s1P5 */ = _zP/* s1P4 */.a,
        _zR/* s1P6 */ = subC/* EXTERNAL */(_zO/* s1P3 */, _zQ/* s1P5 */);
        if(!E(_zR/* s1P6 */.b)){
          return new T1(0,_zR/* s1P6 */.a);
        }else{
          _zL/* s1P0 */ = new T1(1,I_fromInt/* EXTERNAL */(_zO/* s1P3 */));
          _zM/* s1P1 */ = new T1(1,I_fromInt/* EXTERNAL */(_zQ/* s1P5 */));
          continue;
        }
      }else{
        _zL/* s1P0 */ = new T1(1,I_fromInt/* EXTERNAL */(_zO/* s1P3 */));
        _zM/* s1P1 */ = _zP/* s1P4 */;
        continue;
      }
    }else{
      var _zS/* s1Pl */ = E(_zM/* s1P1 */);
      if(!_zS/* s1Pl */._){
        _zL/* s1P0 */ = _zN/* s1P2 */;
        _zM/* s1P1 */ = new T1(1,I_fromInt/* EXTERNAL */(_zS/* s1Pl */.a));
        continue;
      }else{
        return new T1(1,I_sub/* EXTERNAL */(_zN/* s1P2 */.a, _zS/* s1Pl */.a));
      }
    }
  }
},
_zT/* integralEnumFromThen */ = function(_zU/* svry */, _zV/* svrz */, _zW/* svrA */, _zX/* svrB */){
  var _zY/* svrC */ = B(A2(_z6/* GHC.Real.toInteger */,_zU/* svry */, _zX/* svrB */)),
  _zZ/* svrD */ = B(A2(_z6/* GHC.Real.toInteger */,_zU/* svry */, _zW/* svrA */));
  if(!B(_qq/* GHC.Integer.Type.geInteger */(_zY/* svrC */, _zZ/* svrD */))){
    var _A0/* svrH */ = new T(function(){
      return B(_yU/* GHC.Num.fromInteger */(new T(function(){
        return B(_yS/* GHC.Real.$p1Real */(new T(function(){
          return B(_yQ/* GHC.Real.$p1Integral */(_zU/* svry */));
        })));
      })));
    }),
    _A1/* svrL */ = function(_A2/* svrI */, _A3/* svrJ */){
      return new T2(1,new T(function(){
        return B(A1(_A0/* svrH */,_A2/* svrI */));
      }),_A3/* svrJ */);
    };
    return new F(function(){return _zA/* GHC.Enum.enumDeltaToIntegerFB */(_A1/* svrL */, _4/* GHC.Types.[] */, _zZ/* svrD */, B(_zK/* GHC.Integer.Type.minusInteger */(_zY/* svrC */, _zZ/* svrD */)), B(A2(_z6/* GHC.Real.toInteger */,_zU/* svry */, new T(function(){
      return B(_zI/* GHC.Enum.minBound */(_zV/* svrz */));
    }))));});
  }else{
    var _A4/* svrR */ = new T(function(){
      return B(_yU/* GHC.Num.fromInteger */(new T(function(){
        return B(_yS/* GHC.Real.$p1Real */(new T(function(){
          return B(_yQ/* GHC.Real.$p1Integral */(_zU/* svry */));
        })));
      })));
    }),
    _A5/* svrV */ = function(_A6/* svrS */, _A7/* svrT */){
      return new T2(1,new T(function(){
        return B(A1(_A4/* svrR */,_A6/* svrS */));
      }),_A7/* svrT */);
    };
    return new F(function(){return _zA/* GHC.Enum.enumDeltaToIntegerFB */(_A5/* svrV */, _4/* GHC.Types.[] */, _zZ/* svrD */, B(_zK/* GHC.Integer.Type.minusInteger */(_zY/* svrC */, _zZ/* svrD */)), B(A2(_z6/* GHC.Real.toInteger */,_zU/* svry */, new T(function(){
      return B(_z4/* GHC.Enum.maxBound */(_zV/* svrz */));
    }))));});
  }
},
_A8/* $fEnumWord32_$cenumFromThen */ = function(_A9/* B2 */, _zh/* B1 */){
  return new F(function(){return _zT/* GHC.Real.integralEnumFromThen */(_zi/* GHC.Word.$fIntegralWord32 */, _yO/* GHC.Word.$fBoundedWord32 */, _A9/* B2 */, _zh/* B1 */);});
},
_Aa/* integralEnumFromThenTo */ = function(_Ab/* svsd */, _Ac/* svse */, _Ad/* svsf */, _Ae/* svsg */){
  var _Af/* svsh */ = B(A2(_z6/* GHC.Real.toInteger */,_Ab/* svsd */, _Ac/* svse */)),
  _Ag/* svsk */ = new T(function(){
    return B(_yU/* GHC.Num.fromInteger */(new T(function(){
      return B(_yS/* GHC.Real.$p1Real */(new T(function(){
        return B(_yQ/* GHC.Real.$p1Integral */(_Ab/* svsd */));
      })));
    })));
  }),
  _Ah/* svso */ = function(_Ai/* svsl */, _Aj/* svsm */){
    return new T2(1,new T(function(){
      return B(A1(_Ag/* svsk */,_Ai/* svsl */));
    }),_Aj/* svsm */);
  };
  return new F(function(){return _zA/* GHC.Enum.enumDeltaToIntegerFB */(_Ah/* svso */, _4/* GHC.Types.[] */, _Af/* svsh */, B(_zK/* GHC.Integer.Type.minusInteger */(B(A2(_z6/* GHC.Real.toInteger */,_Ab/* svsd */, _Ad/* svsf */)), _Af/* svsh */)), B(A2(_z6/* GHC.Real.toInteger */,_Ab/* svsd */, _Ae/* svsg */)));});
},
_Ak/* $fEnumWord32_$cenumFromThenTo */ = function(_Al/* B3 */, _A9/* B2 */, _zh/* B1 */){
  return new F(function(){return _Aa/* GHC.Real.integralEnumFromThenTo */(_zi/* GHC.Word.$fIntegralWord32 */, _Al/* B3 */, _A9/* B2 */, _zh/* B1 */);});
},
_Am/* integralEnumFromTo */ = function(_An/* svrZ */, _Ao/* svs0 */, _Ap/* svs1 */){
  var _Aq/* svs4 */ = new T(function(){
    return B(_yU/* GHC.Num.fromInteger */(new T(function(){
      return B(_yS/* GHC.Real.$p1Real */(new T(function(){
        return B(_yQ/* GHC.Real.$p1Integral */(_An/* svrZ */));
      })));
    })));
  }),
  _Ar/* svs6 */ = function(_As/* svs7 */){
    return (!B(_yW/* GHC.Integer.Type.gtInteger */(_As/* svs7 */, B(A2(_z6/* GHC.Real.toInteger */,_An/* svrZ */, _Ap/* svs1 */))))) ? new T2(1,new T(function(){
      return B(A1(_Aq/* svs4 */,_As/* svs7 */));
    }),new T(function(){
      return B(_Ar/* svs6 */(B(_qJ/* GHC.Integer.Type.plusInteger */(_As/* svs7 */, _yP/* GHC.Real.$fEnumRatio1 */))));
    })) : __Z/* EXTERNAL */;
  };
  return new F(function(){return _Ar/* svs6 */(B(A2(_z6/* GHC.Real.toInteger */,_An/* svrZ */, _Ao/* svs0 */)));});
},
_At/* $fEnumWord32_$cenumFromTo */ = function(_A9/* B2 */, _zh/* B1 */){
  return new F(function(){return _Am/* GHC.Real.integralEnumFromTo */(_zi/* GHC.Word.$fIntegralWord32 */, _A9/* B2 */, _zh/* B1 */);});
},
_Au/* wordToInteger */ = function(_Av/* s1J7 */){
  return new T1(1,I_fromInt/* EXTERNAL */(_Av/* s1J7 */));
},
_Aw/* $fIntegralWord32_$ctoInteger */ = function(_Ax/* s1RpU */){
  var _Ay/* s1RpV */ = E(_Ax/* s1RpU */),
  _Az/* s1RpX */ = _Ay/* s1RpV */&4294967295;
  if(_Az/* s1RpX */<0){
    return new F(function(){return _Au/* GHC.Integer.Type.wordToInteger */(_Ay/* s1RpV */);});
  }else{
    return new F(function(){return _yy/* GHC.Integer.Type.smallInteger */(_Az/* s1RpX */);});
  }
},
_AA/* integerToJSString */ = function(_AB/* s1Ii */){
  while(1){
    var _AC/* s1Ij */ = E(_AB/* s1Ii */);
    if(!_AC/* s1Ij */._){
      _AB/* s1Ii */ = new T1(1,I_fromInt/* EXTERNAL */(_AC/* s1Ij */.a));
      continue;
    }else{
      return new F(function(){return I_toString/* EXTERNAL */(_AC/* s1Ij */.a);});
    }
  }
},
_AD/* integerToString */ = function(_AE/* sf6p */, _AF/* sf6q */){
  return new F(function(){return _q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(B(_AA/* GHC.Integer.Type.integerToJSString */(_AE/* sf6p */))), _AF/* sf6q */);});
},
_AG/* shows9 */ = new T1(0,0),
_AH/* $w$cshowsPrec1 */ = function(_AI/* sf7E */, _AJ/* sf7F */, _AK/* sf7G */){
  if(_AI/* sf7E */<=6){
    return new F(function(){return _AD/* GHC.Show.integerToString */(_AJ/* sf7F */, _AK/* sf7G */);});
  }else{
    if(!B(_zk/* GHC.Integer.Type.ltInteger */(_AJ/* sf7F */, _AG/* GHC.Show.shows9 */))){
      return new F(function(){return _AD/* GHC.Show.integerToString */(_AJ/* sf7F */, _AK/* sf7G */);});
    }else{
      return new T2(1,_5U/* GHC.Show.shows8 */,new T(function(){
        return B(_q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(B(_AA/* GHC.Integer.Type.integerToJSString */(_AJ/* sf7F */))), new T2(1,_5T/* GHC.Show.shows7 */,_AK/* sf7G */)));
      }));
    }
  }
},
_AL/* $fShowWord32_$cshow */ = function(_AM/* s1RrO */){
  return new F(function(){return _AH/* GHC.Show.$w$cshowsPrec1 */(0, B(_Aw/* GHC.Word.$fIntegralWord32_$ctoInteger */(_AM/* s1RrO */)), _4/* GHC.Types.[] */);});
},
_AN/* $fShowWord2 */ = function(_AO/* s1RrD */, _AP/* s1RrE */){
  var _AQ/* s1RrF */ = E(_AO/* s1RrD */),
  _AR/* s1RrH */ = _AQ/* s1RrF */&4294967295;
  if(_AR/* s1RrH */<0){
    return new F(function(){return _AH/* GHC.Show.$w$cshowsPrec1 */(0, B(_Au/* GHC.Integer.Type.wordToInteger */(_AQ/* s1RrF */)), _AP/* s1RrE */);});
  }else{
    return new F(function(){return _AH/* GHC.Show.$w$cshowsPrec1 */(0, B(_yy/* GHC.Integer.Type.smallInteger */(_AR/* s1RrH */)), _AP/* s1RrE */);});
  }
},
_AS/* $fShowWord32_$cshowList */ = function(_AT/* s1RrM */, _AU/* s1RrN */){
  return new F(function(){return _A/* GHC.Show.showList__ */(_AN/* GHC.Word.$fShowWord2 */, _AT/* s1RrM */, _AU/* s1RrN */);});
},
_AV/* $fShowWord32_$cshowsPrec */ = function(_AW/* s1Rrr */, _AX/* s1Rrs */){
  var _AY/* s1Rrt */ = new T(function(){
    var _AZ/* s1Rru */ = E(_AX/* s1Rrs */),
    _B0/* s1Rrw */ = _AZ/* s1Rru */&4294967295;
    if(_B0/* s1Rrw */<0){
      return B(_Au/* GHC.Integer.Type.wordToInteger */(_AZ/* s1Rru */));
    }else{
      return B(_yy/* GHC.Integer.Type.smallInteger */(_B0/* s1Rrw */));
    }
  });
  return function(_B1/* s1Rrz */){
    return new F(function(){return _AH/* GHC.Show.$w$cshowsPrec1 */(E(_AW/* s1Rrr */), _AY/* s1Rrt */, _B1/* s1Rrz */);});
  };
},
_B2/* $fShowWord32 */ = new T3(0,_AV/* GHC.Word.$fShowWord32_$cshowsPrec */,_AL/* GHC.Word.$fShowWord32_$cshow */,_AS/* GHC.Word.$fShowWord32_$cshowList */),
_B3/* lvl */ = new T2(1,_5T/* GHC.Show.shows7 */,_4/* GHC.Types.[] */),
_B4/* $fShow(,)1 */ = function(_B5/* sfbb */, _B6/* sfbc */, _B7/* sfbd */){
  return new F(function(){return A1(_B5/* sfbb */,new T2(1,_x/* GHC.Show.showList__1 */,new T(function(){
    return B(A1(_B6/* sfbc */,_B7/* sfbd */));
  })));});
},
_B8/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */(": empty list"));
}),
_B9/* errorEmptyList */ = function(_Ba/* sbDG */){
  return new F(function(){return err/* EXTERNAL */(B(_q/* GHC.Base.++ */(_pB/* GHC.List.prel_list_str */, new T(function(){
    return B(_q/* GHC.Base.++ */(_Ba/* sbDG */, _B8/* GHC.List.lvl */));
  },1))));});
},
_Bb/* lvl7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("foldr1"));
}),
_Bc/* lvl8 */ = new T(function(){
  return B(_B9/* GHC.List.errorEmptyList */(_Bb/* GHC.List.lvl7 */));
}),
_Bd/* foldr1 */ = function(_Be/* sbKQ */, _Bf/* sbKR */){
  var _Bg/* sbKS */ = E(_Bf/* sbKR */);
  if(!_Bg/* sbKS */._){
    return E(_Bc/* GHC.List.lvl8 */);
  }else{
    var _Bh/* sbKT */ = _Bg/* sbKS */.a,
    _Bi/* sbKV */ = E(_Bg/* sbKS */.b);
    if(!_Bi/* sbKV */._){
      return E(_Bh/* sbKT */);
    }else{
      return new F(function(){return A2(_Be/* sbKQ */,_Bh/* sbKT */, new T(function(){
        return B(_Bd/* GHC.List.foldr1 */(_Be/* sbKQ */, _Bi/* sbKV */));
      }));});
    }
  }
},
_Bj/* lvl14 */ = function(_Bk/* smzT */){
  return new F(function(){return _5V/* GHC.Show.$wshowSignedInt */(0,  -2147483648, _Bk/* smzT */);});
},
_Bl/* lvl15 */ = function(_Bm/* smzU */){
  return new F(function(){return _5V/* GHC.Show.$wshowSignedInt */(0, 2147483647, _Bm/* smzU */);});
},
_Bn/* lvl16 */ = new T2(1,_Bl/* GHC.Enum.lvl15 */,_4/* GHC.Types.[] */),
_Bo/* lvl17 */ = new T2(1,_Bj/* GHC.Enum.lvl14 */,_Bn/* GHC.Enum.lvl16 */),
_Bp/* lvl18 */ = new T(function(){
  return B(_Bd/* GHC.List.foldr1 */(_B4/* GHC.Show.$fShow(,)1 */, _Bo/* GHC.Enum.lvl17 */));
}),
_Bq/* lvl19 */ = new T(function(){
  return B(A1(_Bp/* GHC.Enum.lvl18 */,_B3/* GHC.Enum.lvl */));
}),
_Br/* lvl20 */ = new T2(1,_5U/* GHC.Show.shows8 */,_Bq/* GHC.Enum.lvl19 */),
_Bs/* lvl21 */ = new T(function(){
  return B(unAppCStr/* EXTERNAL */(") is outside of Int\'s bounds ", _Br/* GHC.Enum.lvl20 */));
}),
_Bt/* show */ = function(_Bu/* sf6a */){
  return E(E(_Bu/* sf6a */).b);
},
_Bv/* lvl22 */ = function(_Bw/* smzV */, _Bx/* smzW */, _By/* smzX */){
  var _Bz/* smA1 */ = new T(function(){
    var _BA/* smA0 */ = new T(function(){
      return B(unAppCStr/* EXTERNAL */("}: value (", new T(function(){
        return B(_q/* GHC.Base.++ */(B(A2(_Bt/* GHC.Show.show */,_By/* smzX */, _Bx/* smzW */)), _Bs/* GHC.Enum.lvl21 */));
      })));
    },1);
    return B(_q/* GHC.Base.++ */(_Bw/* smzV */, _BA/* smA0 */));
  });
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("Enum.fromEnum{", _Bz/* smA1 */)));});
},
_BB/* fromEnumError */ = function(_BC/* smA3 */, _BD/* smA4 */, _BE/* smA5 */){
  return new F(function(){return _Bv/* GHC.Enum.lvl22 */(_BD/* smA4 */, _BE/* smA5 */, _BC/* smA3 */);});
},
_BF/* lvl4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Word32"));
}),
_BG/* lvl10 */ = function(_BH/* s1RrQ */){
  return new F(function(){return _BB/* GHC.Enum.fromEnumError */(_B2/* GHC.Word.$fShowWord32 */, _BF/* GHC.Word.lvl4 */, _BH/* s1RrQ */);});
},
_BI/* $fEnumWord32_$cfromEnum */ = function(_BJ/* s1RMa */){
  var _BK/* s1RMb */ = E(_BJ/* s1RMa */);
  if(_BK/* s1RMb */>2147483647){
    return new F(function(){return _BG/* GHC.Word.lvl10 */(_BK/* s1RMb */);});
  }else{
    return _BK/* s1RMb */&4294967295;
  }
},
_BL/* lvl1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("}: tried to take `pred\' of minBound"));
}),
_BM/* lvl2 */ = function(_BN/* smnl */){
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("Enum.pred{", new T(function(){
    return B(_q/* GHC.Base.++ */(_BN/* smnl */, _BL/* GHC.Enum.lvl1 */));
  }))));});
},
_BO/* predError */ = function(_BP/* B1 */){
  return new F(function(){return _BM/* GHC.Enum.lvl2 */(_BP/* B1 */);});
},
_BQ/* $fEnumWord7 */ = new T(function(){
  return B(_BO/* GHC.Enum.predError */(_BF/* GHC.Word.lvl4 */));
}),
_BR/* $fEnumWord32_$cpred */ = function(_BS/* s1RNu */){
  var _BT/* s1RNx */ = E(_BS/* s1RNu */);
  return (_BT/* s1RNx */==0) ? E(_BQ/* GHC.Word.$fEnumWord7 */) : _BT/* s1RNx */-1>>>0;
},
_BU/* lvl3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("}: tried to take `succ\' of maxBound"));
}),
_BV/* lvl4 */ = function(_BW/* smno */){
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("Enum.succ{", new T(function(){
    return B(_q/* GHC.Base.++ */(_BW/* smno */, _BU/* GHC.Enum.lvl3 */));
  }))));});
},
_BX/* succError */ = function(_BP/* B1 */){
  return new F(function(){return _BV/* GHC.Enum.lvl4 */(_BP/* B1 */);});
},
_BY/* $fEnumWord9 */ = new T(function(){
  return B(_BX/* GHC.Enum.succError */(_BF/* GHC.Word.lvl4 */));
}),
_BZ/* $fEnumWord32_$csucc */ = function(_C0/* s1RNp */){
  var _C1/* s1RNs */ = E(_C0/* s1RNp */);
  return (_C1/* s1RNs */==4294967295) ? E(_BY/* GHC.Word.$fEnumWord9 */) : _C1/* s1RNs */+1>>>0;
},
_C2/* lvl12 */ = new T2(0,_yM/* GHC.Word.$fBitsWord4 */,_yN/* GHC.Word.$fBoundedWord32_$cmaxBound */),
_C3/* shows14 */ = 0,
_C4/* showsPrec */ = function(_C5/* sf65 */){
  return E(E(_C5/* sf65 */).a);
},
_C6/* lvl5 */ = function(_C7/* smnr */, _C8/* smns */, _C9/* smnt */, _Ca/* smnu */){
  var _Cb/* smnK */ = new T(function(){
    var _Cc/* smnJ */ = new T(function(){
      var _Cd/* smnI */ = new T(function(){
        var _Ce/* smnH */ = new T(function(){
          var _Cf/* smnG */ = new T(function(){
            var _Cg/* smny */ = E(_C9/* smnt */),
            _Ch/* smnF */ = new T(function(){
              return B(A3(_Bd/* GHC.List.foldr1 */,_B4/* GHC.Show.$fShow(,)1 */, new T2(1,new T(function(){
                return B(A3(_C4/* GHC.Show.showsPrec */,_Ca/* smnu */, _C3/* GHC.Show.shows14 */, _Cg/* smny */.a));
              }),new T2(1,new T(function(){
                return B(A3(_C4/* GHC.Show.showsPrec */,_Ca/* smnu */, _C3/* GHC.Show.shows14 */, _Cg/* smny */.b));
              }),_4/* GHC.Types.[] */)), _B3/* GHC.Enum.lvl */));
            });
            return new T2(1,_5U/* GHC.Show.shows8 */,_Ch/* smnF */);
          });
          return B(unAppCStr/* EXTERNAL */(") is outside of bounds ", _Cf/* smnG */));
        },1);
        return B(_q/* GHC.Base.++ */(B(_5V/* GHC.Show.$wshowSignedInt */(0, E(_C8/* smns */), _4/* GHC.Types.[] */)), _Ce/* smnH */));
      });
      return B(unAppCStr/* EXTERNAL */("}: tag (", _Cd/* smnI */));
    },1);
    return B(_q/* GHC.Base.++ */(_C7/* smnr */, _Cc/* smnJ */));
  });
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("Enum.toEnum{", _Cb/* smnK */)));});
},
_Ci/* toEnumError */ = function(_Cj/* smnM */, _Ck/* smnN */, _Cl/* smnO */, _Cm/* smnP */){
  return new F(function(){return _C6/* GHC.Enum.lvl5 */(_Ck/* smnN */, _Cl/* smnO */, _Cm/* smnP */, _Cj/* smnM */);});
},
_Cn/* $fEnumWord6 */ = function(_Co/* s1Rs9 */){
  return new F(function(){return _Ci/* GHC.Enum.toEnumError */(_B2/* GHC.Word.$fShowWord32 */, _BF/* GHC.Word.lvl4 */, _Co/* s1Rs9 */, _C2/* GHC.Word.lvl12 */);});
},
_Cp/* $fEnumWord32_$ctoEnum */ = function(_Cq/* s1Rse */){
  var _Cr/* s1Rsf */ = E(_Cq/* s1Rse */);
  if(_Cr/* s1Rsf */<0){
    return new F(function(){return _Cn/* GHC.Word.$fEnumWord6 */(_Cr/* s1Rsf */);});
  }else{
    return _Cr/* s1Rsf */>>>0;
  }
},
_Cs/* $fEnumWord32 */ = new T(function(){
  return {_:0,a:_BZ/* GHC.Word.$fEnumWord32_$csucc */,b:_BR/* GHC.Word.$fEnumWord32_$cpred */,c:_Cp/* GHC.Word.$fEnumWord32_$ctoEnum */,d:_BI/* GHC.Word.$fEnumWord32_$cfromEnum */,e:_zg/* GHC.Word.$fEnumWord32_$cenumFrom */,f:_A8/* GHC.Word.$fEnumWord32_$cenumFromThen */,g:_At/* GHC.Word.$fEnumWord32_$cenumFromTo */,h:_Ak/* GHC.Word.$fEnumWord32_$cenumFromThenTo */};
}),
_Ct/* $fIntegralWord32_$cdivMod */ = function(_Cu/* s1RNe */, _Cv/* s1RNf */){
  var _Cw/* s1RNg */ = E(_Cu/* s1RNe */),
  _Cx/* s1RNk */ = E(_Cv/* s1RNf */);
  return (_Cx/* s1RNk */==0) ? E(_ft/* GHC.Real.divZeroError */) : new T2(0,new T(function(){
    return quot/* EXTERNAL */(_Cw/* s1RNg */, _Cx/* s1RNk */);
  }),new T(function(){
    return _Cw/* s1RNg */%_Cx/* s1RNk */;
  }));
},
_Cy/* $fIntegralWord32_$cquot */ = function(_Cz/* s1RMM */, _CA/* s1RMN */){
  var _CB/* s1RMS */ = E(_CA/* s1RMN */);
  if(!_CB/* s1RMS */){
    return E(_ft/* GHC.Real.divZeroError */);
  }else{
    return new F(function(){return quot/* EXTERNAL */(E(_Cz/* s1RMM */), _CB/* s1RMS */);});
  }
},
_CC/* $fIntegralWord32_$cquotRem */ = function(_CD/* s1RN2 */, _CE/* s1RN3 */){
  var _CF/* s1RN8 */ = E(_CE/* s1RN3 */);
  if(!_CF/* s1RN8 */){
    return E(_ft/* GHC.Real.divZeroError */);
  }else{
    var _CG/* s1RN9 */ = quotRemI/* EXTERNAL */(E(_CD/* s1RN2 */), _CF/* s1RN8 */);
    return new T2(0,_CG/* s1RN9 */.a,_CG/* s1RN9 */.b);
  }
},
_CH/* $fIntegralWord32_$crem */ = function(_CI/* s1RMU */, _CJ/* s1RMV */){
  var _CK/* s1RN0 */ = E(_CJ/* s1RMV */);
  return (_CK/* s1RN0 */==0) ? E(_ft/* GHC.Real.divZeroError */) : E(_CI/* s1RMU */)%_CK/* s1RN0 */;
},
_CL/* integer2Word# */ = function(_CM/* s2C */){
  return I_toInt/* EXTERNAL */(_CM/* s2C */)>>>0;
},
_CN/* integerToWord */ = function(_CO/* s1Rr */){
  var _CP/* s1Rs */ = E(_CO/* s1Rr */);
  if(!_CP/* s1Rs */._){
    return _CP/* s1Rs */.a>>>0;
  }else{
    return new F(function(){return _CL/* GHC.Integer.GMP.Prim.integer2Word# */(_CP/* s1Rs */.a);});
  }
},
_CQ/* $cfromInteger2 */ = function(_CR/* s1Rpq */){
  return new F(function(){return _CN/* GHC.Integer.Type.integerToWord */(_CR/* s1Rpq */);});
},
_CS/* $fNumWord32_$c* */ = function(_CT/* s1Rpz */, _CU/* s1RpA */){
  return imul/* EXTERNAL */(E(_CT/* s1Rpz */), E(_CU/* s1RpA */))>>>0;
},
_CV/* $fNumWord32_$c+ */ = function(_CW/* s1RpN */, _CX/* s1RpO */){
  return E(_CW/* s1RpN */)+E(_CX/* s1RpO */)>>>0;
},
_CY/* $fNumWord32_$c- */ = function(_CZ/* s1RpG */, _D0/* s1RpH */){
  return E(_CZ/* s1RpG */)-E(_D0/* s1RpH */)>>>0;
},
_D1/* $fNumWord32_$cabs */ = function(_D2/* s1Rps */){
  return E(_D2/* s1Rps */);
},
_D3/* $fNumWord32_$cnegate */ = function(_D4/* s1Rpt */){
  return  -(E(_D4/* s1Rpt */)&4294967295)>>>0;
},
_D5/* $fNumWord2 */ = 1,
_D6/* $fNumWord32_$csignum */ = function(_D7/* s1RNz */){
  return (E(_D7/* s1RNz */)==0) ? E(_yM/* GHC.Word.$fBitsWord4 */) : E(_D5/* GHC.Word.$fNumWord2 */);
},
_D8/* $fNumWord32 */ = {_:0,a:_CV/* GHC.Word.$fNumWord32_$c+ */,b:_CY/* GHC.Word.$fNumWord32_$c- */,c:_CS/* GHC.Word.$fNumWord32_$c* */,d:_D3/* GHC.Word.$fNumWord32_$cnegate */,e:_D1/* GHC.Word.$fNumWord32_$cabs */,f:_D6/* GHC.Word.$fNumWord32_$csignum */,g:_CQ/* GHC.Word.$cfromInteger2 */},
_D9/* $fBitsWord32_$c/= */ = function(_Da/* s1RME */, _Db/* s1RMF */){
  return (E(_Da/* s1RME */)!=E(_Db/* s1RMF */)) ? true : false;
},
_Dc/* $fEqWord32_$c== */ = function(_Dd/* s1RMx */, _De/* s1RMy */){
  return E(_Dd/* s1RMx */)==E(_De/* s1RMy */);
},
_Df/* $fEqWord32 */ = new T2(0,_Dc/* GHC.Word.$fEqWord32_$c== */,_D9/* GHC.Word.$fBitsWord32_$c/= */),
_Dg/* $fOrdWord32_$c< */ = function(_Dh/* s1RMg */, _Di/* s1RMh */){
  return E(_Dh/* s1RMg */)<E(_Di/* s1RMh */);
},
_Dj/* $fOrdWord32_$c<= */ = function(_Dk/* s1RLP */, _Dl/* s1RLQ */){
  return E(_Dk/* s1RLP */)<=E(_Dl/* s1RLQ */);
},
_Dm/* $fOrdWord32_$c> */ = function(_Dn/* s1RLI */, _Do/* s1RLJ */){
  return E(_Dn/* s1RLI */)>E(_Do/* s1RLJ */);
},
_Dp/* $fOrdWord32_$c>= */ = function(_Dq/* s1RLB */, _Dr/* s1RLC */){
  return E(_Dq/* s1RLB */)>=E(_Dr/* s1RLC */);
},
_Ds/* $fOrdWord32_$ccompare */ = function(_Dt/* s1RMn */, _Du/* s1RMo */){
  var _Dv/* s1RMp */ = E(_Dt/* s1RMn */),
  _Dw/* s1RMr */ = E(_Du/* s1RMo */);
  return (_Dv/* s1RMp */>=_Dw/* s1RMr */) ? (_Dv/* s1RMp */!=_Dw/* s1RMr */) ? 2 : 1 : 0;
},
_Dx/* $fOrdWord32_$cmax */ = function(_Dy/* s1ROa */, _Dz/* s1ROb */){
  var _DA/* s1ROc */ = E(_Dy/* s1ROa */),
  _DB/* s1ROe */ = E(_Dz/* s1ROb */);
  return (_DA/* s1ROc */>_DB/* s1ROe */) ? E(_DA/* s1ROc */) : E(_DB/* s1ROe */);
},
_DC/* $fOrdWord32_$cmin */ = function(_DD/* s1RO2 */, _DE/* s1RO3 */){
  var _DF/* s1RO4 */ = E(_DD/* s1RO2 */),
  _DG/* s1RO6 */ = E(_DE/* s1RO3 */);
  return (_DF/* s1RO4 */>_DG/* s1RO6 */) ? E(_DG/* s1RO6 */) : E(_DF/* s1RO4 */);
},
_DH/* $fOrdWord32 */ = {_:0,a:_Df/* GHC.Word.$fEqWord32 */,b:_Ds/* GHC.Word.$fOrdWord32_$ccompare */,c:_Dg/* GHC.Word.$fOrdWord32_$c< */,d:_Dj/* GHC.Word.$fOrdWord32_$c<= */,e:_Dm/* GHC.Word.$fOrdWord32_$c> */,f:_Dp/* GHC.Word.$fOrdWord32_$c>= */,g:_Dx/* GHC.Word.$fOrdWord32_$cmax */,h:_DC/* GHC.Word.$fOrdWord32_$cmin */},
_DI/* $fRealWord1 */ = new T1(0,1),
_DJ/* even1 */ = new T1(0,0),
_DK/* remInteger */ = function(_DL/* s1NY */, _DM/* s1NZ */){
  while(1){
    var _DN/* s1O0 */ = E(_DL/* s1NY */);
    if(!_DN/* s1O0 */._){
      var _DO/* s1O2 */ = E(_DN/* s1O0 */.a);
      if(_DO/* s1O2 */==( -2147483648)){
        _DL/* s1NY */ = new T1(1,I_fromInt/* EXTERNAL */( -2147483648));
        continue;
      }else{
        var _DP/* s1O3 */ = E(_DM/* s1NZ */);
        if(!_DP/* s1O3 */._){
          return new T1(0,_DO/* s1O2 */%_DP/* s1O3 */.a);
        }else{
          _DL/* s1NY */ = new T1(1,I_fromInt/* EXTERNAL */(_DO/* s1O2 */));
          _DM/* s1NZ */ = _DP/* s1O3 */;
          continue;
        }
      }
    }else{
      var _DQ/* s1Od */ = _DN/* s1O0 */.a,
      _DR/* s1Oe */ = E(_DM/* s1NZ */);
      return (_DR/* s1Oe */._==0) ? new T1(1,I_rem/* EXTERNAL */(_DQ/* s1Od */, I_fromInt/* EXTERNAL */(_DR/* s1Oe */.a))) : new T1(1,I_rem/* EXTERNAL */(_DQ/* s1Od */, _DR/* s1Oe */.a));
    }
  }
},
_DS/* $fIntegralInteger_$crem */ = function(_DT/* svkU */, _DU/* svkV */){
  if(!B(_yk/* GHC.Integer.Type.eqInteger */(_DU/* svkV */, _DJ/* GHC.Real.even1 */))){
    return new F(function(){return _DK/* GHC.Integer.Type.remInteger */(_DT/* svkU */, _DU/* svkV */);});
  }else{
    return E(_ft/* GHC.Real.divZeroError */);
  }
},
_DV/* $fEnumRatio_gcd' */ = function(_DW/* svl0 */, _DX/* svl1 */){
  while(1){
    if(!B(_yk/* GHC.Integer.Type.eqInteger */(_DX/* svl1 */, _DJ/* GHC.Real.even1 */))){
      var _DY/*  svl0 */ = _DX/* svl1 */,
      _DZ/*  svl1 */ = B(_DS/* GHC.Real.$fIntegralInteger_$crem */(_DW/* svl0 */, _DX/* svl1 */));
      _DW/* svl0 */ = _DY/*  svl0 */;
      _DX/* svl1 */ = _DZ/*  svl1 */;
      continue;
    }else{
      return E(_DW/* svl0 */);
    }
  }
},
_E0/* absInteger */ = function(_E1/* s1QP */){
  var _E2/* s1QQ */ = E(_E1/* s1QP */);
  if(!_E2/* s1QQ */._){
    var _E3/* s1QS */ = E(_E2/* s1QQ */.a);
    return (_E3/* s1QS */==( -2147483648)) ? E(_xO/* GHC.Integer.Type.lvl3 */) : (_E3/* s1QS */<0) ? new T1(0, -_E3/* s1QS */) : E(_E2/* s1QQ */);
  }else{
    var _E4/* s1QW */ = _E2/* s1QQ */.a;
    return (I_compareInt/* EXTERNAL */(_E4/* s1QW */, 0)>=0) ? E(_E2/* s1QQ */) : new T1(1,I_negate/* EXTERNAL */(_E4/* s1QW */));
  }
},
_E5/* quotInteger */ = function(_E6/* s1On */, _E7/* s1Oo */){
  while(1){
    var _E8/* s1Op */ = E(_E6/* s1On */);
    if(!_E8/* s1Op */._){
      var _E9/* s1Or */ = E(_E8/* s1Op */.a);
      if(_E9/* s1Or */==( -2147483648)){
        _E6/* s1On */ = new T1(1,I_fromInt/* EXTERNAL */( -2147483648));
        continue;
      }else{
        var _Ea/* s1Os */ = E(_E7/* s1Oo */);
        if(!_Ea/* s1Os */._){
          return new T1(0,quot/* EXTERNAL */(_E9/* s1Or */, _Ea/* s1Os */.a));
        }else{
          _E6/* s1On */ = new T1(1,I_fromInt/* EXTERNAL */(_E9/* s1Or */));
          _E7/* s1Oo */ = _Ea/* s1Os */;
          continue;
        }
      }
    }else{
      var _Eb/* s1OC */ = _E8/* s1Op */.a,
      _Ec/* s1OD */ = E(_E7/* s1Oo */);
      return (_Ec/* s1OD */._==0) ? new T1(0,I_toInt/* EXTERNAL */(I_quot/* EXTERNAL */(_Eb/* s1OC */, I_fromInt/* EXTERNAL */(_Ec/* s1OD */.a)))) : new T1(1,I_quot/* EXTERNAL */(_Eb/* s1OC */, _Ec/* s1OD */.a));
    }
  }
},
_Ed/* RatioZeroDenominator */ = 5,
_Ee/* ratioZeroDenomException */ = new T(function(){
  return B(_fq/* GHC.Exception.$fExceptionArithException_$ctoException */(_Ed/* GHC.Exception.RatioZeroDenominator */));
}),
_Ef/* ratioZeroDenominatorError */ = new T(function(){
  return die/* EXTERNAL */(_Ee/* GHC.Exception.ratioZeroDenomException */);
}),
_Eg/* $w$sreduce */ = function(_Eh/* svlj */, _Ei/* svlk */){
  if(!B(_yk/* GHC.Integer.Type.eqInteger */(_Ei/* svlk */, _DJ/* GHC.Real.even1 */))){
    var _Ej/* svlm */ = B(_DV/* GHC.Real.$fEnumRatio_gcd' */(B(_E0/* GHC.Integer.Type.absInteger */(_Eh/* svlj */)), B(_E0/* GHC.Integer.Type.absInteger */(_Ei/* svlk */))));
    return (!B(_yk/* GHC.Integer.Type.eqInteger */(_Ej/* svlm */, _DJ/* GHC.Real.even1 */))) ? new T2(0,B(_E5/* GHC.Integer.Type.quotInteger */(_Eh/* svlj */, _Ej/* svlm */)),B(_E5/* GHC.Integer.Type.quotInteger */(_Ei/* svlk */, _Ej/* svlm */))) : E(_ft/* GHC.Real.divZeroError */);
  }else{
    return E(_Ef/* GHC.Real.ratioZeroDenominatorError */);
  }
},
_Ek/* $w$ctoRational */ = function(_El/* s1RrR */){
  var _Em/* s1RrS */ = _El/* s1RrR */&4294967295;
  if(_Em/* s1RrS */<0){
    return new F(function(){return _Eg/* GHC.Real.$w$sreduce */(B(_yA/* GHC.Integer.Type.timesInteger */(B(_Au/* GHC.Integer.Type.wordToInteger */(_El/* s1RrR */)), _DI/* GHC.Word.$fRealWord1 */)), _DI/* GHC.Word.$fRealWord1 */);});
  }else{
    return new F(function(){return _Eg/* GHC.Real.$w$sreduce */(B(_yA/* GHC.Integer.Type.timesInteger */(B(_yy/* GHC.Integer.Type.smallInteger */(_Em/* s1RrS */)), _DI/* GHC.Word.$fRealWord1 */)), _DI/* GHC.Word.$fRealWord1 */);});
  }
},
_En/* $fRealWord32_$ctoRational */ = function(_Eo/* s1RrZ */){
  var _Ep/* s1Rs2 */ = B(_Ek/* GHC.Word.$w$ctoRational */(E(_Eo/* s1RrZ */)));
  return new T2(0,E(_Ep/* s1Rs2 */.a),E(_Ep/* s1Rs2 */.b));
},
_Eq/* $fRealWord32 */ = new T3(0,_D8/* GHC.Word.$fNumWord32 */,_DH/* GHC.Word.$fOrdWord32 */,_En/* GHC.Word.$fRealWord32_$ctoRational */),
_zi/* $fIntegralWord32 */ = new T(function(){
  return {_:0,a:_Eq/* GHC.Word.$fRealWord32 */,b:_Cs/* GHC.Word.$fEnumWord32 */,c:_Cy/* GHC.Word.$fIntegralWord32_$cquot */,d:_CH/* GHC.Word.$fIntegralWord32_$crem */,e:_Cy/* GHC.Word.$fIntegralWord32_$cquot */,f:_CH/* GHC.Word.$fIntegralWord32_$crem */,g:_CC/* GHC.Word.$fIntegralWord32_$cquotRem */,h:_Ct/* GHC.Word.$fIntegralWord32_$cdivMod */,i:_Aw/* GHC.Word.$fIntegralWord32_$ctoInteger */};
}),
_Er/* lvl1 */ = new T1(0, -1),
_Es/* signumInteger */ = function(_Et/* s1OO */){
  var _Eu/* s1OP */ = E(_Et/* s1OO */);
  if(!_Eu/* s1OP */._){
    var _Ev/* s1OQ */ = _Eu/* s1OP */.a;
    return (_Ev/* s1OQ */>=0) ? (E(_Ev/* s1OQ */)==0) ? E(_xx/* GHC.Integer.Type.lvl */) : E(_xM/* GHC.Integer.Type.log2I1 */) : E(_Er/* GHC.Integer.Type.lvl1 */);
  }else{
    var _Ew/* s1OW */ = I_compareInt/* EXTERNAL */(_Eu/* s1OP */.a, 0);
    return (_Ew/* s1OW */<=0) ? (E(_Ew/* s1OW */)==0) ? E(_xx/* GHC.Integer.Type.lvl */) : E(_Er/* GHC.Integer.Type.lvl1 */) : E(_xM/* GHC.Integer.Type.log2I1 */);
  }
},
_Ex/* $w$s$c/ */ = function(_Ey/* svlJ */, _Ez/* svlK */, _EA/* svlL */, _EB/* svlM */){
  var _EC/* svlN */ = B(_yA/* GHC.Integer.Type.timesInteger */(_Ez/* svlK */, _EA/* svlL */));
  return new F(function(){return _Eg/* GHC.Real.$w$sreduce */(B(_yA/* GHC.Integer.Type.timesInteger */(B(_yA/* GHC.Integer.Type.timesInteger */(_Ey/* svlJ */, _EB/* svlM */)), B(_Es/* GHC.Integer.Type.signumInteger */(_EC/* svlN */)))), B(_E0/* GHC.Integer.Type.absInteger */(_EC/* svlN */)));});
},
_ED/* quotRemInteger */ = function(_EE/* s1Ma */, _EF/* s1Mb */){
  while(1){
    var _EG/* s1Mc */ = E(_EE/* s1Ma */);
    if(!_EG/* s1Mc */._){
      var _EH/* s1Me */ = E(_EG/* s1Mc */.a);
      if(_EH/* s1Me */==( -2147483648)){
        _EE/* s1Ma */ = new T1(1,I_fromInt/* EXTERNAL */( -2147483648));
        continue;
      }else{
        var _EI/* s1Mf */ = E(_EF/* s1Mb */);
        if(!_EI/* s1Mf */._){
          var _EJ/* s1Mg */ = _EI/* s1Mf */.a;
          return new T2(0,new T1(0,quot/* EXTERNAL */(_EH/* s1Me */, _EJ/* s1Mg */)),new T1(0,_EH/* s1Me */%_EJ/* s1Mg */));
        }else{
          _EE/* s1Ma */ = new T1(1,I_fromInt/* EXTERNAL */(_EH/* s1Me */));
          _EF/* s1Mb */ = _EI/* s1Mf */;
          continue;
        }
      }
    }else{
      var _EK/* s1Mt */ = E(_EF/* s1Mb */);
      if(!_EK/* s1Mt */._){
        _EE/* s1Ma */ = _EG/* s1Mc */;
        _EF/* s1Mb */ = new T1(1,I_fromInt/* EXTERNAL */(_EK/* s1Mt */.a));
        continue;
      }else{
        var _EL/* s1MA */ = I_quotRem/* EXTERNAL */(_EG/* s1Mc */.a, _EK/* s1Mt */.a);
        return new T2(0,new T1(1,_EL/* s1MA */.a),new T1(1,_EL/* s1MA */.b));
      }
    }
  }
},
_EM/* $w$s$cproperFraction */ = function(_EN/* svzS */, _EO/* svzT */, _EP/* svzU */){
  var _EQ/* svzV */ = new T(function(){
    if(!B(_yk/* GHC.Integer.Type.eqInteger */(_EP/* svzU */, _DJ/* GHC.Real.even1 */))){
      var _ER/* svzX */ = B(_ED/* GHC.Integer.Type.quotRemInteger */(_EO/* svzT */, _EP/* svzU */));
      return new T2(0,_ER/* svzX */.a,_ER/* svzX */.b);
    }else{
      return E(_ft/* GHC.Real.divZeroError */);
    }
  }),
  _ES/* svA6 */ = new T(function(){
    return B(A2(_yU/* GHC.Num.fromInteger */,B(_yS/* GHC.Real.$p1Real */(B(_yQ/* GHC.Real.$p1Integral */(_EN/* svzS */)))), new T(function(){
      return E(E(_EQ/* svzV */).a);
    })));
  });
  return new T2(0,_ES/* svA6 */,new T(function(){
    return new T2(0,E(E(_EQ/* svzV */).b),E(_EP/* svzU */));
  }));
},
_ET/* $fRealFracNominalDiffTime_$ctruncate */ = function(_EU/* sjkL */, _EV/* sjkM */){
  var _EW/* sjkN */ = B(_Ex/* GHC.Real.$w$s$c/ */(_EV/* sjkM */, _yP/* GHC.Real.$fEnumRatio1 */, _y0/* Data.Fixed.$fHasResolutionE5 */, _yP/* GHC.Real.$fEnumRatio1 */));
  return E(B(_EM/* GHC.Real.$w$s$cproperFraction */(_EU/* sjkL */, _EW/* sjkN */.a, _EW/* sjkN */.b)).a);
},
_EX/* $w$cshiftL */ = function(_EY/* s1Rz0 */, _EZ/* s1Rz1 */){
  if(_EZ/* s1Rz1 */<64){
    var _F0/* s1Rz5 */ = hs_uncheckedShiftL64/* EXTERNAL */(_EY/* s1Rz0 */, _EZ/* s1Rz1 */);
    return E(_F0/* s1Rz5 */);
  }else{
    var _F1/* s1Rz9 */ = hs_wordToWord64/* EXTERNAL */(0);
    return E(_F1/* s1Rz9 */);
  }
},
_F2/* lvl5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Negative exponent"));
}),
_F3/* ^1 */ = new T(function(){
  return B(err/* EXTERNAL */(_F2/* GHC.Real.lvl5 */));
}),
_F4/* even2 */ = new T1(0,2),
_F5/* even3 */ = new T(function(){
  return B(_yk/* GHC.Integer.Type.eqInteger */(_F4/* GHC.Real.even2 */, _DJ/* GHC.Real.even1 */));
}),
_F6/* g */ = function(_F7/* svB9 */, _F8/* svBa */, _F9/* svBb */){
  while(1){
    if(!E(_F5/* GHC.Real.even3 */)){
      if(!B(_yk/* GHC.Integer.Type.eqInteger */(B(_DK/* GHC.Integer.Type.remInteger */(_F8/* svBa */, _F4/* GHC.Real.even2 */)), _DJ/* GHC.Real.even1 */))){
        if(!B(_yk/* GHC.Integer.Type.eqInteger */(_F8/* svBa */, _yP/* GHC.Real.$fEnumRatio1 */))){
          var _Fa/*  svB9 */ = B(_yA/* GHC.Integer.Type.timesInteger */(_F7/* svB9 */, _F7/* svB9 */)),
          _Fb/*  svBa */ = B(_E5/* GHC.Integer.Type.quotInteger */(B(_zK/* GHC.Integer.Type.minusInteger */(_F8/* svBa */, _yP/* GHC.Real.$fEnumRatio1 */)), _F4/* GHC.Real.even2 */)),
          _Fc/*  svBb */ = B(_yA/* GHC.Integer.Type.timesInteger */(_F7/* svB9 */, _F9/* svBb */));
          _F7/* svB9 */ = _Fa/*  svB9 */;
          _F8/* svBa */ = _Fb/*  svBa */;
          _F9/* svBb */ = _Fc/*  svBb */;
          continue;
        }else{
          return new F(function(){return _yA/* GHC.Integer.Type.timesInteger */(_F7/* svB9 */, _F9/* svBb */);});
        }
      }else{
        var _Fa/*  svB9 */ = B(_yA/* GHC.Integer.Type.timesInteger */(_F7/* svB9 */, _F7/* svB9 */)),
        _Fb/*  svBa */ = B(_E5/* GHC.Integer.Type.quotInteger */(_F8/* svBa */, _F4/* GHC.Real.even2 */));
        _F7/* svB9 */ = _Fa/*  svB9 */;
        _F8/* svBa */ = _Fb/*  svBa */;
        continue;
      }
    }else{
      return E(_ft/* GHC.Real.divZeroError */);
    }
  }
},
_Fd/* ^_f */ = function(_Fe/* svBn */, _Ff/* svBo */){
  while(1){
    if(!E(_F5/* GHC.Real.even3 */)){
      if(!B(_yk/* GHC.Integer.Type.eqInteger */(B(_DK/* GHC.Integer.Type.remInteger */(_Ff/* svBo */, _F4/* GHC.Real.even2 */)), _DJ/* GHC.Real.even1 */))){
        if(!B(_yk/* GHC.Integer.Type.eqInteger */(_Ff/* svBo */, _yP/* GHC.Real.$fEnumRatio1 */))){
          return new F(function(){return _F6/* GHC.Real.g */(B(_yA/* GHC.Integer.Type.timesInteger */(_Fe/* svBn */, _Fe/* svBn */)), B(_E5/* GHC.Integer.Type.quotInteger */(B(_zK/* GHC.Integer.Type.minusInteger */(_Ff/* svBo */, _yP/* GHC.Real.$fEnumRatio1 */)), _F4/* GHC.Real.even2 */)), _Fe/* svBn */);});
        }else{
          return E(_Fe/* svBn */);
        }
      }else{
        var _Fg/*  svBn */ = B(_yA/* GHC.Integer.Type.timesInteger */(_Fe/* svBn */, _Fe/* svBn */)),
        _Fh/*  svBo */ = B(_E5/* GHC.Integer.Type.quotInteger */(_Ff/* svBo */, _F4/* GHC.Real.even2 */));
        _Fe/* svBn */ = _Fg/*  svBn */;
        _Ff/* svBo */ = _Fh/*  svBo */;
        continue;
      }
    }else{
      return E(_ft/* GHC.Real.divZeroError */);
    }
  }
},
_Fi/* ^_$s^ */ = function(_Fj/* svBz */, _Fk/* svBA */){
  if(!B(_zk/* GHC.Integer.Type.ltInteger */(_Fk/* svBA */, _DJ/* GHC.Real.even1 */))){
    if(!B(_yk/* GHC.Integer.Type.eqInteger */(_Fk/* svBA */, _DJ/* GHC.Real.even1 */))){
      return new F(function(){return _Fd/* GHC.Real.^_f */(_Fj/* svBz */, _Fk/* svBA */);});
    }else{
      return E(_yP/* GHC.Real.$fEnumRatio1 */);
    }
  }else{
    return E(_F3/* GHC.Real.^1 */);
  }
},
_Fl/* cpuTimePrecision1 */ = new T1(0,64),
_Fm/* cpuTimePrecision2 */ = new T1(0,2),
_Fn/* cpuTimePrecision */ = new T(function(){
  return B(_Fi/* GHC.Real.^_$s^ */(_Fm/* System.CPUTime.cpuTimePrecision2 */, _Fl/* System.CPUTime.cpuTimePrecision1 */));
}),
_Fo/* getCPUTime2 */ = new T1(0,0),
_Fp/* initSMGen3 */ = new T(function(){
  return B(_yc/* GHC.Integer.Type.divInteger */(_Fo/* System.CPUTime.getCPUTime2 */, _Fn/* System.CPUTime.cpuTimePrecision */));
}),
_Fq/* initSMGen5 */ = new T1(0,0),
_Fr/* initSMGen4 */ = new T(function(){
  return B(_yk/* GHC.Integer.Type.eqInteger */(_Fn/* System.CPUTime.cpuTimePrecision */, _Fq/* System.Random.SplitMix.initSMGen5 */));
}),
_Fs/* initSMGen2 */ = function(_Ft/* saXA */, _/* EXTERNAL */){
  return new T(function(){
    if(!E(_Fr/* System.Random.SplitMix.initSMGen4 */)){
      var _Fu/* saXF */ = hs_wordToWord64/* EXTERNAL */(B(_CN/* GHC.Integer.Type.integerToWord */(_Fp/* System.Random.SplitMix.initSMGen3 */))),
      _Fv/* saXM */ = hs_wordToWord64/* EXTERNAL */(B(_ET/* Data.Time.Clock.UTC.$fRealFracNominalDiffTime_$ctruncate */(_zi/* GHC.Word.$fIntegralWord32 */, _Ft/* saXA */)));
      return hs_or64/* EXTERNAL */(B(_EX/* GHC.Word.$w$cshiftL */(_Fu/* saXF */, 32)), _Fv/* saXM */);
    }else{
      return E(_ft/* GHC.Real.divZeroError */);
    }
  });
},
_Fw/* mkSMGen */ = function(_Fx/* saZo */){
  var _Fy/* saZp */ = E(_Fx/* saZo */);
  return new T2(0,B(_e4/* System.Random.SplitMix.$wmix64 */(_Fy/* saZp */)),B(_gC/* System.Random.SplitMix.$wmixGamma */(B(_dL/* GHC.Word.$w$c+ */(_Fy/* saZp */, new Long/* EXTERNAL */(2135587861, 2654435769, true))))));
},
_Fz/* lvl */ = function(_/* EXTERNAL */){
  var _FA/* szS3 */ = B(_yI/* Data.Time.Clock.POSIX.getPOSIXTime1 */(0)),
  _FB/* szS6 */ = B(_Fs/* System.Random.SplitMix.initSMGen2 */(_FA/* szS3 */, 0));
  return new F(function(){return nMV/* EXTERNAL */(new T(function(){
    return B(_Fw/* System.Random.SplitMix.mkSMGen */(_FB/* szS6 */));
  }));});
},
_FC/* theStdGen */ = new T(function(){
  return B(_dB/* GHC.IO.unsafeDupablePerformIO */(_Fz/* System.Random.lvl */));
}),
_FD/* a55 */ = function(_FE/* siFm */, _FF/* siFn */, _/* EXTERNAL */){
  var _FG/* siFr */ = mMV/* EXTERNAL */(E(_FC/* System.Random.theStdGen */), _xt/* LudoJS.lvl41 */),
  _FH/* siFu */ = E(_FG/* siFr */),
  _FI/* siFw */ = B(_nZ/* LudoJS.a42 */(_FH/* siFu */, _FF/* siFn */, _/* EXTERNAL */)),
  _FJ/* siFz */ = E(_FI/* siFw */),
  _FK/* siFB */ = _FJ/* siFz */.b,
  _FL/* siFC */ = E(_FJ/* siFz */.a);
  if(!_FL/* siFC */._){
    var _FM/* siFD */ = E(_FK/* siFB */),
    _FN/* siFP */ = B(_xf/* LudoJS.$wa14 */(new T1(0,new T1(1,_FH/* siFu */)), _FM/* siFD */.b, new T(function(){
      return E(_FM/* siFD */.c)-1|0;
    }), _FM/* siFD */.d, _FM/* siFD */.e, _/* EXTERNAL */)),
    _FO/* siFV */ = E(E(_FN/* siFP */).b),
    _FP/* siFW */ = _FO/* siFV */.a,
    _FQ/* siG1 */ = new T5(0,_FP/* siFW */,_FO/* siFV */.b,_FO/* siFV */.c,_FO/* siFV */.d,_FO/* siFV */.e),
    _FR/* siG2 */ = B(_lH/* LudoJS.$w$ctoAny */(_FQ/* siG1 */)),
    _FS/* siG6 */ = __lst2arr/* EXTERNAL */(B(_lD/* GHC.Base.map */(_nX/* LudoJS.$fToAnyOption_$ctoAny */, _4/* GHC.Types.[] */))),
    _FT/* siG9 */ = E(_FP/* siFW */);
    switch(_FT/* siG9 */._){
      case 0:
        var _FU/* siGb */ = E(_FT/* siG9 */.a);
        if(!_FU/* siGb */._){
          var _FV/* siGl */ = __app4/* EXTERNAL */(E(_xs/* LudoJS.f1 */), _FR/* siG2 */, _FS/* siG6 */,  -1, E(_FE/* siFm */));
          return new T2(0,_eb/* GHC.Tuple.() */,_FQ/* siG1 */);
        }else{
          var _FW/* siGB */ = __app4/* EXTERNAL */(E(_xs/* LudoJS.f1 */), _FR/* siG2 */, _FS/* siG6 */, E(_FU/* siGb */.a), E(_FE/* siFm */));
          return new T2(0,_eb/* GHC.Tuple.() */,_FQ/* siG1 */);
        }
        break;
      case 1:
        var _FX/* siGR */ = __app4/* EXTERNAL */(E(_xs/* LudoJS.f1 */), _FR/* siG2 */, _FS/* siG6 */, E(_FT/* siG9 */.a), E(_FE/* siFm */));
        return new T2(0,_eb/* GHC.Tuple.() */,_FQ/* siG1 */);
      case 2:
        var _FY/* siH8 */ = __app4/* EXTERNAL */(E(_xs/* LudoJS.f1 */), _FR/* siG2 */, _FS/* siG6 */, E(_FT/* siG9 */.a), E(_FE/* siFm */));
        return new T2(0,_eb/* GHC.Tuple.() */,_FQ/* siG1 */);
      default:
        return E(_oW/* LudoJS.lvl13 */);
    }
  }else{
    var _FZ/* siHf */ = E(_FK/* siFB */),
    _G0/* siHp */ = new T5(0,new T1(1,_FH/* siFu */),_FZ/* siHf */.b,new T(function(){
      if(E(_FH/* siFu */)==6){
        return E(_di/* LudoJS.numPiecesAt2 */);
      }else{
        return E(_d7/* LudoJS.$fShowStage2 */);
      }
    }),_FZ/* siHf */.d,_FZ/* siHf */.e),
    _G1/* siHu */ = __lst2arr/* EXTERNAL */(B(_lD/* GHC.Base.map */(_nX/* LudoJS.$fToAnyOption_$ctoAny */, _FL/* siFC */))),
    _G2/* siHG */ = __app4/* EXTERNAL */(E(_xs/* LudoJS.f1 */), B(_lH/* LudoJS.$w$ctoAny */(_G0/* siHp */)), _G1/* siHu */, _FH/* siFu */, E(_FE/* siFm */));
    return new T2(0,_eb/* GHC.Tuple.() */,_G0/* siHp */);
  }
},
_G3/* f3 */ = new T(function(){
  return eval/* EXTERNAL */("(() => gameState)");
}),
_G4/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */("piece does not exist"));
}),
_G5/* lvl1 */ = new T(function(){
  return B(err/* EXTERNAL */(_G4/* LudoJS.lvl */));
}),
_G6/* lvl2 */ = "((x, y, player) => posToField(x, y, player))",
_G7/* lvl42 */ = function(_G8/* siJg */){
  var _G9/* siJh */ = B(_gY/* System.Random.$w$crandomR12 */(_gV/* System.Random.Internal.$fRandomGenStdGen */, 1, 4, _G8/* siJg */));
  return new T2(0,E(_G9/* siJh */.b),_G9/* siJh */.a);
},
_Ga/* lvl9 */ = new T2(1,_1P/* LudoJS.Green */,_6b/* LudoJS.lvl8 */),
_Gb/* play9 */ = new T2(1,_1S/* LudoJS.Yellow */,_4/* GHC.Types.[] */),
_Gc/* play8 */ = new T2(1,_1Q/* LudoJS.Red */,_Gb/* LudoJS.play9 */),
_Gd/* play7 */ = new T2(1,_1P/* LudoJS.Green */,_Gc/* LudoJS.play8 */),
_Ge/* lvl15 */ = new T2(0,_61/* LudoJS.lvl14 */,_60/* LudoJS.Out */),
_Gf/* lvl16 */ = new T2(1,_Ge/* LudoJS.lvl15 */,_4/* GHC.Types.[] */),
_Gg/* lvl17 */ = new T2(0,_63/* LudoJS.play10 */,_60/* LudoJS.Out */),
_Gh/* lvl18 */ = new T2(1,_Gg/* LudoJS.lvl17 */,_Gf/* LudoJS.lvl16 */),
_Gi/* lvl20 */ = new T2(0,_65/* LudoJS.lvl19 */,_60/* LudoJS.Out */),
_Gj/* lvl21 */ = new T2(1,_Gi/* LudoJS.lvl20 */,_Gh/* LudoJS.lvl18 */),
_Gk/* lvl22 */ = new T2(0,_di/* LudoJS.numPiecesAt2 */,_60/* LudoJS.Out */),
_Gl/* lvl23 */ = new T2(1,_Gk/* LudoJS.lvl22 */,_Gj/* LudoJS.lvl21 */),
_Gm/* go */ = function(_Gn/* shO2 */){
  var _Go/* shO3 */ = E(_Gn/* shO2 */);
  return (_Go/* shO3 */._==0) ? __Z/* EXTERNAL */ : new T2(1,new T2(0,_Go/* shO3 */.a,_Gl/* LudoJS.lvl23 */),new T(function(){
    return B(_Gm/* LudoJS.go */(_Go/* shO3 */.b));
  }));
},
_Gp/* play_$sgo */ = function(_Gq/* shNY */, _Gr/* shNZ */){
  return new T2(1,new T2(0,_Gq/* shNY */,_Gl/* LudoJS.lvl23 */),new T(function(){
    return B(_Gm/* LudoJS.go */(_Gr/* shNZ */));
  }));
},
_Gs/* play6 */ = new T(function(){
  return B(_Gp/* LudoJS.play_$sgo */(_1O/* LudoJS.Blue */, _Gd/* LudoJS.play7 */));
}),
_Gt/* play5 */ = new T(function(){
  return B(_5z/* LudoJS.$sfromList */(_Gs/* LudoJS.play6 */));
}),
_Gu/* play_f1 */ = new T(function(){
  return eval/* EXTERNAL */("((gs) => gameState=gs)");
}),
_Gv/* $wlvl */ = function(_Gw/* siJn */, _Gx/* siJo */, _Gy/* siJp */, _Gz/* siJq */, _/* EXTERNAL */){
  var _GA/* siJs */ = E(_Gy/* siJp */);
  if(!_GA/* siJs */._){
    return _eb/* GHC.Tuple.() */;
  }else{
    if(!E(_GA/* siJs */.a)){
      var _GB/* siJv */ = E(_G3/* LudoJS.f3 */),
      _GC/* siJy */ = __app0/* EXTERNAL */(_GB/* siJv */),
      _GD/* siJB */ = B(_cU/* LudoJS.$wa1 */(_GC/* siJy */, _/* EXTERNAL */)),
      _GE/* siJS */ = function(_GF/* siJT */){
        var _GG/* siK1 */ = eval/* EXTERNAL */(E(_G6/* LudoJS.lvl2 */)),
        _GH/* siK9 */ = __app3/* EXTERNAL */(E(_GG/* siK1 */), E(_Gw/* siJn */), E(_Gx/* siJo */), toJSStr/* EXTERNAL */(_GF/* siJT */)),
        _GI/* siKf */ = __eq/* EXTERNAL */(_GH/* siK9 */, E(_dF/* Haste.Prim.Any.jsNull */));
        if(!E(_GI/* siKf */)){
          var _GJ/* siKk */ = __isUndef/* EXTERNAL */(_GH/* siK9 */);
          if(!E(_GJ/* siKk */)){
            var _GK/* siKo */ = new T(function(){
              var _GL/* siKq */ = Number/* EXTERNAL */(_GH/* siK9 */);
              return jsTrunc/* EXTERNAL */(_GL/* siKq */);
            }),
            _GM/* siKy */ = __app0/* EXTERNAL */(_GB/* siJv */),
            _GN/* siKB */ = B(_cU/* LudoJS.$wa1 */(_GM/* siKy */, _/* EXTERNAL */)),
            _GO/* siKE */ = E(_GN/* siKB */),
            _GP/* siKG */ = _GO/* siKE */.b,
            _GQ/* siKH */ = _GO/* siKE */.c,
            _GR/* siKI */ = _GO/* siKE */.d,
            _GS/* siKJ */ = _GO/* siKE */.e,
            _GT/* siKK */ = E(_GO/* siKE */.a);
            switch(_GT/* siKK */._){
              case 0:
                if(E(_GK/* siKo */)==( -5)){
                  var _GU/* siKW */ = E(_GT/* siKK */.a);
                  if(!_GU/* siKW */._){
                    var _GV/* siKX */ = B(_FD/* LudoJS.a55 */(_kX/* LudoJS.$fToAnyGameState18 */, _GO/* siKE */, _/* EXTERNAL */)),
                    _GW/* siLd */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_GV/* siKX */).b))));
                    return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                  }else{
                    var _GX/* siLh */ = B(_FD/* LudoJS.a55 */(_GU/* siKW */.a, _GO/* siKE */, _/* EXTERNAL */)),
                    _GY/* siLx */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_GX/* siLh */).b))));
                    return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                  }
                }else{
                  var _GZ/* siKT */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(_GO/* siKE */)));
                  return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                }
                break;
              case 1:
                var _H0/* siLB */ = E(_GK/* siKo */),
                _H1/* siLD */ = function(_/* EXTERNAL */, _H2/* siLF */, _H3/* siLG */, _H4/* siLH */, _H5/* siLI */, _H6/* siLJ */){
                  var _H7/* siLL */ = B(_oX/* LudoJS.$wa16 */(_H2/* siLF */, _H3/* siLG */, _H4/* siLH */, _H5/* siLI */, _H6/* siLJ */, new T5(0,_H2/* siLF */,_H3/* siLG */,_H4/* siLH */,_H5/* siLI */,_H6/* siLJ */), _/* EXTERNAL */)),
                  _H8/* siM1 */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_H7/* siLL */).b))));
                  return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                },
                _H9/* siM4 */ = function(_/* EXTERNAL */, _Ha/* siM6 */, _Hb/* siM7 */, _Hc/* siM8 */, _Hd/* siM9 */, _He/* siMa */, _Hf/* siMb */){
                  var _Hg/* siMc */ = E(_Ha/* siM6 */);
                  if(!_Hg/* siMc */._){
                    var _Hh/* siMe */ = B(_oX/* LudoJS.$wa16 */(_Hb/* siM7 */, _Hc/* siM8 */, _Hd/* siM9 */, _He/* siMa */, _Hf/* siMb */, new T5(0,_Hb/* siM7 */,_Hc/* siM8 */,_Hd/* siM9 */,_He/* siMa */,_Hf/* siMb */), _/* EXTERNAL */)),
                    _Hi/* siMu */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_Hh/* siMe */).b))));
                    return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                  }else{
                    var _Hj/* siMx */ = _Hg/* siMc */.a,
                    _Hk/* siMy */ = function(_Hl/* siMz */){
                      var _Hm/* siMB */ = B(_oX/* LudoJS.$wa16 */(_Hb/* siM7 */, _Hc/* siM8 */, _Hd/* siM9 */, _He/* siMa */, _Hf/* siMb */, new T5(0,_Hb/* siM7 */,_Hc/* siM8 */,_Hd/* siM9 */,_He/* siMa */,_Hf/* siMb */), _/* EXTERNAL */)),
                      _Hn/* siMR */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_Hm/* siMB */).b))));
                      return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                    },
                    _Ho/* siMU */ = function(_Hp/* siMV */){
                      var _Hq/* siMW */ = new T2(2,_GT/* siKK */.a,_Hj/* siMx */),
                      _Hr/* siMY */ = B(_oX/* LudoJS.$wa16 */(_Hq/* siMW */, _Hc/* siM8 */, _Hd/* siM9 */, _He/* siMa */, _Hf/* siMb */, new T5(0,_Hq/* siMW */,_Hc/* siM8 */,_Hd/* siM9 */,_He/* siMa */,_Hf/* siMb */), _/* EXTERNAL */)),
                      _Hs/* siNe */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_Hr/* siMY */).b))));
                      return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                    },
                    _Ht/* siNh */ = B(_da/* LudoJS.$s!1 */(_Hc/* siM8 */, _He/* siMa */));
                    if(!_Ht/* siNh */._){
                      return new F(function(){return _Hk/* siMy */(_/* EXTERNAL */);});
                    }else{
                      var _Hu/* siNp */ = E(_Hj/* siMx */);
                      if(E(E(_Ht/* siNh */.a).a)!=_Hu/* siNp */){
                        var _Hv/* siNt */ = function(_Hw/* siNu */){
                          while(1){
                            var _Hx/* siNv */ = E(_Hw/* siNu */);
                            if(!_Hx/* siNv */._){
                              return false;
                            }else{
                              if(E(E(_Hx/* siNv */.a).a)!=_Hu/* siNp */){
                                _Hw/* siNu */ = _Hx/* siNv */.b;
                                continue;
                              }else{
                                return true;
                              }
                            }
                          }
                        };
                        if(!B(_Hv/* siNt */(_Ht/* siNh */.b))){
                          return new F(function(){return _Hk/* siMy */(_/* EXTERNAL */);});
                        }else{
                          return new F(function(){return _Ho/* siMU */(_/* EXTERNAL */);});
                        }
                      }else{
                        return new F(function(){return _Ho/* siMU */(_/* EXTERNAL */);});
                      }
                    }
                  }
                };
                if(_H0/* siLB */<( -4)){
                  if(_H0/* siLB */<0){
                    return new F(function(){return _H1/* siLD */(_/* EXTERNAL */, _GT/* siKK */, _GP/* siKG */, _GQ/* siKH */, _GR/* siKI */, _GS/* siKJ */);});
                  }else{
                    if(_H0/* siLB */>55){
                      return new F(function(){return _H1/* siLD */(_/* EXTERNAL */, _GT/* siKK */, _GP/* siKG */, _GQ/* siKH */, _GR/* siKI */, _GS/* siKJ */);});
                    }else{
                      var _Hy/* siNM */ = function(_Hz/* siNN */){
                        while(1){
                          var _HA/* siNO */ = E(_Hz/* siNN */);
                          if(!_HA/* siNO */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _HB/* siNQ */ = _HA/* siNO */.b,
                            _HC/* siNR */ = E(_HA/* siNO */.a),
                            _HD/* siNU */ = E(_HC/* siNR */.b);
                            if(!_HD/* siNU */._){
                              _Hz/* siNN */ = _HB/* siNQ */;
                              continue;
                            }else{
                              if(_H0/* siLB */!=E(_HD/* siNU */.a)){
                                _Hz/* siNN */ = _HB/* siNQ */;
                                continue;
                              }else{
                                return new T1(1,_HC/* siNR */.a);
                              }
                            }
                          }
                        }
                      };
                      return new F(function(){return _H9/* siM4 */(_/* EXTERNAL */, B(_Hy/* siNM */(B(_da/* LudoJS.$s!1 */(_GP/* siKG */, _GR/* siKI */)))), _GT/* siKK */, _GP/* siKG */, _GQ/* siKH */, _GR/* siKI */, _GS/* siKJ */);});
                    }
                  }
                }else{
                  if(_H0/* siLB */>( -1)){
                    if(_H0/* siLB */<0){
                      return new F(function(){return _H1/* siLD */(_/* EXTERNAL */, _GT/* siKK */, _GP/* siKG */, _GQ/* siKH */, _GR/* siKI */, _GS/* siKJ */);});
                    }else{
                      if(_H0/* siLB */>55){
                        return new F(function(){return _H1/* siLD */(_/* EXTERNAL */, _GT/* siKK */, _GP/* siKG */, _GQ/* siKH */, _GR/* siKI */, _GS/* siKJ */);});
                      }else{
                        var _HE/* siO8 */ = function(_HF/* siO9 */){
                          while(1){
                            var _HG/* siOa */ = E(_HF/* siO9 */);
                            if(!_HG/* siOa */._){
                              return __Z/* EXTERNAL */;
                            }else{
                              var _HH/* siOc */ = _HG/* siOa */.b,
                              _HI/* siOd */ = E(_HG/* siOa */.a),
                              _HJ/* siOg */ = E(_HI/* siOd */.b);
                              if(!_HJ/* siOg */._){
                                _HF/* siO9 */ = _HH/* siOc */;
                                continue;
                              }else{
                                if(_H0/* siLB */!=E(_HJ/* siOg */.a)){
                                  _HF/* siO9 */ = _HH/* siOc */;
                                  continue;
                                }else{
                                  return new T1(1,_HI/* siOd */.a);
                                }
                              }
                            }
                          }
                        };
                        return new F(function(){return _H9/* siM4 */(_/* EXTERNAL */, B(_HE/* siO8 */(B(_da/* LudoJS.$s!1 */(_GP/* siKG */, _GR/* siKI */)))), _GT/* siKK */, _GP/* siKG */, _GQ/* siKH */, _GR/* siKI */, _GS/* siKJ */);});
                      }
                    }
                  }else{
                    var _HK/* siOo */ = _H0/* siLB */+5|0,
                    _HL/* siOr */ = function(_HM/* siOs */){
                      while(1){
                        var _HN/* siOt */ = E(_HM/* siOs */);
                        if(!_HN/* siOt */._){
                          return __Z/* EXTERNAL */;
                        }else{
                          var _HO/* siOv */ = _HN/* siOt */.b,
                          _HP/* siOw */ = E(_HN/* siOt */.a);
                          if(E(_HP/* siOw */.a)!=_HK/* siOo */){
                            _HM/* siOs */ = _HO/* siOv */;
                            continue;
                          }else{
                            if(!E(_HP/* siOw */.b)._){
                              return E(new T1(1,_HK/* siOo */));
                            }else{
                              _HM/* siOs */ = _HO/* siOv */;
                              continue;
                            }
                          }
                        }
                      }
                    };
                    return new F(function(){return _H9/* siM4 */(_/* EXTERNAL */, B(_HL/* siOr */(B(_da/* LudoJS.$s!1 */(_GP/* siKG */, _GR/* siKI */)))), _GT/* siKK */, _GP/* siKG */, _GQ/* siKH */, _GR/* siKI */, _GS/* siKJ */);});
                  }
                }
                break;
              case 2:
                var _HQ/* siOH */ = _GT/* siKK */.a,
                _HR/* siOI */ = _GT/* siKK */.b,
                _HS/* siOJ */ = E(_GK/* siKo */);
                if(_HS/* siOJ */>56){
                  var _HT/* siOR */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(_GO/* siKE */)));
                  return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                }else{
                  if(_HS/* siOJ */<0){
                    var _HU/* siP0 */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(_GO/* siKE */)));
                    return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                  }else{
                    var _HV/* siP3 */ = B(_nZ/* LudoJS.a42 */(_HQ/* siOH */, _GO/* siKE */, _/* EXTERNAL */)),
                    _HW/* siP6 */ = E(_HV/* siP3 */),
                    _HX/* siP8 */ = _HW/* siP6 */.b,
                    _HY/* siP9 */ = new T(function(){
                      var _HZ/* siPa */ = new T2(1,_HR/* siOI */,_HS/* siOJ */),
                      _I0/* siPb */ = B(_da/* LudoJS.$s!1 */(_GP/* siKG */, _GR/* siKI */));
                      if(!_I0/* siPb */._){
                        return E(_G5/* LudoJS.lvl1 */);
                      }else{
                        var _I1/* siPe */ = E(_I0/* siPb */.a),
                        _I2/* siPh */ = E(_I1/* siPe */.a),
                        _I3/* siPj */ = E(_HR/* siOI */);
                        if(_I2/* siPh */!=_I3/* siPj */){
                          var _I4/* siPn */ = function(_I5/* siPo */){
                            while(1){
                              var _I6/* siPp */ = E(_I5/* siPo */);
                              if(!_I6/* siPp */._){
                                return E(_G5/* LudoJS.lvl1 */);
                              }else{
                                var _I7/* siPs */ = E(_I6/* siPp */.a),
                                _I8/* siPv */ = E(_I7/* siPs */.a);
                                if(_I8/* siPv */!=_I3/* siPj */){
                                  _I5/* siPo */ = _I6/* siPp */.b;
                                  continue;
                                }else{
                                  return (E(_I7/* siPs */.b)._==0) ? new T1(0,_I8/* siPv */) : E(_HZ/* siPa */);
                                }
                              }
                            }
                          };
                          return B(_I4/* siPn */(_I0/* siPb */.b));
                        }else{
                          if(!E(_I1/* siPe */.b)._){
                            return new T1(0,_I2/* siPh */);
                          }else{
                            return E(_HZ/* siPa */);
                          }
                        }
                      }
                    }),
                    _I9/* siPD */ = function(_/* EXTERNAL */, _Ia/* siPF */, _Ib/* siPG */, _Ic/* siPH */, _Id/* siPI */, _Ie/* siPJ */, _If/* siPK */){
                      if(!E(_Ia/* siPF */)){
                        var _Ig/* siPM */ = new T1(1,_HQ/* siOH */),
                        _Ih/* siPO */ = B(_oX/* LudoJS.$wa16 */(_Ig/* siPM */, _Ic/* siPH */, _Id/* siPI */, _Ie/* siPJ */, _If/* siPK */, new T5(0,_Ig/* siPM */,_Ic/* siPH */,_Id/* siPI */,_Ie/* siPJ */,_If/* siPK */), _/* EXTERNAL */)),
                        _Ii/* siQ4 */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_Ih/* siPO */).b))));
                        return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                      }else{
                        var _Ij/* siQ7 */ = B(_nC/* LudoJS.$wa15 */(_Ib/* siPG */, _Ic/* siPH */, _Id/* siPI */, _Ie/* siPJ */, _If/* siPK */, _/* EXTERNAL */)),
                        _Ik/* siQd */ = E(E(_Ij/* siQ7 */).b),
                        _Il/* siQj */ = B(_oX/* LudoJS.$wa16 */(_Ik/* siQd */.a, _Ik/* siQd */.b, _Ik/* siQd */.c, _Ik/* siQd */.d, _Ik/* siQd */.e, _Ik/* siQd */, _/* EXTERNAL */)),
                        _Im/* siQz */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_Il/* siQj */).b))));
                        return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                      }
                    };
                    if(!B(_uw/* GHC.List.elem */(_mJ/* LudoJS.$fEqOption */, _HY/* siP9 */, _HW/* siP6 */.a))){
                      var _In/* siQD */ = E(_HX/* siP8 */);
                      return new F(function(){return _I9/* siPD */(_/* EXTERNAL */, _qp/* GHC.Types.False */, _In/* siQD */.a, _In/* siQD */.b, _In/* siQD */.c, _In/* siQD */.d, _In/* siQD */.e);});
                    }else{
                      var _Io/* siQJ */ = function(_/* EXTERNAL */, _Ip/* siQL */, _Iq/* siQM */, _Ir/* siQN */, _Is/* siQO */, _It/* siQP */){
                        if(!B(_mL/* GHC.List.$wlenAcc */(B(_da/* LudoJS.$s!1 */(_Iq/* siQM */, _Is/* siQO */)), 0))){
                          var _Iu/* siRx */ = B(_q/* GHC.Base.++ */(_It/* siQP */, new T2(1,_Iq/* siQM */,_4/* GHC.Types.[] */)));
                          if(B(_mL/* GHC.List.$wlenAcc */(_Iu/* siRx */, 0))==3){
                            var _Iv/* siRC */ = B(_nd/* LudoJS.$sa1 */(_1O/* LudoJS.Blue */, _Ga/* LudoJS.lvl9 */, _Iu/* siRx */, _Ip/* siQL */, _Iq/* siQM */, _Ir/* siQN */, _Is/* siQO */, _Iu/* siRx */, _/* EXTERNAL */)),
                            _Iw/* siRJ */ = B(_nC/* LudoJS.$wa15 */(_cw/* LudoJS.GameFinished */, _Iq/* siQM */, _Ir/* siQN */, _Is/* siQO */, new T(function(){
                              return E(E(_Iv/* siRC */).a);
                            }), _/* EXTERNAL */)),
                            _Ix/* siRP */ = E(E(_Iw/* siRJ */).b),
                            _Iy/* siRV */ = B(_oX/* LudoJS.$wa16 */(_Ix/* siRP */.a, _Ix/* siRP */.b, _Ix/* siRP */.c, _Ix/* siRP */.d, _Ix/* siRP */.e, _Ix/* siRP */, _/* EXTERNAL */)),
                            _Iz/* siSb */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_Iy/* siRV */).b))));
                            return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                          }else{
                            return new F(function(){return _I9/* siPD */(_/* EXTERNAL */, _xe/* GHC.Types.True */, new T1(0,new T1(1,_HQ/* siOH */)), _Iq/* siQM */, _Ir/* siQN */, _Is/* siQO */, _Iu/* siRx */);});
                          }
                        }else{
                          if(B(_mL/* GHC.List.$wlenAcc */(_It/* siQP */, 0))==3){
                            var _IA/* siQV */ = B(_nd/* LudoJS.$sa1 */(_1O/* LudoJS.Blue */, _Ga/* LudoJS.lvl9 */, _It/* siQP */, _Ip/* siQL */, _Iq/* siQM */, _Ir/* siQN */, _Is/* siQO */, _It/* siQP */, _/* EXTERNAL */)),
                            _IB/* siR2 */ = B(_nC/* LudoJS.$wa15 */(_cw/* LudoJS.GameFinished */, _Iq/* siQM */, _Ir/* siQN */, _Is/* siQO */, new T(function(){
                              return E(E(_IA/* siQV */).a);
                            }), _/* EXTERNAL */)),
                            _IC/* siR8 */ = E(E(_IB/* siR2 */).b),
                            _ID/* siRe */ = B(_oX/* LudoJS.$wa16 */(_IC/* siR8 */.a, _IC/* siR8 */.b, _IC/* siR8 */.c, _IC/* siR8 */.d, _IC/* siR8 */.e, _IC/* siR8 */, _/* EXTERNAL */)),
                            _IE/* siRu */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_ID/* siRe */).b))));
                            return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
                          }else{
                            return new F(function(){return _I9/* siPD */(_/* EXTERNAL */, _xe/* GHC.Types.True */, new T1(0,new T1(1,_HQ/* siOH */)), _Iq/* siQM */, _Ir/* siQN */, _Is/* siQO */, _It/* siQP */);});
                          }
                        }
                      },
                      _IF/* siSe */ = E(_HY/* siP9 */);
                      if(!_IF/* siSe */._){
                        var _IG/* siSg */ = B(_vo/* LudoJS.$wa7 */(_IF/* siSe */.a, 0, _HX/* siP8 */, _/* EXTERNAL */)),
                        _IH/* siSm */ = E(E(_IG/* siSg */).b);
                        return new F(function(){return _Io/* siQJ */(_/* EXTERNAL */, _IH/* siSm */.a, _IH/* siSm */.b, _IH/* siSm */.c, _IH/* siSm */.d, _IH/* siSm */.e);});
                      }else{
                        var _II/* siSw */ = B(_vo/* LudoJS.$wa7 */(_IF/* siSe */.a, E(_IF/* siSe */.b), _HX/* siP8 */, _/* EXTERNAL */)),
                        _IJ/* siSC */ = E(E(_II/* siSw */).b);
                        return new F(function(){return _Io/* siQJ */(_/* EXTERNAL */, _IJ/* siSC */.a, _IJ/* siSC */.b, _IJ/* siSC */.c, _IJ/* siSC */.d, _IJ/* siSC */.e);});
                      }
                    }
                  }
                }
                break;
              default:
                var _IK/* siSK */ = mMV/* EXTERNAL */(E(_FC/* System.Random.theStdGen */), _G7/* LudoJS.lvl42 */),
                _IL/* siSP */ = new T(function(){
                  switch(E(_IK/* siSK */)){
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
                _IM/* siSS */ = B(_oX/* LudoJS.$wa16 */(_mw/* LudoJS.play11 */, _IL/* siSP */, _63/* LudoJS.play10 */, _Gt/* LudoJS.play5 */, _4/* GHC.Types.[] */, new T5(0,_mw/* LudoJS.play11 */,_IL/* siSP */,_63/* LudoJS.play10 */,_Gt/* LudoJS.play5 */,_4/* GHC.Types.[] */), _/* EXTERNAL */)),
                _IN/* siT8 */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), B(_lH/* LudoJS.$w$ctoAny */(E(E(_IM/* siSS */).b))));
                return new F(function(){return _mK/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
            }
          }else{
            return _eb/* GHC.Tuple.() */;
          }
        }else{
          return _eb/* GHC.Tuple.() */;
        }
      };
      switch(E(E(_GD/* siJB */).b)){
        case 0:
          return new F(function(){return _GE/* siJS */(E(_1u/* LudoJS.$fFromAnyGameState14 */));});
          break;
        case 1:
          return new F(function(){return _GE/* siJS */(E(_1t/* LudoJS.$fFromAnyGameState13 */));});
          break;
        case 2:
          return new F(function(){return _GE/* siJS */(E(_1s/* LudoJS.$fFromAnyGameState12 */));});
          break;
        default:
          return new F(function(){return _GE/* siJS */(E(_1r/* LudoJS.$fFromAnyGameState11 */));});
      }
    }else{
      return _eb/* GHC.Tuple.() */;
    }
  }
},
_IO/* play2 */ = function(_IP/* siTg */, _/* EXTERNAL */){
  var _IQ/* siTi */ = E(_IP/* siTg */),
  _IR/* siTm */ = E(_IQ/* siTi */.a);
  return new F(function(){return _Gv/* LudoJS.$wlvl */(_IR/* siTm */.a, _IR/* siTm */.b, _IQ/* siTi */.b, _IQ/* siTi */.c, _/* EXTERNAL */);});
},
_IS/* play4 */ = new T(function(){
  return B(_lD/* GHC.Base.map */(_nX/* LudoJS.$fToAnyOption_$ctoAny */, _4/* GHC.Types.[] */));
}),
_IT/* play1 */ = function(_IU/* siTp */, _IV/* siTq */, _/* EXTERNAL */){
  var _IW/* siTt */ = B(_lH/* LudoJS.$w$ctoAny */(new T5(0,_mw/* LudoJS.play11 */,_IU/* siTp */,_63/* LudoJS.play10 */,_Gt/* LudoJS.play5 */,_4/* GHC.Types.[] */))),
  _IX/* siTx */ = __app1/* EXTERNAL */(E(_Gu/* LudoJS.play_f1 */), _IW/* siTt */),
  _IY/* siTD */ = __lst2arr/* EXTERNAL */(E(_IS/* LudoJS.play4 */)),
  _IZ/* siTJ */ = eval/* EXTERNAL */(E(_oU/* LudoJS.play3 */)),
  _J0/* siTT */ = __app3/* EXTERNAL */(E(_IZ/* siTJ */), _IW/* siTt */, _IY/* siTD */,  -1),
  _J1/* siTW */ = B(A(_m4/* Haste.Events.Core.onEvent */,[_kB/* Haste.Events.Core.$fMonadEventIO */, _jR/* Haste.Events.Core.$fEventSourceElem1 */, _jQ/* Haste.Events.MouseEvents.$fEventMouseEvent */, _IV/* siTq */, _lK/* Haste.Events.MouseEvents.Click */, _IO/* LudoJS.play2 */, _/* EXTERNAL */]));
  return _eb/* GHC.Tuple.() */;
},
_J2/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */(" found!"));
}),
_J3/* withElem1 */ = function(_J4/* svSB */){
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("No element with ID ", new T(function(){
    return B(_q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(E(_J4/* svSB */)), _J2/* Haste.DOM.JSString.lvl */));
  }))));});
},
_J5/* main1 */ = function(_/* EXTERNAL */){
  var _J6/* stGn */ = function(_J7/* stFU */){
    var _J8/* stFV */ = new T(function(){
      var _J9/* stFY */ = B(_cU/* LudoJS.$wa1 */(E(_J7/* stFU */), _/* EXTERNAL */));
      return E(_J9/* stFY */);
    }),
    _Ja/* stGm */ = function(_Jb/* stG1 */){
      var _Jc/* stG2 */ = new T(function(){
        var _Jd/* stG3 */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_Jb/* stG1 */, _/* EXTERNAL */));
        return E(_Jd/* stG3 */);
      });
      return function(_Je/* stG6 */, _/* EXTERNAL */){
        var _Jf/* stGb */ = Number/* EXTERNAL */(E(_Je/* stG6 */)),
        _Jg/* stGf */ = jsTrunc/* EXTERNAL */(_Jf/* stGb */);
        return new F(function(){return _dj/* LudoJS.$wa6 */(_J8/* stFV */, _Jc/* stG2 */, _Jg/* stGf */, _/* EXTERNAL */);});
      };
    };
    return E(_Ja/* stGm */);
  },
  _Jh/* stGr */ = __createJSFunc/* EXTERNAL */(4, E(_J6/* stGn */)),
  _Ji/* stGz */ = __app2/* EXTERNAL */(E(_dz/* Main.f2 */), "numPiecesAt", _Jh/* stGr */),
  _Jj/* stGE */ = mMV/* EXTERNAL */(E(_FC/* System.Random.theStdGen */), _hg/* Main.lvl4 */),
  _Jk/* stGL */ = "canvas",
  _Jl/* stGR */ = __app1/* EXTERNAL */(E(_dy/* Haste.DOM.JSString.elemById_f1 */), _Jk/* stGL */),
  _Jm/* stGX */ = __eq/* EXTERNAL */(_Jl/* stGR */, E(_dF/* Haste.Prim.Any.jsNull */));
  if(!E(_Jm/* stGX */)){
    var _Jn/* stH3 */ = __isUndef/* EXTERNAL */(_Jl/* stGR */);
    if(!E(_Jn/* stH3 */)){
      return new F(function(){return _IT/* LudoJS.play1 */(new T(function(){
        switch(E(_Jj/* stGE */)){
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
      }), _Jl/* stGR */, _/* EXTERNAL */);});
    }else{
      return new F(function(){return _J3/* Haste.DOM.JSString.withElem1 */(_Jk/* stGL */);});
    }
  }else{
    return new F(function(){return _J3/* Haste.DOM.JSString.withElem1 */(_Jk/* stGL */);});
  }
},
_Jo/* main */ = function(_/* EXTERNAL */){
  return new F(function(){return _J5/* Main.main1 */(_/* EXTERNAL */);});
};

var hasteMain = function() {B(A(_Jo, [0]));};onHasteStart(); hasteMain();