"use strict";
var __haste_prog_id = '23cf56da0ffec282152927f83385ca4d6927d44abddedb525e3a56561eb93113';
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
const boardColor = [230, 230, 230];
const diceColor = boardColor;

function onHasteStart() {
    c = document.getElementById("canvas");
    c.width = c.scrollWidth;
    c.height = c.scrollHeight;
    ctx = c.getContext("2d");

    ctx.imageSmoothingEnabled = false;

    tileWidth = c.width / numTilesX;
    tileHeight = c.height / numTilesY;

    window.onresize = function(event) {    
        c.width = c.scrollWidth;
        c.height = c.scrollHeight;
        tileWidth = c.width / numTilesX;
        tileHeight = c.height / numTilesY;

        drawBoard(drawGameState, drawOptions, drawRoll);
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

function drawPlayer(posX, posY, player = "Green") {
    ctx.beginPath();
    ctx.strokeStyle = cssColor(...playerColorDark[player]);
    ctx.lineWidth = tileWidth * 0.1;

    ctx.moveTo(posX + tileWidth / 2, posY + tileHeight * 1/4);
    ctx.lineTo(posX + tileWidth * 1/4, posY + tileHeight * 3/4);
    ctx.moveTo(posX + tileWidth / 2, posY + tileHeight * 1/4);
    ctx.lineTo(posX + tileWidth * 3/4, posY + tileHeight * 3/4);
    ctx.stroke();
}

function drawDice(posX, posY, num) {
    ctx.fillStyle = cssColor(...diceColor);
    ctx.fillRect(posX + tileWidth * 0.15, posY + tileHeight * 0.15, tileWidth * 0.7, tileHeight * 0.7)

    ctx.fillStyle = cssColor(...iconColor);

    function drawDot(mulX, mulY) {
        ctx.beginPath();
        ctx.arc(posX + tileWidth * mulX, posY + tileHeight * mulY, tileWidth * 0.08, 0, 2 * Math.PI);
        ctx.fill();
    }

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
    
    ctx.moveTo(posX, posY);
    ctx.lineTo(posX + tileWidth, posY);
    ctx.lineTo(posX + tileWidth, posY + tileHeight);
    ctx.lineTo(posX, posY + tileHeight)
    ctx.lineTo(posX, posY);
    ctx.stroke();
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

    drawStaticBoard();

    // Draw dice
    drawTiles(dicePositions, drawDice, roll);

    // Draw players
    for (let player in gameState.pieces) {
        let playerPieces = gameState.pieces[player];

        for (let pieceIndex in gameState.pieces[player]) {
            let piece = playerPieces[pieceIndex];

            if (piece.piece == "Out") {
                drawTiles([outPositions[player][pieceIndex - 1]], drawPlayer, player);
            }
            else if (piece.piece == "Active") {
                drawTiles([playerPosToTilePos(player, piece.field)], drawPlayer, player);
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
  return B(_1o/* Control.Exception.Base.patError */("LudoJS.hs:(80,18)-(84,27)|case"));
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
_1A/* $fFromAnyGameState9 */ = function(_1B/* sgG7 */, _/* EXTERNAL */){
  return new T(function(){
    var _1C/* sgGc */ = String/* EXTERNAL */(E(_1B/* sgG7 */)),
    _1D/* sgGh */ = fromJSStr/* EXTERNAL */(_1C/* sgGc */);
    if(!B(_1v/* GHC.Base.eqString */(_1D/* sgGh */, _1u/* LudoJS.$fFromAnyGameState14 */))){
      if(!B(_1v/* GHC.Base.eqString */(_1D/* sgGh */, _1t/* LudoJS.$fFromAnyGameState13 */))){
        if(!B(_1v/* GHC.Base.eqString */(_1D/* sgGh */, _1s/* LudoJS.$fFromAnyGameState12 */))){
          if(!B(_1v/* GHC.Base.eqString */(_1D/* sgGh */, _1r/* LudoJS.$fFromAnyGameState11 */))){
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
_1E/* modInt# */ = function(_1F/* scF6 */, _1G/* scF7 */){
  var _1H/* scF8 */ = _1F/* scF6 */%_1G/* scF7 */;
  if(_1F/* scF6 */<=0){
    if(_1F/* scF6 */>=0){
      return E(_1H/* scF8 */);
    }else{
      if(_1G/* scF7 */<=0){
        return E(_1H/* scF8 */);
      }else{
        var _1I/* scFf */ = E(_1H/* scF8 */);
        return (_1I/* scFf */==0) ? 0 : _1I/* scFf */+_1G/* scF7 */|0;
      }
    }
  }else{
    if(_1G/* scF7 */>=0){
      if(_1F/* scF6 */>=0){
        return E(_1H/* scF8 */);
      }else{
        if(_1G/* scF7 */<=0){
          return E(_1H/* scF8 */);
        }else{
          var _1J/* scFm */ = E(_1H/* scF8 */);
          return (_1J/* scFm */==0) ? 0 : _1J/* scFm */+_1G/* scF7 */|0;
        }
      }
    }else{
      var _1K/* scFn */ = E(_1H/* scF8 */);
      return (_1K/* scFn */==0) ? 0 : _1K/* scFn */+_1G/* scF7 */|0;
    }
  }
},
_1L/* $wconvertCell */ = function(_1M/* sgHM */, _1N/* sgHN */, _1O/* sgHO */){
  switch(E(_1O/* sgHO */)){
    case 0:
      switch(E(_1M/* sgHM */)){
        case 0:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
      break;
    case 1:
      switch(E(_1M/* sgHM */)){
        case 0:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
      break;
    case 2:
      switch(E(_1M/* sgHM */)){
        case 0:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
      break;
    default:
      switch(E(_1M/* sgHM */)){
        case 0:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgHN */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
  }
},
_1P/* elemById_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function(id){return document.getElementById(id);})");
}),
_1Q/* f2 */ = new T(function(){
  return eval/* EXTERNAL */("(function(s,f){Haste[s] = f;})");
}),
_1R/* lvl2 */ = function(_/* EXTERNAL */){
  return new F(function(){return __jsNull/* EXTERNAL */();});
},
_1S/* unsafeDupablePerformIO */ = function(_1T/* s2YSa */){
  var _1U/* s2YSb */ = B(A1(_1T/* s2YSa */,_/* EXTERNAL */));
  return E(_1U/* s2YSb */);
},
_1V/* nullValue */ = new T(function(){
  return B(_1S/* GHC.IO.unsafeDupablePerformIO */(_1R/* Haste.Prim.Any.lvl2 */));
}),
_1W/* jsNull */ = new T(function(){
  return E(_1V/* Haste.Prim.Any.nullValue */);
}),
_1X/* maxInt */ = 2147483647,
_1Y/* minInt */ =  -2147483648,
_1Z/* $fRandomGenSMGen1 */ = new T2(0,_1Y/* GHC.Base.minInt */,_1X/* GHC.Base.maxInt */),
_20/* $fRandomGenSMGen0_$cgenRange */ = function(_21/* sixI */){
  return E(_1Z/* System.Random.Internal.$fRandomGenSMGen1 */);
},
_22/* $w$c+ */ = function(_23/* s1RvK */, _24/* s1RvL */){
  var _25/* s1RvN */ = hs_word64ToInt64/* EXTERNAL */(_24/* s1RvL */),
  _26/* s1RvR */ = hs_word64ToInt64/* EXTERNAL */(_23/* s1RvK */),
  _27/* s1RvV */ = hs_plusInt64/* EXTERNAL */(_26/* s1RvR */, _25/* s1RvN */),
  _28/* s1RvZ */ = hs_int64ToWord64/* EXTERNAL */(_27/* s1RvV */);
  return E(_28/* s1RvZ */);
},
_29/* $w$cshiftR */ = function(_2a/* s1Ryx */, _2b/* s1Ryy */){
  if(_2b/* s1Ryy */<64){
    var _2c/* s1RyC */ = hs_uncheckedShiftRL64/* EXTERNAL */(_2a/* s1Ryx */, _2b/* s1Ryy */);
    return E(_2c/* s1RyC */);
  }else{
    var _2d/* s1RyG */ = hs_wordToWord64/* EXTERNAL */(0);
    return E(_2d/* s1RyG */);
  }
},
_2e/* $w$c* */ = function(_2f/* s1RuW */, _2g/* s1RuX */){
  var _2h/* s1RuZ */ = hs_word64ToInt64/* EXTERNAL */(_2g/* s1RuX */),
  _2i/* s1Rv3 */ = hs_word64ToInt64/* EXTERNAL */(_2f/* s1RuW */),
  _2j/* s1Rv7 */ = hs_timesInt64/* EXTERNAL */(_2i/* s1Rv3 */, _2h/* s1RuZ */),
  _2k/* s1Rvb */ = hs_int64ToWord64/* EXTERNAL */(_2j/* s1Rv7 */);
  return E(_2k/* s1Rvb */);
},
_2l/* $wmix64 */ = function(_2m/* saYW */){
  var _2n/* saYZ */ = hs_xor64/* EXTERNAL */(_2m/* saYW */, B(_29/* GHC.Word.$w$cshiftR */(_2m/* saYW */, 33))),
  _2o/* saZ2 */ = B(_2e/* GHC.Word.$w$c* */(_2n/* saYZ */, new Long/* EXTERNAL */(3981806797, 4283543511, true))),
  _2p/* saZ5 */ = hs_xor64/* EXTERNAL */(_2o/* saZ2 */, B(_29/* GHC.Word.$w$cshiftR */(_2o/* saZ2 */, 33))),
  _2q/* saZ8 */ = B(_2e/* GHC.Word.$w$c* */(_2p/* saZ5 */, new Long/* EXTERNAL */(444984403, 3301882366, true))),
  _2r/* saZb */ = hs_xor64/* EXTERNAL */(_2q/* saZ8 */, B(_29/* GHC.Word.$w$cshiftR */(_2q/* saZ8 */, 33)));
  return E(_2r/* saZb */);
},
_2s/* () */ = 0,
_2t/* mix64 */ = function(_2u/* saZe */){
  return new F(function(){return _2l/* System.Random.SplitMix.$wmix64 */(E(_2u/* saZe */));});
},
_2v/* $fRandomGenSMGen0_$cgenShortByteString */ = function(_2w/* sixJ */, _2x/* sixK */){
  var _2y/* siz8 */ = function(_/* EXTERNAL */){
    var _2z/* sixN */ = E(_2w/* sixJ */),
    _2A/* sixP */ = function(_2B/* sixQ */){
      var _2C/* sixR */ = newByteArr/* EXTERNAL */(_2B/* sixQ */),
      _2D/* sixV */ = function(_2E/* sixW */, _2F/* sixX */, _2G/* sixY */, _/* EXTERNAL */){
        while(1){
          if(_2E/* sixW */>=quot/* EXTERNAL */(_2B/* sixQ */, 8)){
            return new T2(0,_2F/* sixX */,_2G/* sixY */);
          }else{
            var _2H/* siy3 */ = E(_2G/* sixY */),
            _2I/* siy5 */ = _2H/* siy3 */.b,
            _2J/* siy6 */ = E(_2F/* sixX */),
            _2K/* siy8 */ = B(_22/* GHC.Word.$w$c+ */(_2H/* siy3 */.a, _2I/* siy5 */)),
            _/* EXTERNAL */ = writeOffAddr64/* EXTERNAL */(_2J/* siy6 */, 0, B(_2l/* System.Random.SplitMix.$wmix64 */(_2K/* siy8 */))),
            _2L/*  sixW */ = _2E/* sixW */+1|0;
            _2E/* sixW */ = _2L/*  sixW */;
            _2F/* sixX */ = plusAddr/* EXTERNAL */(_2J/* siy6 */, 8);
            _2G/* sixY */ = new T2(0,_2K/* siy8 */,_2I/* siy5 */);
            _/* EXTERNAL */ = 0;
            continue;
          }
        }
      },
      _2M/* siyh */ = B(_2D/* sixV */(0, _2C/* sixR */, _2x/* sixK */, 0)),
      _2N/* siyk */ = E(_2M/* siyh */),
      _2O/* siym */ = _2N/* siyk */.b,
      _2P/* siyn */ = _2B/* sixQ */%8;
      if(_2P/* siyn */<=0){
        return new T2(0,_2C/* sixR */,_2O/* siym */);
      }else{
        var _2Q/* siyv */ = E(_2O/* siym */),
        _2R/* siyx */ = _2Q/* siyv */.b,
        _2S/* siyy */ = new T(function(){
          return B(_22/* GHC.Word.$w$c+ */(_2Q/* siyv */.a, _2R/* siyx */));
        }),
        _2T/* siyA */ = function(_2U/*  siyB */, _2V/*  siyC */, _/* EXTERNAL */){
          while(1){
            var _2W/*  siyA */ = B((function(_2X/* siyB */, _2Y/* siyC */, _/* EXTERNAL */){
              if(_2Y/* siyC */>=_2P/* siyn */){
                return _2s/* GHC.Tuple.() */;
              }else{
                var _2Z/* siyI */ = E(_2X/* siyB */),
                _30/* siyL */ = hs_word64ToWord/* EXTERNAL */(_2Z/* siyI */),
                _/* EXTERNAL */ = writeOffAddr/* EXTERNAL */("w8", 1, plusAddr/* EXTERNAL */(E(_2N/* siyk */.a), _2Y/* siyC */), 0, _30/* siyL */&255),
                _31/*   siyC */ = _2Y/* siyC */+1|0;
                _2U/*  siyB */ = new T(function(){
                  return B(_29/* GHC.Word.$w$cshiftR */(_2Z/* siyI */, 8));
                },1);
                _2V/*  siyC */ = _31/*   siyC */;
                _/* EXTERNAL */ = 0;
                return __continue/* EXTERNAL */;
              }
            })(_2U/*  siyB */, _2V/*  siyC */, _/* EXTERNAL */));
            if(_2W/*  siyA */!=__continue/* EXTERNAL */){
              return _2W/*  siyA */;
            }
          }
        },
        _32/* siyV */ = B(_2T/* siyA */(new T(function(){
          return B(_2t/* System.Random.SplitMix.mix64 */(_2S/* siyy */));
        },1), 0, 0));
        return new T2(0,_2C/* sixR */,new T(function(){
          return new T2(0,E(_2S/* siyy */),_2R/* siyx */);
        }));
      }
    };
    if(0>_2z/* sixN */){
      return new F(function(){return _2A/* sixP */(0);});
    }else{
      return new F(function(){return _2A/* sixP */(_2z/* sixN */);});
    }
  };
  return new F(function(){return _1S/* GHC.IO.unsafeDupablePerformIO */(_2y/* siz8 */);});
},
_33/* $wnextWord32 */ = function(_34/* sb0P */){
  var _35/* sb0Q */ = new T(function(){
    var _36/* sb0R */ = E(_34/* sb0P */),
    _37/* sb0T */ = _36/* sb0R */.b,
    _38/* sb0U */ = new T(function(){
      return B(_22/* GHC.Word.$w$c+ */(_36/* sb0R */.a, _37/* sb0T */));
    });
    return new T2(0,new T(function(){
      return B(_2t/* System.Random.SplitMix.mix64 */(_38/* sb0U */));
    }),new T(function(){
      return new T2(0,E(_38/* sb0U */),_37/* sb0T */);
    }));
  });
  return new T2(0,new T(function(){
    return hs_word64ToWord/* EXTERNAL */(E(E(_35/* sb0Q */).a));
  }),new T(function(){
    return E(E(_35/* sb0Q */).b);
  }));
},
_39/* $fRandomGenSMGen0_$cgenWord16 */ = function(_3a/* siA0 */){
  var _3b/* siA1 */ = new T(function(){
    var _3c/* siA2 */ = B(_33/* System.Random.SplitMix.$wnextWord32 */(_3a/* siA0 */));
    return new T2(0,_3c/* siA2 */.a,_3c/* siA2 */.b);
  });
  return new T2(0,new T(function(){
    return E(E(_3b/* siA1 */).a)&65535;
  }),new T(function(){
    return E(E(_3b/* siA1 */).b);
  }));
},
_3d/* $fExceptionArithException_ww2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("base"));
}),
_3e/* $fExceptionArithException_ww4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("GHC.Exception"));
}),
_3f/* $fExceptionArithException_ww5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("ArithException"));
}),
_3g/* $fExceptionArithException_wild */ = new T5(0,new Long/* EXTERNAL */(4194982440, 719304104, true),new Long/* EXTERNAL */(3110813675, 1843557400, true),_3d/* GHC.Exception.$fExceptionArithException_ww2 */,_3e/* GHC.Exception.$fExceptionArithException_ww4 */,_3f/* GHC.Exception.$fExceptionArithException_ww5 */),
_3h/* $fExceptionArithException8 */ = new T5(0,new Long/* EXTERNAL */(4194982440, 719304104, true),new Long/* EXTERNAL */(3110813675, 1843557400, true),_3g/* GHC.Exception.$fExceptionArithException_wild */,_4/* GHC.Types.[] */,_4/* GHC.Types.[] */),
_3i/* $fExceptionArithException7 */ = function(_3j/* s2VcG */){
  return E(_3h/* GHC.Exception.$fExceptionArithException8 */);
},
_3k/* $fExceptionArithException_$cfromException */ = function(_3l/* s2VcH */){
  var _3m/* s2VcI */ = E(_3l/* s2VcH */);
  return new F(function(){return _a/* Data.Typeable.cast */(B(_8/* GHC.Exception.$p1Exception */(_3m/* s2VcI */.a)), _3i/* GHC.Exception.$fExceptionArithException7 */, _3m/* s2VcI */.b);});
},
_3n/* $fExceptionArithException1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Ratio has zero denominator"));
}),
_3o/* $fExceptionArithException2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("denormal"));
}),
_3p/* $fExceptionArithException3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("divide by zero"));
}),
_3q/* $fExceptionArithException4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("loss of precision"));
}),
_3r/* $fExceptionArithException5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("arithmetic underflow"));
}),
_3s/* $fExceptionArithException6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("arithmetic overflow"));
}),
_3t/* $w$cshowsPrec */ = function(_3u/* s2VaQ */, _3v/* s2VaR */){
  switch(E(_3u/* s2VaQ */)){
    case 0:
      return new F(function(){return _q/* GHC.Base.++ */(_3s/* GHC.Exception.$fExceptionArithException6 */, _3v/* s2VaR */);});
      break;
    case 1:
      return new F(function(){return _q/* GHC.Base.++ */(_3r/* GHC.Exception.$fExceptionArithException5 */, _3v/* s2VaR */);});
      break;
    case 2:
      return new F(function(){return _q/* GHC.Base.++ */(_3q/* GHC.Exception.$fExceptionArithException4 */, _3v/* s2VaR */);});
      break;
    case 3:
      return new F(function(){return _q/* GHC.Base.++ */(_3p/* GHC.Exception.$fExceptionArithException3 */, _3v/* s2VaR */);});
      break;
    case 4:
      return new F(function(){return _q/* GHC.Base.++ */(_3o/* GHC.Exception.$fExceptionArithException2 */, _3v/* s2VaR */);});
      break;
    default:
      return new F(function(){return _q/* GHC.Base.++ */(_3n/* GHC.Exception.$fExceptionArithException1 */, _3v/* s2VaR */);});
  }
},
_3w/* $fExceptionArithException_$cshow */ = function(_3x/* s2VaY */){
  return new F(function(){return _3t/* GHC.Exception.$w$cshowsPrec */(_3x/* s2VaY */, _4/* GHC.Types.[] */);});
},
_3y/* $fExceptionArithException_$cshowsPrec */ = function(_3z/* s2VaT */, _3A/* s2VaU */, _3B/* s2VaV */){
  return new F(function(){return _3t/* GHC.Exception.$w$cshowsPrec */(_3A/* s2VaU */, _3B/* s2VaV */);});
},
_3C/* $fShowArithException_$cshowList */ = function(_3D/* s2VaW */, _3E/* s2VaX */){
  return new F(function(){return _A/* GHC.Show.showList__ */(_3t/* GHC.Exception.$w$cshowsPrec */, _3D/* s2VaW */, _3E/* s2VaX */);});
},
_3F/* $fShowArithException */ = new T3(0,_3y/* GHC.Exception.$fExceptionArithException_$cshowsPrec */,_3w/* GHC.Exception.$fExceptionArithException_$cshow */,_3C/* GHC.Exception.$fShowArithException_$cshowList */),
_3G/* $fExceptionArithException */ = new T(function(){
  return new T5(0,_3i/* GHC.Exception.$fExceptionArithException7 */,_3F/* GHC.Exception.$fShowArithException */,_3H/* GHC.Exception.$fExceptionArithException_$ctoException */,_3k/* GHC.Exception.$fExceptionArithException_$cfromException */,_3w/* GHC.Exception.$fExceptionArithException_$cshow */);
}),
_3H/* $fExceptionArithException_$ctoException */ = function(_11/* B1 */){
  return new T2(0,_3G/* GHC.Exception.$fExceptionArithException */,_11/* B1 */);
},
_3I/* DivideByZero */ = 3,
_3J/* divZeroException */ = new T(function(){
  return B(_3H/* GHC.Exception.$fExceptionArithException_$ctoException */(_3I/* GHC.Exception.DivideByZero */));
}),
_3K/* divZeroError */ = new T(function(){
  return die/* EXTERNAL */(_3J/* GHC.Exception.divZeroException */);
}),
_3L/* $w$sunbiasedWordMult32Exclusive1 */ = function(_3M/* sj9N */){
  var _3N/* sj9O */ = new T(function(){
    return hs_wordToWord64/* EXTERNAL */(E(_3M/* sj9N */));
  }),
  _3O/* sj9V */ = new T(function(){
    var _3P/* sj9Y */ = E(_3M/* sj9N */);
    if(!_3P/* sj9Y */){
      return E(_3K/* GHC.Real.divZeroError */);
    }else{
      return ( -(_3P/* sj9Y */&4294967295)>>>0)%_3P/* sj9Y */;
    }
  }),
  _3Q/* sja3 */ = function(_3R/*  sja4 */){
    while(1){
      var _3S/*  sja3 */ = B((function(_3T/* sja4 */){
        var _3U/* sja5 */ = B(_33/* System.Random.SplitMix.$wnextWord32 */(_3T/* sja4 */)),
        _3V/* sja7 */ = _3U/* sja5 */.b,
        _3W/* sjab */ = hs_wordToWord64/* EXTERNAL */(E(_3U/* sja5 */.a)),
        _3X/* sjag */ = B(_2e/* GHC.Word.$w$c* */(_3W/* sjab */, E(_3N/* sj9O */))),
        _3Y/* sjai */ = hs_word64ToWord/* EXTERNAL */(_3X/* sjag */);
        if(_3Y/* sjai */<E(_3O/* sj9V */)){
          _3R/*  sja4 */ = _3V/* sja7 */;
          return __continue/* EXTERNAL */;
        }else{
          return new T2(0,new T(function(){
            return hs_word64ToWord/* EXTERNAL */(B(_29/* GHC.Word.$w$cshiftR */(_3X/* sjag */, 32)));
          }),_3V/* sja7 */);
        }
      })(_3R/*  sja4 */));
      if(_3S/*  sja3 */!=__continue/* EXTERNAL */){
        return _3S/*  sja3 */;
      }
    }
  };
  return function(_3Z/* sjaw */){
    var _40/* sjax */ = B(_3Q/* sja3 */(_3Z/* sjaw */));
    return new T2(0,_40/* sjax */.a,_40/* sjax */.b);
  };
},
_41/* nextWord32 */ = function(_42/* sb1e */){
  var _43/* sb1f */ = B(_33/* System.Random.SplitMix.$wnextWord32 */(_42/* sb1e */));
  return new T2(0,_43/* sb1f */.a,_43/* sb1f */.b);
},
_44/* $w$cgenWord32R1 */ = function(_45/* sjaA */, _46/* sjaB */){
  var _47/* sjaC */ = E(_45/* sjaA */);
  if(_47/* sjaC */==4294967295){
    return new F(function(){return _41/* System.Random.SplitMix.nextWord32 */(_46/* sjaB */);});
  }else{
    return new F(function(){return A2(_3L/* System.Random.Internal.$w$sunbiasedWordMult32Exclusive1 */,_47/* sjaC */+1>>>0, _46/* sjaB */);});
  }
},
_48/* $fRandomGenSMGen0_$cgenWord32R */ = function(_49/* sjaF */, _4a/* sjaG */){
  return new F(function(){return _44/* System.Random.Internal.$w$cgenWord32R1 */(E(_49/* sjaF */), _4a/* sjaG */);});
},
_4b/* $fBitsWord64_$czeroBits */ = new T(function(){
  var _4c/* s1RAQ */ = hs_uncheckedShiftL64/* EXTERNAL */(new Long/* EXTERNAL */(1, 0, true), 0),
  _4d/* s1RAU */ = hs_not64/* EXTERNAL */(_4c/* s1RAQ */);
  return hs_and64/* EXTERNAL */(_4c/* s1RAQ */, _4d/* s1RAU */);
}),
_4e/* $w$cgenWord64R1 */ = function(_4f/* siz9 */, _4g/* siza */, _4h/* sizb */){
  var _4i/* sizf */ = hs_not64/* EXTERNAL */(E(_4b/* GHC.Word.$fBitsWord64_$czeroBits */)),
  _4j/* sizi */ = E(_4f/* siz9 */),
  _4k/* sizl */ = hs_or64/* EXTERNAL */(_4j/* sizi */, new Long/* EXTERNAL */(1, 0, true)),
  _4l/* sizo */ = die/* EXTERNAL */("Unsupported PrimOp: clz64#")&4294967295,
  _4m/* sizq */ = function(_4n/* sizr */){
    var _4o/* sizs */ = function(_4p/* sizt */, _4q/* sizu */){
      while(1){
        var _4r/* sizv */ = B(_22/* GHC.Word.$w$c+ */(_4p/* sizt */, _4q/* sizu */)),
        _4s/* sizy */ = hs_and64/* EXTERNAL */(B(_2l/* System.Random.SplitMix.$wmix64 */(_4r/* sizv */)), _4n/* sizr */),
        _4t/* sizC */ = hs_gtWord64/* EXTERNAL */(_4s/* sizy */, _4j/* sizi */);
        if(!_4t/* sizC */){
          return new T2(0,_4s/* sizy */,new T2(0,_4r/* sizv */,_4q/* sizu */));
        }else{
          _4p/* sizt */ = _4r/* sizv */;
          continue;
        }
      }
    };
    return new F(function(){return _4o/* sizs */(_4g/* siza */, _4h/* sizb */);});
  };
  if(_4l/* sizo */<64){
    var _4u/* sizL */ = hs_uncheckedShiftRL64/* EXTERNAL */(_4i/* sizf */, _4l/* sizo */);
    return new F(function(){return _4m/* sizq */(_4u/* sizL */);});
  }else{
    var _4v/* sizP */ = hs_wordToWord64/* EXTERNAL */(0);
    return new F(function(){return _4m/* sizq */(_4v/* sizP */);});
  }
},
_4w/* $fRandomGenSMGen0_$cgenWord64R */ = function(_4x/* sizS */, _4y/* sizT */){
  var _4z/* sizU */ = E(_4y/* sizT */),
  _4A/* sizX */ = B(_4e/* System.Random.Internal.$w$cgenWord64R1 */(_4x/* sizS */, _4z/* sizU */.a, _4z/* sizU */.b));
  return new T2(0,_4A/* sizX */.a,_4A/* sizX */.b);
},
_4B/* $fRandomGenSMGen0_$cgenWord8 */ = function(_4C/* siAg */){
  var _4D/* siAh */ = new T(function(){
    var _4E/* siAi */ = B(_33/* System.Random.SplitMix.$wnextWord32 */(_4C/* siAg */));
    return new T2(0,_4E/* siAi */.a,_4E/* siAi */.b);
  });
  return new T2(0,new T(function(){
    return E(E(_4D/* siAh */).a)&255;
  }),new T(function(){
    return E(E(_4D/* siAh */).b);
  }));
},
_4F/* $wnextInt */ = function(_4G/* sb1V */, _4H/* sb1W */){
  var _4I/* sb1X */ = new T(function(){
    return B(_22/* GHC.Word.$w$c+ */(_4G/* sb1V */, _4H/* sb1W */));
  });
  return new T2(0,new T(function(){
    var _4J/* sb23 */ = hs_word64ToWord/* EXTERNAL */(B(_2l/* System.Random.SplitMix.$wmix64 */(E(_4I/* sb1X */))));
    return _4J/* sb23 */&4294967295;
  }),new T(function(){
    return new T2(0,E(_4I/* sb1X */),_4H/* sb1W */);
  }));
},
_4K/* nextInt */ = function(_4L/* sb2b */){
  var _4M/* sb2c */ = E(_4L/* sb2b */),
  _4N/* sb2f */ = B(_4F/* System.Random.SplitMix.$wnextInt */(_4M/* sb2c */.a, _4M/* sb2c */.b));
  return new T2(0,_4N/* sb2f */.a,_4N/* sb2f */.b);
},
_4O/* nextWord64 */ = function(_4P/* sb0F */){
  var _4Q/* sb0G */ = E(_4P/* sb0F */),
  _4R/* sb0I */ = _4Q/* sb0G */.b,
  _4S/* sb0J */ = new T(function(){
    return B(_22/* GHC.Word.$w$c+ */(_4Q/* sb0G */.a, _4R/* sb0I */));
  });
  return new T2(0,new T(function(){
    return B(_2t/* System.Random.SplitMix.mix64 */(_4S/* sb0J */));
  }),new T(function(){
    return new T2(0,E(_4S/* sb0J */),_4R/* sb0I */);
  }));
},
_4T/* $wmixGamma */ = function(_4U/* saYn */){
  var _4V/* saYq */ = hs_xor64/* EXTERNAL */(_4U/* saYn */, B(_29/* GHC.Word.$w$cshiftR */(_4U/* saYn */, 30))),
  _4W/* saYt */ = B(_2e/* GHC.Word.$w$c* */(_4V/* saYq */, new Long/* EXTERNAL */(484763065, 3210233709, true))),
  _4X/* saYw */ = hs_xor64/* EXTERNAL */(_4W/* saYt */, B(_29/* GHC.Word.$w$cshiftR */(_4W/* saYt */, 27))),
  _4Y/* saYz */ = B(_2e/* GHC.Word.$w$c* */(_4X/* saYw */, new Long/* EXTERNAL */(321982955, 2496678331, true))),
  _4Z/* saYC */ = hs_xor64/* EXTERNAL */(_4Y/* saYz */, B(_29/* GHC.Word.$w$cshiftR */(_4Y/* saYz */, 31))),
  _50/* saYG */ = hs_or64/* EXTERNAL */(_4Z/* saYC */, new Long/* EXTERNAL */(1, 0, true)),
  _51/* saYL */ = hs_xor64/* EXTERNAL */(_50/* saYG */, B(_29/* GHC.Word.$w$cshiftR */(_50/* saYG */, 1)));
  if((popCnt64/* EXTERNAL */(_51/* saYL */)&4294967295)<24){
    var _52/* saYT */ = hs_xor64/* EXTERNAL */(_50/* saYG */, new Long/* EXTERNAL */(2863311530, 2863311530, true));
    return E(_52/* saYT */);
  }else{
    return E(_50/* saYG */);
  }
},
_53/* $wsplitSMGen */ = function(_54/* saZP */, _55/* saZQ */){
  var _56/* saZR */ = new T(function(){
    return B(_22/* GHC.Word.$w$c+ */(_54/* saZP */, _55/* saZQ */));
  }),
  _57/* saZT */ = new T(function(){
    return B(_22/* GHC.Word.$w$c+ */(E(_56/* saZR */), _55/* saZQ */));
  });
  return new T2(0,new T(function(){
    return new T2(0,E(_57/* saZT */),_55/* saZQ */);
  }),new T(function(){
    return new T2(0,B(_2l/* System.Random.SplitMix.$wmix64 */(E(_56/* saZR */))),B(_4T/* System.Random.SplitMix.$wmixGamma */(E(_57/* saZT */))));
  }));
},
_58/* splitSMGen */ = function(_59/* sb07 */){
  var _5a/* sb08 */ = E(_59/* sb07 */),
  _5b/* sb0b */ = B(_53/* System.Random.SplitMix.$wsplitSMGen */(_5a/* sb08 */.a, _5a/* sb08 */.b));
  return new T2(0,_5b/* sb0b */.a,_5b/* sb0b */.b);
},
_5c/* $fRandomGenStdGen */ = {_:0,a:_4K/* System.Random.SplitMix.nextInt */,b:_4B/* System.Random.Internal.$fRandomGenSMGen0_$cgenWord8 */,c:_39/* System.Random.Internal.$fRandomGenSMGen0_$cgenWord16 */,d:_41/* System.Random.SplitMix.nextWord32 */,e:_4O/* System.Random.SplitMix.nextWord64 */,f:_48/* System.Random.Internal.$fRandomGenSMGen0_$cgenWord32R */,g:_4w/* System.Random.Internal.$fRandomGenSMGen0_$cgenWord64R */,h:_2v/* System.Random.Internal.$fRandomGenSMGen0_$cgenShortByteString */,i:_20/* System.Random.Internal.$fRandomGenSMGen0_$cgenRange */,j:_58/* System.Random.SplitMix.splitSMGen */},
_5d/* genWord32 */ = function(_5e/* sif0 */){
  return E(E(_5e/* sif0 */).d);
},
_5f/* $w$crandomR12 */ = function(_5g/* szhd */, _5h/* szhe */, _5i/* szhf */, _5j/* szhg */){
  if(_5h/* szhe */!=_5i/* szhf */){
    var _5k/* szhj */ = function(_5l/* szhk */, _5m/* szhl */){
      var _5n/* szhm */ = E(_5m/* szhl */),
      _5o/* szho */ = __clz/* EXTERNAL */(32, (_5n/* szhm */|1)>>>0)&4294967295,
      _5p/* szhr */ = function(_5q/* szhs */){
        var _5r/* szht */ = function(_5s/* szhu */){
          while(1){
            var _5t/* szhv */ = B(A2(_5d/* System.Random.Internal.genWord32 */,_5g/* szhd */, _5s/* szhu */)),
            _5u/* szhx */ = _5t/* szhv */.b,
            _5v/* szhA */ = (E(_5t/* szhv */.a)&_5q/* szhs */)>>>0;
            if(_5v/* szhA */<=_5n/* szhm */){
              return new T2(0,_5v/* szhA */,_5u/* szhx */);
            }else{
              _5s/* szhu */ = _5u/* szhx */;
              continue;
            }
          }
        },
        _5w/* szhE */ = B(_5r/* szht */(_5j/* szhg */));
        return new T2(0,new T(function(){
          return E(_5l/* szhk */)+(E(_5w/* szhE */.a)&4294967295)|0;
        }),_5w/* szhE */.b);
      };
      if(_5o/* szho */<32){
        return new F(function(){return _5p/* szhr */(4294967295>>>_5o/* szho */);});
      }else{
        return new F(function(){return _5p/* szhr */(0);});
      }
    };
    if(_5h/* szhe */<=_5i/* szhf */){
      return new F(function(){return _5k/* szhj */(_5h/* szhe */, (_5i/* szhf */>>>0)-(_5h/* szhe */>>>0)>>>0);});
    }else{
      return new F(function(){return _5k/* szhj */(_5i/* szhf */, (_5h/* szhe */>>>0)-(_5i/* szhf */>>>0)>>>0);});
    }
  }else{
    return new T2(0,_5i/* szhf */,_5j/* szhg */);
  }
},
_5x/* lvl4 */ = function(_5y/* ssPz */){
  var _5z/* ssPA */ = B(_5f/* System.Random.$w$crandomR12 */(_5c/* System.Random.Internal.$fRandomGenStdGen */, 1, 4, _5y/* ssPz */));
  return new T2(0,E(_5z/* ssPA */.b),_5z/* ssPA */.a);
},
_5A/* lvl25 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Integer must be in the range [1, 4]"));
}),
_5B/* numToColor1 */ = new T(function(){
  return B(err/* EXTERNAL */(_5A/* LudoJS.lvl25 */));
}),
_5C/* $fEventMouseEvent2 */ = "deltaZ",
_5D/* $fEventMouseEvent3 */ = "deltaY",
_5E/* $fEventMouseEvent4 */ = "deltaX",
_5F/* itos */ = function(_5G/* sf6u */, _5H/* sf6v */){
  var _5I/* sf6x */ = jsShowI/* EXTERNAL */(_5G/* sf6u */);
  return new F(function(){return _q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(_5I/* sf6x */), _5H/* sf6v */);});
},
_5J/* shows7 */ = 41,
_5K/* shows8 */ = 40,
_5L/* $wshowSignedInt */ = function(_5M/* sf6C */, _5N/* sf6D */, _5O/* sf6E */){
  if(_5N/* sf6D */>=0){
    return new F(function(){return _5F/* GHC.Show.itos */(_5N/* sf6D */, _5O/* sf6E */);});
  }else{
    if(_5M/* sf6C */<=6){
      return new F(function(){return _5F/* GHC.Show.itos */(_5N/* sf6D */, _5O/* sf6E */);});
    }else{
      return new T2(1,_5K/* GHC.Show.shows8 */,new T(function(){
        var _5P/* sf6K */ = jsShowI/* EXTERNAL */(_5N/* sf6D */);
        return B(_q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(_5P/* sf6K */), new T2(1,_5J/* GHC.Show.shows7 */,_5O/* sf6E */)));
      }));
    }
  }
},
_5Q/* lvl2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */(")"));
}),
_5R/* lvl3 */ = new T(function(){
  return B(_5L/* GHC.Show.$wshowSignedInt */(0, 2, _5Q/* Haste.Events.MouseEvents.lvl2 */));
}),
_5S/* lvl4 */ = new T(function(){
  return B(unAppCStr/* EXTERNAL */(") is outside of enumeration\'s range (0,", _5R/* Haste.Events.MouseEvents.lvl3 */));
}),
_5T/* $fEnumMouseButton1 */ = function(_5U/* sr4R */){
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("toEnum{MouseButton}: tag (", new T(function(){
    return B(_5L/* GHC.Show.$wshowSignedInt */(0, _5U/* sr4R */, _5S/* Haste.Events.MouseEvents.lvl4 */));
  }))));});
},
_5V/* $fEventMouseEvent5 */ = function(_5W/* sr7q */, _/* EXTERNAL */){
  return new T(function(){
    var _5X/* sr7v */ = Number/* EXTERNAL */(E(_5W/* sr7q */)),
    _5Y/* sr7z */ = jsTrunc/* EXTERNAL */(_5X/* sr7v */);
    if(_5Y/* sr7z */<0){
      return B(_5T/* Haste.Events.MouseEvents.$fEnumMouseButton1 */(_5Y/* sr7z */));
    }else{
      if(_5Y/* sr7z */>2){
        return B(_5T/* Haste.Events.MouseEvents.$fEnumMouseButton1 */(_5Y/* sr7z */));
      }else{
        return _5Y/* sr7z */;
      }
    }
  });
},
_5Z/* $fEventMouseEvent7 */ = 0,
_60/* $fEventMouseEvent6 */ = new T3(0,_5Z/* Haste.Events.MouseEvents.$fEventMouseEvent7 */,_5Z/* Haste.Events.MouseEvents.$fEventMouseEvent7 */,_5Z/* Haste.Events.MouseEvents.$fEventMouseEvent7 */),
_61/* $fEventMouseEvent8 */ = "button",
_62/* $fEventMouseEvent_f1 */ = new T(function(){
  return eval/* EXTERNAL */("jsGetMouseCoords");
}),
_63/* $fFromAnyInt2 */ = function(_64/* sc5b */, _/* EXTERNAL */){
  var _65/* sc5d */ = E(_64/* sc5b */);
  if(!_65/* sc5d */._){
    return _4/* GHC.Types.[] */;
  }else{
    var _66/* sc5g */ = B(_63/* Haste.Prim.Any.$fFromAnyInt2 */(_65/* sc5d */.b, _/* EXTERNAL */));
    return new T2(1,new T(function(){
      var _67/* sc5m */ = Number/* EXTERNAL */(E(_65/* sc5d */.a));
      return jsTrunc/* EXTERNAL */(_67/* sc5m */);
    }),_66/* sc5g */);
  }
},
_68/* $wa22 */ = function(_69/* sc5v */, _/* EXTERNAL */){
  var _6a/* sc5y */ = __arr2lst/* EXTERNAL */(0, _69/* sc5v */);
  return new F(function(){return _63/* Haste.Prim.Any.$fFromAnyInt2 */(_6a/* sc5y */, _/* EXTERNAL */);});
},
_6b/* $fFromAnyInt1 */ = function(_6c/* sc5C */, _/* EXTERNAL */){
  return new F(function(){return _68/* Haste.Prim.Any.$wa22 */(E(_6c/* sc5C */), _/* EXTERNAL */);});
},
_6d/* $fFromAnyInt3 */ = function(_6e/* sbVc */, _/* EXTERNAL */){
  return new T(function(){
    var _6f/* sbVh */ = Number/* EXTERNAL */(E(_6e/* sbVc */));
    return jsTrunc/* EXTERNAL */(_6f/* sbVh */);
  });
},
_6g/* $fFromAnyInt */ = new T2(0,_6d/* Haste.Prim.Any.$fFromAnyInt3 */,_6b/* Haste.Prim.Any.$fFromAnyInt1 */),
_6h/* $fFromAny(,)2 */ = function(_6i/* schQ */, _/* EXTERNAL */){
  var _6j/* schS */ = E(_6i/* schQ */);
  if(!_6j/* schS */._){
    return _4/* GHC.Types.[] */;
  }else{
    var _6k/* schV */ = B(_6h/* Haste.Prim.Any.$fFromAny(,)2 */(_6j/* schS */.b, _/* EXTERNAL */));
    return new T2(1,_6j/* schS */.a,_6k/* schV */);
  }
},
_6l/* $fExceptionAllocationLimitExceeded_ww2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("base"));
}),
_6m/* $fExceptionAllocationLimitExceeded_ww4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("GHC.IO.Exception"));
}),
_6n/* $fExceptionIOException_ww4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("IOException"));
}),
_6o/* $fExceptionIOException_wild */ = new T5(0,new Long/* EXTERNAL */(4053623282, 1685460941, true),new Long/* EXTERNAL */(3693590983, 2507416641, true),_6l/* GHC.IO.Exception.$fExceptionAllocationLimitExceeded_ww2 */,_6m/* GHC.IO.Exception.$fExceptionAllocationLimitExceeded_ww4 */,_6n/* GHC.IO.Exception.$fExceptionIOException_ww4 */),
_6p/* $fExceptionIOException4 */ = new T5(0,new Long/* EXTERNAL */(4053623282, 1685460941, true),new Long/* EXTERNAL */(3693590983, 2507416641, true),_6o/* GHC.IO.Exception.$fExceptionIOException_wild */,_4/* GHC.Types.[] */,_4/* GHC.Types.[] */),
_6q/* $fExceptionIOException3 */ = function(_6r/* s3MW8 */){
  return E(_6p/* GHC.IO.Exception.$fExceptionIOException4 */);
},
_6s/* $fExceptionIOException_$cfromException */ = function(_6t/* s3N1e */){
  var _6u/* s3N1f */ = E(_6t/* s3N1e */);
  return new F(function(){return _a/* Data.Typeable.cast */(B(_8/* GHC.Exception.$p1Exception */(_6u/* s3N1f */.a)), _6q/* GHC.IO.Exception.$fExceptionIOException3 */, _6u/* s3N1f */.b);});
},
_6v/* $fExceptionArrayException2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */(": "));
}),
_6w/* $fExceptionIOException1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */(")"));
}),
_6x/* $fExceptionIOException2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */(" ("));
}),
_6y/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */("interrupted"));
}),
_6z/* lvl1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("resource vanished"));
}),
_6A/* lvl10 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("unsatisified constraints"));
}),
_6B/* lvl11 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("user error"));
}),
_6C/* lvl12 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("permission denied"));
}),
_6D/* lvl13 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("illegal operation"));
}),
_6E/* lvl14 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("end of file"));
}),
_6F/* lvl15 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("resource exhausted"));
}),
_6G/* lvl16 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("resource busy"));
}),
_6H/* lvl17 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("does not exist"));
}),
_6I/* lvl18 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("already exists"));
}),
_6J/* lvl2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("timeout"));
}),
_6K/* lvl3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("unsupported operation"));
}),
_6L/* lvl4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("hardware fault"));
}),
_6M/* lvl5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("inappropriate type"));
}),
_6N/* lvl6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("invalid argument"));
}),
_6O/* lvl7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("failed"));
}),
_6P/* lvl8 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("protocol error"));
}),
_6Q/* lvl9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("system error"));
}),
_6R/* $w$cshowsPrec3 */ = function(_6S/* s3N03 */, _6T/* s3N04 */){
  switch(E(_6S/* s3N03 */)){
    case 0:
      return new F(function(){return _q/* GHC.Base.++ */(_6I/* GHC.IO.Exception.lvl18 */, _6T/* s3N04 */);});
      break;
    case 1:
      return new F(function(){return _q/* GHC.Base.++ */(_6H/* GHC.IO.Exception.lvl17 */, _6T/* s3N04 */);});
      break;
    case 2:
      return new F(function(){return _q/* GHC.Base.++ */(_6G/* GHC.IO.Exception.lvl16 */, _6T/* s3N04 */);});
      break;
    case 3:
      return new F(function(){return _q/* GHC.Base.++ */(_6F/* GHC.IO.Exception.lvl15 */, _6T/* s3N04 */);});
      break;
    case 4:
      return new F(function(){return _q/* GHC.Base.++ */(_6E/* GHC.IO.Exception.lvl14 */, _6T/* s3N04 */);});
      break;
    case 5:
      return new F(function(){return _q/* GHC.Base.++ */(_6D/* GHC.IO.Exception.lvl13 */, _6T/* s3N04 */);});
      break;
    case 6:
      return new F(function(){return _q/* GHC.Base.++ */(_6C/* GHC.IO.Exception.lvl12 */, _6T/* s3N04 */);});
      break;
    case 7:
      return new F(function(){return _q/* GHC.Base.++ */(_6B/* GHC.IO.Exception.lvl11 */, _6T/* s3N04 */);});
      break;
    case 8:
      return new F(function(){return _q/* GHC.Base.++ */(_6A/* GHC.IO.Exception.lvl10 */, _6T/* s3N04 */);});
      break;
    case 9:
      return new F(function(){return _q/* GHC.Base.++ */(_6Q/* GHC.IO.Exception.lvl9 */, _6T/* s3N04 */);});
      break;
    case 10:
      return new F(function(){return _q/* GHC.Base.++ */(_6P/* GHC.IO.Exception.lvl8 */, _6T/* s3N04 */);});
      break;
    case 11:
      return new F(function(){return _q/* GHC.Base.++ */(_6O/* GHC.IO.Exception.lvl7 */, _6T/* s3N04 */);});
      break;
    case 12:
      return new F(function(){return _q/* GHC.Base.++ */(_6N/* GHC.IO.Exception.lvl6 */, _6T/* s3N04 */);});
      break;
    case 13:
      return new F(function(){return _q/* GHC.Base.++ */(_6M/* GHC.IO.Exception.lvl5 */, _6T/* s3N04 */);});
      break;
    case 14:
      return new F(function(){return _q/* GHC.Base.++ */(_6L/* GHC.IO.Exception.lvl4 */, _6T/* s3N04 */);});
      break;
    case 15:
      return new F(function(){return _q/* GHC.Base.++ */(_6K/* GHC.IO.Exception.lvl3 */, _6T/* s3N04 */);});
      break;
    case 16:
      return new F(function(){return _q/* GHC.Base.++ */(_6J/* GHC.IO.Exception.lvl2 */, _6T/* s3N04 */);});
      break;
    case 17:
      return new F(function(){return _q/* GHC.Base.++ */(_6z/* GHC.IO.Exception.lvl1 */, _6T/* s3N04 */);});
      break;
    default:
      return new F(function(){return _q/* GHC.Base.++ */(_6y/* GHC.IO.Exception.lvl */, _6T/* s3N04 */);});
  }
},
_6U/* showHandle1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("}"));
}),
_6V/* showHandle2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("{handle: "));
}),
_6W/* $w$cshowsPrec2 */ = function(_6X/* s3N0c */, _6Y/* s3N0d */, _6Z/* s3N0e */, _70/* s3N0f */, _71/* s3N0g */, _72/* s3N0h */){
  var _73/* s3N0i */ = new T(function(){
    var _74/* s3N0j */ = new T(function(){
      var _75/* s3N0p */ = new T(function(){
        var _76/* s3N0k */ = E(_70/* s3N0f */);
        if(!_76/* s3N0k */._){
          return E(_72/* s3N0h */);
        }else{
          var _77/* s3N0o */ = new T(function(){
            return B(_q/* GHC.Base.++ */(_76/* s3N0k */, new T(function(){
              return B(_q/* GHC.Base.++ */(_6w/* GHC.IO.Exception.$fExceptionIOException1 */, _72/* s3N0h */));
            },1)));
          },1);
          return B(_q/* GHC.Base.++ */(_6x/* GHC.IO.Exception.$fExceptionIOException2 */, _77/* s3N0o */));
        }
      },1);
      return B(_6R/* GHC.IO.Exception.$w$cshowsPrec3 */(_6Y/* s3N0d */, _75/* s3N0p */));
    }),
    _78/* s3N0q */ = E(_6Z/* s3N0e */);
    if(!_78/* s3N0q */._){
      return E(_74/* s3N0j */);
    }else{
      return B(_q/* GHC.Base.++ */(_78/* s3N0q */, new T(function(){
        return B(_q/* GHC.Base.++ */(_6v/* GHC.IO.Exception.$fExceptionArrayException2 */, _74/* s3N0j */));
      },1)));
    }
  }),
  _79/* s3N0u */ = E(_71/* s3N0g */);
  if(!_79/* s3N0u */._){
    var _7a/* s3N0v */ = E(_6X/* s3N0c */);
    if(!_7a/* s3N0v */._){
      return E(_73/* s3N0i */);
    }else{
      var _7b/* s3N0x */ = E(_7a/* s3N0v */.a);
      if(!_7b/* s3N0x */._){
        var _7c/* s3N0C */ = new T(function(){
          var _7d/* s3N0B */ = new T(function(){
            return B(_q/* GHC.Base.++ */(_6U/* GHC.IO.Handle.Types.showHandle1 */, new T(function(){
              return B(_q/* GHC.Base.++ */(_6v/* GHC.IO.Exception.$fExceptionArrayException2 */, _73/* s3N0i */));
            },1)));
          },1);
          return B(_q/* GHC.Base.++ */(_7b/* s3N0x */.a, _7d/* s3N0B */));
        },1);
        return new F(function(){return _q/* GHC.Base.++ */(_6V/* GHC.IO.Handle.Types.showHandle2 */, _7c/* s3N0C */);});
      }else{
        var _7e/* s3N0I */ = new T(function(){
          var _7f/* s3N0H */ = new T(function(){
            return B(_q/* GHC.Base.++ */(_6U/* GHC.IO.Handle.Types.showHandle1 */, new T(function(){
              return B(_q/* GHC.Base.++ */(_6v/* GHC.IO.Exception.$fExceptionArrayException2 */, _73/* s3N0i */));
            },1)));
          },1);
          return B(_q/* GHC.Base.++ */(_7b/* s3N0x */.a, _7f/* s3N0H */));
        },1);
        return new F(function(){return _q/* GHC.Base.++ */(_6V/* GHC.IO.Handle.Types.showHandle2 */, _7e/* s3N0I */);});
      }
    }
  }else{
    return new F(function(){return _q/* GHC.Base.++ */(_79/* s3N0u */.a, new T(function(){
      return B(_q/* GHC.Base.++ */(_6v/* GHC.IO.Exception.$fExceptionArrayException2 */, _73/* s3N0i */));
    },1));});
  }
},
_7g/* $fExceptionIOException_$cshow */ = function(_7h/* s3N16 */){
  var _7i/* s3N17 */ = E(_7h/* s3N16 */);
  return new F(function(){return _6W/* GHC.IO.Exception.$w$cshowsPrec2 */(_7i/* s3N17 */.a, _7i/* s3N17 */.b, _7i/* s3N17 */.c, _7i/* s3N17 */.d, _7i/* s3N17 */.f, _4/* GHC.Types.[] */);});
},
_7j/* $fExceptionIOException_$cshowsPrec */ = function(_7k/* s3N0L */, _7l/* s3N0M */, _7m/* s3N0N */){
  var _7n/* s3N0O */ = E(_7l/* s3N0M */);
  return new F(function(){return _6W/* GHC.IO.Exception.$w$cshowsPrec2 */(_7n/* s3N0O */.a, _7n/* s3N0O */.b, _7n/* s3N0O */.c, _7n/* s3N0O */.d, _7n/* s3N0O */.f, _7m/* s3N0N */);});
},
_7o/* $s$dmshow9 */ = function(_7p/* s3N0V */, _7q/* s3N0W */){
  var _7r/* s3N0X */ = E(_7p/* s3N0V */);
  return new F(function(){return _6W/* GHC.IO.Exception.$w$cshowsPrec2 */(_7r/* s3N0X */.a, _7r/* s3N0X */.b, _7r/* s3N0X */.c, _7r/* s3N0X */.d, _7r/* s3N0X */.f, _7q/* s3N0W */);});
},
_7s/* $fShowIOException_$cshowList */ = function(_7t/* s3N14 */, _7u/* s3N15 */){
  return new F(function(){return _A/* GHC.Show.showList__ */(_7o/* GHC.IO.Exception.$s$dmshow9 */, _7t/* s3N14 */, _7u/* s3N15 */);});
},
_7v/* $fShowIOException */ = new T3(0,_7j/* GHC.IO.Exception.$fExceptionIOException_$cshowsPrec */,_7g/* GHC.IO.Exception.$fExceptionIOException_$cshow */,_7s/* GHC.IO.Exception.$fShowIOException_$cshowList */),
_7w/* $fExceptionIOException */ = new T(function(){
  return new T5(0,_6q/* GHC.IO.Exception.$fExceptionIOException3 */,_7v/* GHC.IO.Exception.$fShowIOException */,_7x/* GHC.IO.Exception.$fExceptionIOException_$ctoException */,_6s/* GHC.IO.Exception.$fExceptionIOException_$cfromException */,_7g/* GHC.IO.Exception.$fExceptionIOException_$cshow */);
}),
_7x/* $fExceptionIOException_$ctoException */ = function(_7y/* B1 */){
  return new T2(0,_7w/* GHC.IO.Exception.$fExceptionIOException */,_7y/* B1 */);
},
_7z/* Nothing */ = __Z/* EXTERNAL */,
_7A/* UserError */ = 7,
_7B/* lvl25 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Pattern match failure in do expression at src\\Haste\\Prim\\Any.hs:268:5-9"));
}),
_7C/* lvl26 */ = new T6(0,_7z/* GHC.Base.Nothing */,_7A/* GHC.IO.Exception.UserError */,_4/* GHC.Types.[] */,_7B/* Haste.Prim.Any.lvl25 */,_7z/* GHC.Base.Nothing */,_7z/* GHC.Base.Nothing */),
_7D/* lvl27 */ = new T(function(){
  return B(_7x/* GHC.IO.Exception.$fExceptionIOException_$ctoException */(_7C/* Haste.Prim.Any.lvl26 */));
}),
_7E/* $wa3 */ = function(_/* EXTERNAL */){
  return new F(function(){return die/* EXTERNAL */(_7D/* Haste.Prim.Any.lvl27 */);});
},
_7F/* fromAny */ = function(_7G/* sbDR */){
  return E(E(_7G/* sbDR */).a);
},
_7H/* $wa2 */ = function(_7I/* schZ */, _7J/* sci0 */, _7K/* sci1 */, _/* EXTERNAL */){
  var _7L/* sci4 */ = __arr2lst/* EXTERNAL */(0, _7K/* sci1 */),
  _7M/* sci8 */ = B(_6h/* Haste.Prim.Any.$fFromAny(,)2 */(_7L/* sci4 */, _/* EXTERNAL */)),
  _7N/* scib */ = E(_7M/* sci8 */);
  if(!_7N/* scib */._){
    return new F(function(){return _7E/* Haste.Prim.Any.$wa3 */(_/* EXTERNAL */);});
  }else{
    var _7O/* scie */ = E(_7N/* scib */.b);
    if(!_7O/* scie */._){
      return new F(function(){return _7E/* Haste.Prim.Any.$wa3 */(_/* EXTERNAL */);});
    }else{
      if(!E(_7O/* scie */.b)._){
        var _7P/* scii */ = B(A3(_7F/* Haste.Prim.Any.fromAny */,_7I/* schZ */, _7N/* scib */.a, _/* EXTERNAL */)),
        _7Q/* scil */ = B(A3(_7F/* Haste.Prim.Any.fromAny */,_7J/* sci0 */, _7O/* scie */.a, _/* EXTERNAL */));
        return new T2(0,_7P/* scii */,_7Q/* scil */);
      }else{
        return new F(function(){return _7E/* Haste.Prim.Any.$wa3 */(_/* EXTERNAL */);});
      }
    }
  }
},
_7R/* $wa */ = function(_7S/* sr8b */, _7T/* sr8c */, _/* EXTERNAL */){
  if(E(_7S/* sr8b */)==7){
    var _7U/* sr90 */ = __app1/* EXTERNAL */(E(_62/* Haste.Events.MouseEvents.$fEventMouseEvent_f1 */), _7T/* sr8c */),
    _7V/* sr93 */ = B(_7H/* Haste.Prim.Any.$wa2 */(_6g/* Haste.Prim.Any.$fFromAnyInt */, _6g/* Haste.Prim.Any.$fFromAnyInt */, _7U/* sr90 */, _/* EXTERNAL */)),
    _7W/* sr99 */ = __get/* EXTERNAL */(_7T/* sr8c */, E(_5E/* Haste.Events.MouseEvents.$fEventMouseEvent4 */)),
    _7X/* sr9f */ = __get/* EXTERNAL */(_7T/* sr8c */, E(_5D/* Haste.Events.MouseEvents.$fEventMouseEvent3 */)),
    _7Y/* sr9l */ = __get/* EXTERNAL */(_7T/* sr8c */, E(_5C/* Haste.Events.MouseEvents.$fEventMouseEvent2 */));
    return new T(function(){
      return new T3(0,E(_7V/* sr93 */),E(_7z/* GHC.Base.Nothing */),E(new T3(0,_7W/* sr99 */,_7X/* sr9f */,_7Y/* sr9l */)));
    });
  }else{
    var _7Z/* sr8i */ = __app1/* EXTERNAL */(E(_62/* Haste.Events.MouseEvents.$fEventMouseEvent_f1 */), _7T/* sr8c */),
    _80/* sr8l */ = B(_7H/* Haste.Prim.Any.$wa2 */(_6g/* Haste.Prim.Any.$fFromAnyInt */, _6g/* Haste.Prim.Any.$fFromAnyInt */, _7Z/* sr8i */, _/* EXTERNAL */)),
    _81/* sr8r */ = __get/* EXTERNAL */(_7T/* sr8c */, E(_61/* Haste.Events.MouseEvents.$fEventMouseEvent8 */)),
    _82/* sr8x */ = __eq/* EXTERNAL */(_81/* sr8r */, E(_1W/* Haste.Prim.Any.jsNull */));
    if(!E(_82/* sr8x */)){
      var _83/* sr8G */ = __isUndef/* EXTERNAL */(_81/* sr8r */);
      if(!E(_83/* sr8G */)){
        var _84/* sr8P */ = B(_5V/* Haste.Events.MouseEvents.$fEventMouseEvent5 */(_81/* sr8r */, _/* EXTERNAL */));
        return new T(function(){
          return new T3(0,E(_80/* sr8l */),E(new T1(1,_84/* sr8P */)),E(_60/* Haste.Events.MouseEvents.$fEventMouseEvent6 */));
        });
      }else{
        return new T(function(){
          return new T3(0,E(_80/* sr8l */),E(_7z/* GHC.Base.Nothing */),E(_60/* Haste.Events.MouseEvents.$fEventMouseEvent6 */));
        });
      }
    }else{
      return new T(function(){
        return new T3(0,E(_80/* sr8l */),E(_7z/* GHC.Base.Nothing */),E(_60/* Haste.Events.MouseEvents.$fEventMouseEvent6 */));
      });
    }
  }
},
_85/* $fEventMouseEvent1 */ = function(_86/* sr9w */, _87/* sr9x */, _/* EXTERNAL */){
  return new F(function(){return _7R/* Haste.Events.MouseEvents.$wa */(_86/* sr9w */, E(_87/* sr9x */), _/* EXTERNAL */);});
},
_88/* $fEventMouseEvent10 */ = "mouseout",
_89/* $fEventMouseEvent11 */ = "mouseover",
_8a/* $fEventMouseEvent12 */ = "mousemove",
_8b/* $fEventMouseEvent13 */ = "mouseup",
_8c/* $fEventMouseEvent14 */ = "mousedown",
_8d/* $fEventMouseEvent15 */ = "dblclick",
_8e/* $fEventMouseEvent16 */ = "click",
_8f/* $fEventMouseEvent9 */ = "wheel",
_8g/* $fEventMouseEvent_$ceventName */ = function(_8h/* sr5I */){
  switch(E(_8h/* sr5I */)){
    case 0:
      return E(_8e/* Haste.Events.MouseEvents.$fEventMouseEvent16 */);
    case 1:
      return E(_8d/* Haste.Events.MouseEvents.$fEventMouseEvent15 */);
    case 2:
      return E(_8c/* Haste.Events.MouseEvents.$fEventMouseEvent14 */);
    case 3:
      return E(_8b/* Haste.Events.MouseEvents.$fEventMouseEvent13 */);
    case 4:
      return E(_8a/* Haste.Events.MouseEvents.$fEventMouseEvent12 */);
    case 5:
      return E(_89/* Haste.Events.MouseEvents.$fEventMouseEvent11 */);
    case 6:
      return E(_88/* Haste.Events.MouseEvents.$fEventMouseEvent10 */);
    default:
      return E(_8f/* Haste.Events.MouseEvents.$fEventMouseEvent9 */);
  }
},
_8i/* $fEventMouseEvent */ = new T2(0,_8g/* Haste.Events.MouseEvents.$fEventMouseEvent_$ceventName */,_85/* Haste.Events.MouseEvents.$fEventMouseEvent1 */),
_8j/* $fEventSourceElem1 */ = function(_8k/* smB0 */){
  return E(_8k/* smB0 */);
},
_8l/* $fApplicativeIO1 */ = function(_8m/* s3he */, _8n/* s3hf */, _/* EXTERNAL */){
  var _8o/* s3hh */ = B(A1(_8m/* s3he */,_/* EXTERNAL */)),
  _8p/* s3hk */ = B(A1(_8n/* s3hf */,_/* EXTERNAL */));
  return _8o/* s3hh */;
},
_8q/* $fApplicativeIO2 */ = function(_8r/* s3gs */, _8s/* s3gt */, _/* EXTERNAL */){
  var _8t/* s3gv */ = B(A1(_8r/* s3gs */,_/* EXTERNAL */)),
  _8u/* s3gy */ = B(A1(_8s/* s3gt */,_/* EXTERNAL */));
  return new T(function(){
    return B(A1(_8t/* s3gv */,_8u/* s3gy */));
  });
},
_8v/* $fFunctorIO1 */ = function(_8w/* s3gX */, _8x/* s3gY */, _/* EXTERNAL */){
  var _8y/* s3h0 */ = B(A1(_8x/* s3gY */,_/* EXTERNAL */));
  return _8w/* s3gX */;
},
_8z/* $fFunctorIO2 */ = function(_8A/* s3gQ */, _8B/* s3gR */, _/* EXTERNAL */){
  var _8C/* s3gT */ = B(A1(_8B/* s3gR */,_/* EXTERNAL */));
  return new T(function(){
    return B(A1(_8A/* s3gQ */,_8C/* s3gT */));
  });
},
_8D/* $fFunctorIO */ = new T2(0,_8z/* GHC.Base.$fFunctorIO2 */,_8v/* GHC.Base.$fFunctorIO1 */),
_8E/* returnIO1 */ = function(_8F/* s3g5 */, _/* EXTERNAL */){
  return _8F/* s3g5 */;
},
_8G/* thenIO1 */ = function(_8H/* s3fZ */, _8I/* s3g0 */, _/* EXTERNAL */){
  var _8J/* s3g2 */ = B(A1(_8H/* s3fZ */,_/* EXTERNAL */));
  return new F(function(){return A1(_8I/* s3g0 */,_/* EXTERNAL */);});
},
_8K/* $fApplicativeIO */ = new T5(0,_8D/* GHC.Base.$fFunctorIO */,_8E/* GHC.Base.returnIO1 */,_8q/* GHC.Base.$fApplicativeIO2 */,_8G/* GHC.Base.thenIO1 */,_8l/* GHC.Base.$fApplicativeIO1 */),
_8L/* $fxExceptionIOException */ = new T(function(){
  return E(_7w/* GHC.IO.Exception.$fExceptionIOException */);
}),
_8M/* userError */ = function(_8N/* s3N8k */){
  return new T6(0,_7z/* GHC.Base.Nothing */,_7A/* GHC.IO.Exception.UserError */,_4/* GHC.Types.[] */,_8N/* s3N8k */,_7z/* GHC.Base.Nothing */,_7z/* GHC.Base.Nothing */);
},
_8O/* failIO1 */ = function(_8P/* s2YSE */, _/* EXTERNAL */){
  var _8Q/* s2YSH */ = new T(function(){
    return B(A2(_U/* GHC.Exception.toException */,_8L/* GHC.IO.Exception.$fxExceptionIOException */, new T(function(){
      return B(A1(_8M/* GHC.IO.Exception.userError */,_8P/* s2YSE */));
    })));
  });
  return new F(function(){return die/* EXTERNAL */(_8Q/* s2YSH */);});
},
_8R/* failIO */ = function(_8S/* B2 */, _/* EXTERNAL */){
  return new F(function(){return _8O/* GHC.IO.failIO1 */(_8S/* B2 */, _/* EXTERNAL */);});
},
_8T/* $fMonadIO_$cfail */ = function(_8U/* s3ei */){
  return new F(function(){return A1(_8R/* GHC.IO.failIO */,_8U/* s3ei */);});
},
_8V/* bindIO1 */ = function(_8W/* s3g7 */, _8X/* s3g8 */, _/* EXTERNAL */){
  var _8Y/* s3ga */ = B(A1(_8W/* s3g7 */,_/* EXTERNAL */));
  return new F(function(){return A2(_8X/* s3g8 */,_8Y/* s3ga */, _/* EXTERNAL */);});
},
_8Z/* $fMonadIO */ = new T5(0,_8K/* GHC.Base.$fApplicativeIO */,_8V/* GHC.Base.bindIO1 */,_8G/* GHC.Base.thenIO1 */,_8E/* GHC.Base.returnIO1 */,_8T/* GHC.Base.$fMonadIO_$cfail */),
_90/* id */ = function(_91/* s3aG */){
  return E(_91/* s3aG */);
},
_92/* $fMonadIOIO */ = new T2(0,_8Z/* GHC.Base.$fMonadIO */,_90/* GHC.Base.id */),
_93/* $fMonadEventIO */ = new T2(0,_92/* Control.Monad.IO.Class.$fMonadIOIO */,_8E/* GHC.Base.returnIO1 */),
_94/* $fFromAnyGameState15 */ = "turn",
_95/* $fFromAnyGameState16 */ = "stage",
_96/* $fFromAnyGameState5 */ = "finished",
_97/* $fFromAnyGameState7 */ = "pieces",
_98/* $fFromAnyGameState8 */ = "numRolls",
_99/* $w$ctoAny1 */ = function(_9a/* shnK */){
  switch(E(_9a/* shnK */)){
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
_9b/* $fToAnyGameState_$ctoAny1 */ = function(_9c/* sho6 */){
  return new F(function(){return _99/* LudoJS.$w$ctoAny1 */(_9c/* sho6 */);});
},
_9d/* $fToAnyGameState4 */ = "GameFinished",
_9e/* $fToAnyGameState5 */ = "stage",
_9f/* $fToAnyGameState3 */ = new T2(0,_9e/* LudoJS.$fToAnyGameState5 */,_9d/* LudoJS.$fToAnyGameState4 */),
_9g/* $fToAnyGameState2 */ = new T2(1,_9f/* LudoJS.$fToAnyGameState3 */,_4/* GHC.Types.[] */),
_9h/* $wtoObject */ = function(_9i/* sbWw */){
  var _9j/* sbWy */ = __new/* EXTERNAL */(),
  _9k/* sbWA */ = _9j/* sbWy */,
  _9l/* sbWB */ = function(_9m/* sbWC */, _/* EXTERNAL */){
    while(1){
      var _9n/* sbWE */ = E(_9m/* sbWC */);
      if(!_9n/* sbWE */._){
        return _2s/* GHC.Tuple.() */;
      }else{
        var _9o/* sbWH */ = E(_9n/* sbWE */.a),
        _9p/* sbWP */ = __set/* EXTERNAL */(_9k/* sbWA */, E(_9o/* sbWH */.a), E(_9o/* sbWH */.b));
        _9m/* sbWC */ = _9n/* sbWE */.b;
        continue;
      }
    }
  },
  _9q/* sbWR */ = B(_9l/* sbWB */(_9i/* sbWw */, _/* EXTERNAL */));
  return E(_9k/* sbWA */);
},
_9r/* $fToAnyGameState1 */ = new T(function(){
  return B(_9h/* Haste.Prim.Any.$wtoObject */(_9g/* LudoJS.$fToAnyGameState2 */));
}),
_9s/* $fToAnyGameState11 */ = "rollNumber",
_9t/* $fToAnyGameState13 */ = "Roll",
_9u/* $fToAnyGameState12 */ = new T2(0,_95/* LudoJS.$fFromAnyGameState16 */,_9t/* LudoJS.$fToAnyGameState13 */),
_9v/* $fToAnyGameState18 */ =  -1,
_9w/* $fToAnyGameState19 */ = "rollNumber",
_9x/* $fToAnyGameState17 */ = new T2(0,_9w/* LudoJS.$fToAnyGameState19 */,_9v/* LudoJS.$fToAnyGameState18 */),
_9y/* $fToAnyGameState16 */ = new T2(1,_9x/* LudoJS.$fToAnyGameState17 */,_4/* GHC.Types.[] */),
_9z/* $fToAnyGameState21 */ = "stage",
_9A/* $fToAnyGameState20 */ = new T2(0,_9z/* LudoJS.$fToAnyGameState21 */,_9t/* LudoJS.$fToAnyGameState13 */),
_9B/* $fToAnyGameState15 */ = new T2(1,_9A/* LudoJS.$fToAnyGameState20 */,_9y/* LudoJS.$fToAnyGameState16 */),
_9C/* $fToAnyGameState14 */ = new T(function(){
  return B(_9h/* Haste.Prim.Any.$wtoObject */(_9B/* LudoJS.$fToAnyGameState15 */));
}),
_9D/* $fToAnyGameState6 */ = "pieceIndex",
_9E/* $fToAnyGameState8 */ = "SelectField",
_9F/* $fToAnyGameState7 */ = new T2(0,_95/* LudoJS.$fFromAnyGameState16 */,_9E/* LudoJS.$fToAnyGameState8 */),
_9G/* $fToAnyGameState10 */ = "SelectPiece",
_9H/* $fToAnyGameState9 */ = new T2(0,_95/* LudoJS.$fFromAnyGameState16 */,_9G/* LudoJS.$fToAnyGameState10 */),
_9I/* $fToAnyGameState_$ctoAny2 */ = function(_9J/* sgY5 */){
  var _9K/* sgY6 */ = E(_9J/* sgY5 */);
  switch(_9K/* sgY6 */._){
    case 0:
      var _9L/* sgY8 */ = E(_9K/* sgY6 */.a);
      if(!_9L/* sgY8 */._){
        return E(_9C/* LudoJS.$fToAnyGameState14 */);
      }else{
        return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,_9u/* LudoJS.$fToAnyGameState12 */,new T2(1,new T2(0,_9s/* LudoJS.$fToAnyGameState11 */,_9L/* sgY8 */.a),_4/* GHC.Types.[] */)));});
      }
      break;
    case 1:
      return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,_9H/* LudoJS.$fToAnyGameState9 */,new T2(1,new T2(0,_9s/* LudoJS.$fToAnyGameState11 */,_9K/* sgY6 */.a),_4/* GHC.Types.[] */)));});
      break;
    case 2:
      return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,_9F/* LudoJS.$fToAnyGameState7 */,new T2(1,new T2(0,_9s/* LudoJS.$fToAnyGameState11 */,_9K/* sgY6 */.a),new T2(1,new T2(0,_9D/* LudoJS.$fToAnyGameState6 */,_9K/* sgY6 */.b),_4/* GHC.Types.[] */))));});
      break;
    default:
      return E(_9r/* LudoJS.$fToAnyGameState1 */);
  }
},
_9M/* $fToAnyOption1 */ = "field",
_9N/* $fToAnyOption5 */ = "piece",
_9O/* $fToAnyPiece2 */ = "Active",
_9P/* $fToAnyPiece1 */ = new T2(0,_9N/* LudoJS.$fToAnyOption5 */,_9O/* LudoJS.$fToAnyPiece2 */),
_9Q/* $fToAnyPiece6 */ = "Out",
_9R/* $fToAnyPiece7 */ = "piece",
_9S/* $fToAnyPiece5 */ = new T2(0,_9R/* LudoJS.$fToAnyPiece7 */,_9Q/* LudoJS.$fToAnyPiece6 */),
_9T/* $fToAnyPiece4 */ = new T2(1,_9S/* LudoJS.$fToAnyPiece5 */,_4/* GHC.Types.[] */),
_9U/* $fToAnyPiece3 */ = new T(function(){
  return B(_9h/* Haste.Prim.Any.$wtoObject */(_9T/* LudoJS.$fToAnyPiece4 */));
}),
_9V/* $fToAnyPiece_$ctoAny */ = function(_9W/* sgXR */){
  var _9X/* sgXS */ = E(_9W/* sgXR */);
  if(!_9X/* sgXS */._){
    return E(_9U/* LudoJS.$fToAnyPiece3 */);
  }else{
    return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,_9P/* LudoJS.$fToAnyPiece1 */,new T2(1,new T2(0,_9M/* LudoJS.$fToAnyOption1 */,_9X/* sgXS */.a),_4/* GHC.Types.[] */)));});
  }
},
_9Y/* go1 */ = function(_9Z/* shof */){
  var _a0/* shog */ = E(_9Z/* shof */);
  if(!_a0/* shog */._){
    return __Z/* EXTERNAL */;
  }else{
    var _a1/* shoj */ = E(_a0/* shog */.a);
    return new T2(1,new T2(0,new T(function(){
      return toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, E(_a1/* shoj */.a), _4/* GHC.Types.[] */)));
    }),new T(function(){
      return B(_9V/* LudoJS.$fToAnyPiece_$ctoAny */(_a1/* shoj */.b));
    })),new T(function(){
      return B(_9Y/* LudoJS.go1 */(_a0/* shog */.b));
    }));
  }
},
_a2/* lvl38 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_1r/* LudoJS.$fFromAnyGameState11 */, _4/* GHC.Types.[] */));
}),
_a3/* lvl39 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_1s/* LudoJS.$fFromAnyGameState12 */, _4/* GHC.Types.[] */));
}),
_a4/* lvl40 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_1t/* LudoJS.$fFromAnyGameState13 */, _4/* GHC.Types.[] */));
}),
_a5/* lvl41 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_1u/* LudoJS.$fFromAnyGameState14 */, _4/* GHC.Types.[] */));
}),
_a6/* $fToAnyGameState_go10 */ = function(_a7/*  shov */, _a8/*  show */){
  while(1){
    var _a9/*  $fToAnyGameState_go10 */ = B((function(_aa/* shov */, _ab/* show */){
      var _ac/* shox */ = E(_ab/* show */);
      if(!_ac/* shox */._){
        var _ad/* shoT */ = new T(function(){
          return B(_9h/* Haste.Prim.Any.$wtoObject */(new T(function(){
            return B(_9Y/* LudoJS.go1 */(_ac/* shox */.c));
          },1)));
        });
        _a7/*  shov */ = new T2(1,new T2(0,new T(function(){
          switch(E(_ac/* shox */.b)){
            case 0:
              return toJSStr/* EXTERNAL */(E(_a5/* LudoJS.lvl41 */));
              break;
            case 1:
              return toJSStr/* EXTERNAL */(E(_a4/* LudoJS.lvl40 */));
              break;
            case 2:
              return toJSStr/* EXTERNAL */(E(_a3/* LudoJS.lvl39 */));
              break;
            default:
              return toJSStr/* EXTERNAL */(E(_a2/* LudoJS.lvl38 */));
          }
        }),_ad/* shoT */),new T(function(){
          return B(_a6/* LudoJS.$fToAnyGameState_go10 */(_aa/* shov */, _ac/* shox */.e));
        }));
        _a8/*  show */ = _ac/* shox */.d;
        return __continue/* EXTERNAL */;
      }else{
        return E(_aa/* shov */);
      }
    })(_a7/*  shov */, _a8/*  show */));
    if(_a9/*  $fToAnyGameState_go10 */!=__continue/* EXTERNAL */){
      return _a9/*  $fToAnyGameState_go10 */;
    }
  }
},
_ae/* map */ = function(_af/* s3hr */, _ag/* s3hs */){
  var _ah/* s3ht */ = E(_ag/* s3hs */);
  return (_ah/* s3ht */._==0) ? __Z/* EXTERNAL */ : new T2(1,new T(function(){
    return B(A1(_af/* s3hr */,_ah/* s3ht */.a));
  }),new T(function(){
    return B(_ae/* GHC.Base.map */(_af/* s3hr */, _ah/* s3ht */.b));
  }));
},
_ai/* $w$ctoAny */ = function(_aj/* shp7 */){
  var _ak/* shpR */ = new T(function(){
    return B(_9h/* Haste.Prim.Any.$wtoObject */(new T(function(){
      return B(_a6/* LudoJS.$fToAnyGameState_go10 */(_4/* GHC.Types.[] */, E(_aj/* shp7 */).d));
    },1)));
  });
  return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,new T2(0,_95/* LudoJS.$fFromAnyGameState16 */,new T(function(){
    return B(_9I/* LudoJS.$fToAnyGameState_$ctoAny2 */(E(_aj/* shp7 */).a));
  })),new T2(1,new T2(0,_94/* LudoJS.$fFromAnyGameState15 */,new T(function(){
    switch(E(E(_aj/* shp7 */).b)){
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
  })),new T2(1,new T2(0,_98/* LudoJS.$fFromAnyGameState8 */,new T(function(){
    return E(E(_aj/* shp7 */).c);
  })),new T2(1,new T2(0,_97/* LudoJS.$fFromAnyGameState7 */,_ak/* shpR */),new T2(1,new T2(0,_96/* LudoJS.$fFromAnyGameState5 */,new T(function(){
    return __lst2arr/* EXTERNAL */(B(_ae/* GHC.Base.map */(_9b/* LudoJS.$fToAnyGameState_$ctoAny1 */, E(_aj/* shp7 */).e)));
  })),_4/* GHC.Types.[] */))))));});
},
_al/* Click */ = 0,
_am/* $p1MonadEvent */ = function(_an/* smAR */){
  return E(E(_an/* smAR */).a);
},
_ao/* $p1MonadIO */ = function(_ap/* suB */){
  return E(E(_ap/* suB */).a);
},
_aq/* >>= */ = function(_ar/* s34T */){
  return E(E(_ar/* s34T */).b);
},
_as/* eventData */ = function(_at/* smAN */){
  return E(E(_at/* smAN */).b);
},
_au/* eventName */ = function(_av/* smAJ */){
  return E(E(_av/* smAJ */).a);
},
_aw/* lvl3 */ = function(_/* EXTERNAL */){
  return new F(function(){return nMV/* EXTERNAL */(_7z/* GHC.Base.Nothing */);});
},
_ax/* evtRef */ = new T(function(){
  return B(_1S/* GHC.IO.unsafeDupablePerformIO */(_aw/* Haste.Events.Core.lvl3 */));
}),
_ay/* liftIO */ = function(_az/* suF */){
  return E(E(_az/* suF */).b);
},
_aA/* mkHandler */ = function(_aB/* smAV */){
  return E(E(_aB/* smAV */).b);
},
_aC/* onEvent_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function(e,name,f){e.addEventListener(name,f,false);return [f];})");
}),
_aD/* return */ = function(_aE/* s357 */){
  return E(E(_aE/* s357 */).d);
},
_aF/* onEvent */ = function(_aG/* smBs */, _aH/* smBt */, _aI/* smBu */, _aJ/* smBv */, _aK/* smBw */, _aL/* smBx */){
  var _aM/* smBy */ = B(_am/* Haste.Events.Core.$p1MonadEvent */(_aG/* smBs */)),
  _aN/* smBz */ = B(_ao/* Control.Monad.IO.Class.$p1MonadIO */(_aM/* smBy */)),
  _aO/* smBA */ = new T(function(){
    return B(_ay/* Control.Monad.IO.Class.liftIO */(_aM/* smBy */));
  }),
  _aP/* smBB */ = new T(function(){
    return B(_aD/* GHC.Base.return */(_aN/* smBz */));
  }),
  _aQ/* smBC */ = new T(function(){
    return B(A1(_aH/* smBt */,_aJ/* smBv */));
  }),
  _aR/* smBD */ = new T(function(){
    return B(A2(_au/* Haste.Events.Core.eventName */,_aI/* smBu */, _aK/* smBw */));
  }),
  _aS/* smBE */ = function(_aT/* smBF */){
    return new F(function(){return A1(_aP/* smBB */,new T3(0,_aR/* smBD */,_aQ/* smBC */,_aT/* smBF */));});
  },
  _aU/* smCl */ = function(_aV/* smBS */){
    var _aW/* smCk */ = new T(function(){
      var _aX/* smCj */ = new T(function(){
        var _aY/* smC7 */ = __createJSFunc/* EXTERNAL */(2, function(_aZ/* smBY */, _/* EXTERNAL */){
          var _b0/* smC0 */ = B(A2(E(_aV/* smBS */),_aZ/* smBY */, _/* EXTERNAL */));
          return _1W/* Haste.Prim.Any.jsNull */;
        }),
        _b1/* smC9 */ = _aY/* smC7 */;
        return function(_/* EXTERNAL */){
          return new F(function(){return __app3/* EXTERNAL */(E(_aC/* Haste.Events.Core.onEvent_f1 */), E(_aQ/* smBC */), E(_aR/* smBD */), _b1/* smC9 */);});
        };
      });
      return B(A1(_aO/* smBA */,_aX/* smCj */));
    });
    return new F(function(){return A3(_aq/* GHC.Base.>>= */,_aN/* smBz */, _aW/* smCk */, _aS/* smBE */);});
  },
  _b2/* smBR */ = new T(function(){
    var _b3/* smBH */ = new T(function(){
      return B(_ay/* Control.Monad.IO.Class.liftIO */(_aM/* smBy */));
    }),
    _b4/* smBQ */ = function(_b5/* smBI */){
      var _b6/* smBP */ = new T(function(){
        return B(A1(_b3/* smBH */,function(_/* EXTERNAL */){
          var _/* EXTERNAL */ = wMV/* EXTERNAL */(E(_ax/* Haste.Events.Core.evtRef */), new T1(1,_b5/* smBI */));
          return new F(function(){return A(_as/* Haste.Events.Core.eventData */,[_aI/* smBu */, _aK/* smBw */, _b5/* smBI */, _/* EXTERNAL */]);});
        }));
      });
      return new F(function(){return A3(_aq/* GHC.Base.>>= */,_aN/* smBz */, _b6/* smBP */, _aL/* smBx */);});
    };
    return B(A2(_aA/* Haste.Events.Core.mkHandler */,_aG/* smBs */, _b4/* smBQ */));
  });
  return new F(function(){return A3(_aq/* GHC.Base.>>= */,_aN/* smBz */, _b2/* smBR */, _aU/* smCl */);});
},
_b7/* play10 */ = 3,
_b8/* play11 */ = new T1(0,_7z/* GHC.Base.Nothing */),
_b9/* eqInt */ = function(_ba/* scEd */, _bb/* scEe */){
  return E(_ba/* scEd */)==E(_bb/* scEe */);
},
_bc/* $fEqOption_$c== */ = function(_bd/* sh1j */, _be/* sh1k */){
  var _bf/* sh1l */ = E(_bd/* sh1j */);
  if(!_bf/* sh1l */._){
    var _bg/* sh1n */ = E(_be/* sh1k */);
    if(!_bg/* sh1n */._){
      return new F(function(){return _b9/* GHC.Classes.eqInt */(_bf/* sh1l */.a, _bg/* sh1n */.a);});
    }else{
      return false;
    }
  }else{
    var _bh/* sh1t */ = E(_be/* sh1k */);
    if(!_bh/* sh1t */._){
      return false;
    }else{
      if(E(_bf/* sh1l */.a)!=E(_bh/* sh1t */.a)){
        return false;
      }else{
        return new F(function(){return _b9/* GHC.Classes.eqInt */(_bf/* sh1l */.b, _bh/* sh1t */.b);});
      }
    }
  }
},
_bi/* $fEqOption_$c/= */ = function(_bj/* sh1D */, _bk/* sh1E */){
  return (!B(_bc/* LudoJS.$fEqOption_$c== */(_bj/* sh1D */, _bk/* sh1E */))) ? true : false;
},
_bl/* $fEqOption */ = new T2(0,_bc/* LudoJS.$fEqOption_$c== */,_bi/* LudoJS.$fEqOption_$c/= */),
_bm/* $fShowStage2 */ = 0,
_bn/* Blue */ = 0,
_bo/* Green */ = 1,
_bp/* Red */ = 2,
_bq/* Yellow */ = 3,
_br/* lvl26 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Map.!: given key is not an element in the map"));
}),
_bs/* lvl27 */ = new T(function(){
  return B(err/* EXTERNAL */(_br/* LudoJS.lvl26 */));
}),
_bt/* $s!1 */ = function(_bu/* sgxe */, _bv/* sgxf */){
  while(1){
    var _bw/* sgxg */ = E(_bv/* sgxf */);
    if(!_bw/* sgxg */._){
      var _bx/* sgxi */ = _bw/* sgxg */.b,
      _by/* sgxj */ = _bw/* sgxg */.c,
      _bz/* sgxk */ = _bw/* sgxg */.d,
      _bA/* sgxl */ = _bw/* sgxg */.e;
      switch(E(_bu/* sgxe */)){
        case 0:
          switch(E(_bx/* sgxi */)){
            case 0:
              return E(_by/* sgxj */);
            case 1:
              _bu/* sgxe */ = _bn/* LudoJS.Blue */;
              _bv/* sgxf */ = _bz/* sgxk */;
              continue;
            case 2:
              _bu/* sgxe */ = _bn/* LudoJS.Blue */;
              _bv/* sgxf */ = _bz/* sgxk */;
              continue;
            default:
              _bu/* sgxe */ = _bn/* LudoJS.Blue */;
              _bv/* sgxf */ = _bz/* sgxk */;
              continue;
          }
          break;
        case 1:
          switch(E(_bx/* sgxi */)){
            case 0:
              _bu/* sgxe */ = _bo/* LudoJS.Green */;
              _bv/* sgxf */ = _bA/* sgxl */;
              continue;
            case 1:
              return E(_by/* sgxj */);
            case 2:
              _bu/* sgxe */ = _bo/* LudoJS.Green */;
              _bv/* sgxf */ = _bz/* sgxk */;
              continue;
            default:
              _bu/* sgxe */ = _bo/* LudoJS.Green */;
              _bv/* sgxf */ = _bz/* sgxk */;
              continue;
          }
          break;
        case 2:
          switch(E(_bx/* sgxi */)){
            case 2:
              return E(_by/* sgxj */);
            case 3:
              _bu/* sgxe */ = _bp/* LudoJS.Red */;
              _bv/* sgxf */ = _bz/* sgxk */;
              continue;
            default:
              _bu/* sgxe */ = _bp/* LudoJS.Red */;
              _bv/* sgxf */ = _bA/* sgxl */;
              continue;
          }
          break;
        default:
          if(E(_bx/* sgxi */)==3){
            return E(_by/* sgxj */);
          }else{
            _bu/* sgxe */ = _bq/* LudoJS.Yellow */;
            _bv/* sgxf */ = _bA/* sgxl */;
            continue;
          }
      }
    }else{
      return E(_bs/* LudoJS.lvl27 */);
    }
  }
},
_bB/* $wlenAcc */ = function(_bC/* sbMs */, _bD/* sbMt */){
  while(1){
    var _bE/* sbMu */ = E(_bC/* sbMs */);
    if(!_bE/* sbMu */._){
      return E(_bD/* sbMt */);
    }else{
      var _bF/*  sbMt */ = _bD/* sbMt */+1|0;
      _bC/* sbMs */ = _bE/* sbMu */.b;
      _bD/* sbMt */ = _bF/*  sbMt */;
      continue;
    }
  }
},
_bG/* $sa */ = function(_bH/*  shuW */, _bI/*  shuX */, _bJ/*  shuY */, _bK/*  shuZ */, _bL/*  shv0 */, _bM/*  shv1 */, _bN/*  shv2 */, _/* EXTERNAL */){
  while(1){
    var _bO/*  $sa */ = B((function(_bP/* shuW */, _bQ/* shuX */, _bR/* shuY */, _bS/* shuZ */, _bT/* shv0 */, _bU/* shv1 */, _bV/* shv2 */, _/* EXTERNAL */){
      var _bW/* shv4 */ = E(_bP/* shuW */);
      if(!_bW/* shv4 */._){
        return new T2(0,_bQ/* shuX */,new T5(0,_bR/* shuY */,_bS/* shuZ */,_bT/* shv0 */,_bU/* shv1 */,_bV/* shv2 */));
      }else{
        var _bX/* shv7 */ = _bW/* shv4 */.a,
        _bY/*   shuY */ = _bR/* shuY */,
        _bZ/*   shuZ */ = _bS/* shuZ */,
        _c0/*   shv0 */ = _bT/* shv0 */,
        _c1/*   shv1 */ = _bU/* shv1 */,
        _c2/*   shv2 */ = _bV/* shv2 */;
        _bH/*  shuW */ = _bW/* shv4 */.b;
        _bI/*  shuX */ = new T(function(){
          if(!B(_bB/* GHC.List.$wlenAcc */(B(_bt/* LudoJS.$s!1 */(_bX/* shv7 */, _bU/* shv1 */)), 0))){
            return E(_bQ/* shuX */);
          }else{
            return B(_q/* GHC.Base.++ */(_bQ/* shuX */, new T2(1,_bX/* shv7 */,_4/* GHC.Types.[] */)));
          }
        });
        _bJ/*  shuY */ = _bY/*   shuY */;
        _bK/*  shuZ */ = _bZ/*   shuZ */;
        _bL/*  shv0 */ = _c0/*   shv0 */;
        _bM/*  shv1 */ = _c1/*   shv1 */;
        _bN/*  shv2 */ = _c2/*   shv2 */;
        return __continue/* EXTERNAL */;
      }
    })(_bH/*  shuW */, _bI/*  shuX */, _bJ/*  shuY */, _bK/*  shuZ */, _bL/*  shv0 */, _bM/*  shv1 */, _bN/*  shv2 */, _/* EXTERNAL */));
    if(_bO/*  $sa */!=__continue/* EXTERNAL */){
      return _bO/*  $sa */;
    }
  }
},
_c3/* $sa1 */ = function(_c4/* shvd */, _c5/* shve */, _c6/* shvf */, _c7/* shvg */, _c8/* shvh */, _c9/* shvi */, _ca/* shvj */, _cb/* shvk */, _/* EXTERNAL */){
  return new F(function(){return _bG/* LudoJS.$sa */(_c5/* shve */, new T(function(){
    if(!B(_bB/* GHC.List.$wlenAcc */(B(_bt/* LudoJS.$s!1 */(_c4/* shvd */, _ca/* shvj */)), 0))){
      return E(_c6/* shvf */);
    }else{
      return B(_q/* GHC.Base.++ */(_c6/* shvf */, new T2(1,_c4/* shvd */,_4/* GHC.Types.[] */)));
    }
  }), _c7/* shvg */, _c8/* shvh */, _c9/* shvi */, _ca/* shvj */, _cb/* shvk */, _/* EXTERNAL */);});
},
_cc/* $fFromAny()4 */ = function(_/* EXTERNAL */){
  return _2s/* GHC.Tuple.() */;
},
_cd/* $fToAnyOption3 */ = "Move",
_ce/* $fToAnyOption4 */ = "option",
_cf/* $fToAnyOption2 */ = new T2(0,_ce/* LudoJS.$fToAnyOption4 */,_cd/* LudoJS.$fToAnyOption3 */),
_cg/* $fToAnyOption7 */ = "Play",
_ch/* $fToAnyOption6 */ = new T2(0,_ce/* LudoJS.$fToAnyOption4 */,_cg/* LudoJS.$fToAnyOption7 */),
_ci/* $w$ctoAny2 */ = function(_cj/* sgYy */){
  var _ck/* sgYz */ = E(_cj/* sgYy */);
  if(!_ck/* sgYz */._){
    return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,_ch/* LudoJS.$fToAnyOption6 */,new T2(1,new T2(0,_9N/* LudoJS.$fToAnyOption5 */,_ck/* sgYz */.a),_4/* GHC.Types.[] */)));});
  }else{
    return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,_cf/* LudoJS.$fToAnyOption2 */,new T2(1,new T2(0,_9N/* LudoJS.$fToAnyOption5 */,_ck/* sgYz */.a),new T2(1,new T2(0,_9M/* LudoJS.$fToAnyOption1 */,_ck/* sgYz */.b),_4/* GHC.Types.[] */))));});
  }
},
_cl/* $fToAnyOption_$ctoAny */ = function(_cm/* sgYL */){
  return new F(function(){return _ci/* LudoJS.$w$ctoAny2 */(_cm/* sgYL */);});
},
_cn/* a42 */ = function(_co/* sgDS */, _cp/* sgDT */, _/* EXTERNAL */){
  var _cq/* sgG5 */ = new T(function(){
    var _cr/* sgDV */ = E(_cp/* sgDT */),
    _cs/* sgE1 */ = function(_ct/* sgE2 */){
      var _cu/* sgE3 */ = E(_ct/* sgE2 */);
      if(!_cu/* sgE3 */._){
        return __Z/* EXTERNAL */;
      }else{
        var _cv/* sgE5 */ = _cu/* sgE3 */.b,
        _cw/* sgE6 */ = E(_cu/* sgE3 */.a),
        _cx/* sgE7 */ = _cw/* sgE6 */.a,
        _cy/* sgE9 */ = E(_cw/* sgE6 */.b);
        if(!_cy/* sgE9 */._){
          var _cz/* sgEc */ = E(_co/* sgDS */);
          if(_cz/* sgEc */==6){
            var _cA/* sgEY */ = new T(function(){
              var _cB/* sgEA */ = function(_cC/*  sgEB */){
                while(1){
                  var _cD/*  sgEA */ = B((function(_cE/* sgEB */){
                    var _cF/* sgEC */ = E(_cE/* sgEB */);
                    if(!_cF/* sgEC */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _cG/* sgEE */ = _cF/* sgEC */.b,
                      _cH/* sgEF */ = E(_cF/* sgEC */.a),
                      _cI/* sgEG */ = _cH/* sgEF */.a,
                      _cJ/* sgEI */ = E(_cH/* sgEF */.b);
                      if(!_cJ/* sgEI */._){
                        return new T2(1,new T1(0,_cI/* sgEG */),new T(function(){
                          return B(_cB/* sgEA */(_cG/* sgEE */));
                        }));
                      }else{
                        var _cK/* sgEM */ = E(_cJ/* sgEI */.a);
                        if(_cK/* sgEM */>=51){
                          if((6+_cK/* sgEM */|0)==56){
                            return new T2(1,new T2(1,_cI/* sgEG */,56),new T(function(){
                              return B(_cB/* sgEA */(_cG/* sgEE */));
                            }));
                          }else{
                            _cC/*  sgEB */ = _cG/* sgEE */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          return new T2(1,new T2(1,_cI/* sgEG */,6+_cK/* sgEM */|0),new T(function(){
                            return B(_cB/* sgEA */(_cG/* sgEE */));
                          }));
                        }
                      }
                    }
                  })(_cC/*  sgEB */));
                  if(_cD/*  sgEA */!=__continue/* EXTERNAL */){
                    return _cD/*  sgEA */;
                  }
                }
              };
              return B(_cB/* sgEA */(_cv/* sgE5 */));
            });
            return new T2(1,new T1(0,_cx/* sgE7 */),_cA/* sgEY */);
          }else{
            var _cL/* sgEd */ = function(_cM/*  sgEe */){
              while(1){
                var _cN/*  sgEd */ = B((function(_cO/* sgEe */){
                  var _cP/* sgEf */ = E(_cO/* sgEe */);
                  if(!_cP/* sgEf */._){
                    return __Z/* EXTERNAL */;
                  }else{
                    var _cQ/* sgEh */ = _cP/* sgEf */.b,
                    _cR/* sgEi */ = E(_cP/* sgEf */.a),
                    _cS/* sgEj */ = _cR/* sgEi */.a,
                    _cT/* sgEl */ = E(_cR/* sgEi */.b);
                    if(!_cT/* sgEl */._){
                      _cM/*  sgEe */ = _cQ/* sgEh */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _cU/* sgEn */ = E(_cT/* sgEl */.a);
                      if(_cU/* sgEn */>=51){
                        if((_cz/* sgEc */+_cU/* sgEn */|0)==56){
                          return new T2(1,new T2(1,_cS/* sgEj */,56),new T(function(){
                            return B(_cL/* sgEd */(_cQ/* sgEh */));
                          }));
                        }else{
                          _cM/*  sgEe */ = _cQ/* sgEh */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        return new T2(1,new T2(1,_cS/* sgEj */,_cz/* sgEc */+_cU/* sgEn */|0),new T(function(){
                          return B(_cL/* sgEd */(_cQ/* sgEh */));
                        }));
                      }
                    }
                  }
                })(_cM/*  sgEe */));
                if(_cN/*  sgEd */!=__continue/* EXTERNAL */){
                  return _cN/*  sgEd */;
                }
              }
            };
            return new F(function(){return _cL/* sgEd */(_cv/* sgE5 */);});
          }
        }else{
          var _cV/* sgF0 */ = E(_cy/* sgE9 */.a);
          if(_cV/* sgF0 */>=51){
            var _cW/* sgF4 */ = E(_co/* sgDS */);
            if((_cW/* sgF4 */+_cV/* sgF0 */|0)==56){
              var _cX/* sgFX */ = new T(function(){
                var _cY/* sgFy */ = function(_cZ/*  sgFz */){
                  while(1){
                    var _d0/*  sgFy */ = B((function(_d1/* sgFz */){
                      var _d2/* sgFA */ = E(_d1/* sgFz */);
                      if(!_d2/* sgFA */._){
                        return __Z/* EXTERNAL */;
                      }else{
                        var _d3/* sgFC */ = _d2/* sgFA */.b,
                        _d4/* sgFD */ = E(_d2/* sgFA */.a),
                        _d5/* sgFE */ = _d4/* sgFD */.a,
                        _d6/* sgFG */ = E(_d4/* sgFD */.b);
                        if(!_d6/* sgFG */._){
                          if(E(_cW/* sgF4 */)==6){
                            return new T2(1,new T1(0,_d5/* sgFE */),new T(function(){
                              return B(_cY/* sgFy */(_d3/* sgFC */));
                            }));
                          }else{
                            _cZ/*  sgFz */ = _d3/* sgFC */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          var _d7/* sgFL */ = E(_d6/* sgFG */.a);
                          if(_d7/* sgFL */>=51){
                            if((_cW/* sgF4 */+_d7/* sgFL */|0)==56){
                              return new T2(1,new T2(1,_d5/* sgFE */,56),new T(function(){
                                return B(_cY/* sgFy */(_d3/* sgFC */));
                              }));
                            }else{
                              _cZ/*  sgFz */ = _d3/* sgFC */;
                              return __continue/* EXTERNAL */;
                            }
                          }else{
                            return new T2(1,new T2(1,_d5/* sgFE */,_cW/* sgF4 */+_d7/* sgFL */|0),new T(function(){
                              return B(_cY/* sgFy */(_d3/* sgFC */));
                            }));
                          }
                        }
                      }
                    })(_cZ/*  sgFz */));
                    if(_d0/*  sgFy */!=__continue/* EXTERNAL */){
                      return _d0/*  sgFy */;
                    }
                  }
                };
                return B(_cY/* sgFy */(_cv/* sgE5 */));
              });
              return new T2(1,new T2(1,_cx/* sgE7 */,56),_cX/* sgFX */);
            }else{
              var _d8/* sgF7 */ = function(_d9/*  sgF8 */){
                while(1){
                  var _da/*  sgF7 */ = B((function(_db/* sgF8 */){
                    var _dc/* sgF9 */ = E(_db/* sgF8 */);
                    if(!_dc/* sgF9 */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _dd/* sgFb */ = _dc/* sgF9 */.b,
                      _de/* sgFc */ = E(_dc/* sgF9 */.a),
                      _df/* sgFd */ = _de/* sgFc */.a,
                      _dg/* sgFf */ = E(_de/* sgFc */.b);
                      if(!_dg/* sgFf */._){
                        if(E(_cW/* sgF4 */)==6){
                          return new T2(1,new T1(0,_df/* sgFd */),new T(function(){
                            return B(_d8/* sgF7 */(_dd/* sgFb */));
                          }));
                        }else{
                          _d9/*  sgF8 */ = _dd/* sgFb */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        var _dh/* sgFk */ = E(_dg/* sgFf */.a);
                        if(_dh/* sgFk */>=51){
                          if((_cW/* sgF4 */+_dh/* sgFk */|0)==56){
                            return new T2(1,new T2(1,_df/* sgFd */,56),new T(function(){
                              return B(_d8/* sgF7 */(_dd/* sgFb */));
                            }));
                          }else{
                            _d9/*  sgF8 */ = _dd/* sgFb */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          return new T2(1,new T2(1,_df/* sgFd */,_cW/* sgF4 */+_dh/* sgFk */|0),new T(function(){
                            return B(_d8/* sgF7 */(_dd/* sgFb */));
                          }));
                        }
                      }
                    }
                  })(_d9/*  sgF8 */));
                  if(_da/*  sgF7 */!=__continue/* EXTERNAL */){
                    return _da/*  sgF7 */;
                  }
                }
              };
              return new F(function(){return _d8/* sgF7 */(_cv/* sgE5 */);});
            }
          }else{
            return new T2(1,new T2(1,_cx/* sgE7 */,new T(function(){
              return E(_co/* sgDS */)+_cV/* sgF0 */|0;
            })),new T(function(){
              return B(_cs/* sgE1 */(_cv/* sgE5 */));
            }));
          }
        }
      }
    };
    return B(_cs/* sgE1 */(B(_bt/* LudoJS.$s!1 */(_cr/* sgDV */.b, _cr/* sgDV */.d))));
  });
  return new T2(0,_cq/* sgG5 */,_cp/* sgDT */);
},
_di/* play3 */ = "((gs, opts, rolls) => drawBoard(gs, opts, rolls))",
_dj/* f1 */ = new T(function(){
  return eval/* EXTERNAL */(E(_di/* LudoJS.play3 */));
}),
_dk/* lvl14 */ = new T(function(){
  return B(_1o/* Control.Exception.Base.patError */("LudoJS.hs:(292,13)-(296,40)|case"));
}),
_dl/* play_f1 */ = new T(function(){
  return eval/* EXTERNAL */("((gs) => gameState=gs)");
}),
_dm/* $w$j */ = function(_/* EXTERNAL */, _dn/* shtp */, _do/* shtq */, _dp/* shtr */, _dq/* shts */, _dr/* shtt */){
  var _ds/* shtu */ = function(_dt/* shtv */, _du/* shtw */){
    var _dv/* shtz */ = new T5(0,_dn/* shtp */,_do/* shtq */,_dp/* shtr */,_dq/* shts */,_dr/* shtt */),
    _dw/* shtA */ = function(_/* EXTERNAL */, _dx/* shtC */, _dy/* shtD */){
      var _dz/* shtI */ = __lst2arr/* EXTERNAL */(B(_ae/* GHC.Base.map */(_cl/* LudoJS.$fToAnyOption_$ctoAny */, _dx/* shtC */))),
      _dA/* shtO */ = __app3/* EXTERNAL */(E(_dj/* LudoJS.f1 */), B(_ai/* LudoJS.$w$ctoAny */(_dv/* shtz */)), _dz/* shtI */, _dt/* shtv */);
      return new T2(0,_2s/* GHC.Tuple.() */,_dy/* shtD */);
    };
    if(E(_dt/* shtv */)==( -1)){
      var _dB/* shui */ = B(_dw/* shtA */(_/* EXTERNAL */, _4/* GHC.Types.[] */, _dv/* shtz */)),
      _dC/* shuy */ = __app1/* EXTERNAL */(E(_dl/* LudoJS.play_f1 */), B(_ai/* LudoJS.$w$ctoAny */(E(E(_dB/* shui */).b))));
      return new F(function(){return _cc/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
    }else{
      var _dD/* shtT */ = B(_cn/* LudoJS.a42 */(_du/* shtw */, _dv/* shtz */, _/* EXTERNAL */)),
      _dE/* shtW */ = E(_dD/* shtT */),
      _dF/* shtZ */ = B(_dw/* shtA */(_/* EXTERNAL */, _dE/* shtW */.a, _dE/* shtW */.b)),
      _dG/* shuf */ = __app1/* EXTERNAL */(E(_dl/* LudoJS.play_f1 */), B(_ai/* LudoJS.$w$ctoAny */(E(E(_dF/* shtZ */).b))));
      return new F(function(){return _cc/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
    }
  },
  _dH/* shuB */ = E(_dn/* shtp */);
  switch(_dH/* shuB */._){
    case 0:
      var _dI/* shuD */ = E(_dH/* shuB */.a);
      if(!_dI/* shuD */._){
        return new F(function(){return _ds/* shtu */( -1, _9v/* LudoJS.$fToAnyGameState18 */);});
      }else{
        var _dJ/* shuF */ = E(_dI/* shuD */.a);
        return new F(function(){return _ds/* shtu */(_dJ/* shuF */, _dJ/* shuF */);});
      }
      break;
    case 1:
      var _dK/* shuI */ = E(_dH/* shuB */.a);
      return new F(function(){return _ds/* shtu */(_dK/* shuI */, _dK/* shuI */);});
      break;
    case 2:
      var _dL/* shuM */ = E(_dH/* shuB */.a);
      return new F(function(){return _ds/* shtu */(_dL/* shuM */, _dL/* shuM */);});
      break;
    default:
      return E(_dk/* LudoJS.lvl14 */);
  }
},
_dM/* $fFromAnyGameState4 */ = function(_dN/* sgYU */, _/* EXTERNAL */){
  var _dO/* sgYW */ = E(_dN/* sgYU */);
  if(!_dO/* sgYW */._){
    return _4/* GHC.Types.[] */;
  }else{
    var _dP/* sgYZ */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_dO/* sgYW */.a, _/* EXTERNAL */)),
    _dQ/* sgZ2 */ = B(_dM/* LudoJS.$fFromAnyGameState4 */(_dO/* sgYW */.b, _/* EXTERNAL */));
    return new T2(1,_dP/* sgYZ */,_dQ/* sgZ2 */);
  }
},
_dR/* Tip */ = new T0(1),
_dS/* lvl12 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Failure in Data.Map.balanceL"));
}),
_dT/* $wpoly_fail2 */ = function(_dU/* s2ee1 */){
  return new F(function(){return err/* EXTERNAL */(_dS/* Data.Map.Base.lvl12 */);});
},
_dV/* lvl13 */ = new T(function(){
  return B(_dT/* Data.Map.Base.$wpoly_fail2 */(_/* EXTERNAL */));
}),
_dW/* balanceL */ = function(_dX/* s2ee2 */, _dY/* s2ee3 */, _dZ/* s2ee4 */, _e0/* s2ee5 */){
  var _e1/* s2ee6 */ = E(_e0/* s2ee5 */);
  if(!_e1/* s2ee6 */._){
    var _e2/* s2ee7 */ = _e1/* s2ee6 */.a,
    _e3/* s2eec */ = E(_dZ/* s2ee4 */);
    if(!_e3/* s2eec */._){
      var _e4/* s2eed */ = _e3/* s2eec */.a,
      _e5/* s2eee */ = _e3/* s2eec */.b,
      _e6/* s2eef */ = _e3/* s2eec */.c;
      if(_e4/* s2eed */<=(imul/* EXTERNAL */(3, _e2/* s2ee7 */)|0)){
        return new T5(0,(1+_e4/* s2eed */|0)+_e2/* s2ee7 */|0,E(_dX/* s2ee2 */),_dY/* s2ee3 */,E(_e3/* s2eec */),E(_e1/* s2ee6 */));
      }else{
        var _e7/* s2eeo */ = E(_e3/* s2eec */.d);
        if(!_e7/* s2eeo */._){
          var _e8/* s2eep */ = _e7/* s2eeo */.a,
          _e9/* s2eeu */ = E(_e3/* s2eec */.e);
          if(!_e9/* s2eeu */._){
            var _ea/* s2eev */ = _e9/* s2eeu */.a,
            _eb/* s2eew */ = _e9/* s2eeu */.b,
            _ec/* s2eex */ = _e9/* s2eeu */.c,
            _ed/* s2eey */ = _e9/* s2eeu */.d;
            if(_ea/* s2eev */>=(imul/* EXTERNAL */(2, _e8/* s2eep */)|0)){
              var _ee/* s2eeD */ = function(_ef/* s2eeE */){
                var _eg/* s2eeF */ = E(_e9/* s2eeu */.e);
                return (_eg/* s2eeF */._==0) ? new T5(0,(1+_e4/* s2eed */|0)+_e2/* s2ee7 */|0,E(_eb/* s2eew */),_ec/* s2eex */,E(new T5(0,(1+_e8/* s2eep */|0)+_ef/* s2eeE */|0,E(_e5/* s2eee */),_e6/* s2eef */,E(_e7/* s2eeo */),E(_ed/* s2eey */))),E(new T5(0,(1+_e2/* s2ee7 */|0)+_eg/* s2eeF */.a|0,E(_dX/* s2ee2 */),_dY/* s2ee3 */,E(_eg/* s2eeF */),E(_e1/* s2ee6 */)))) : new T5(0,(1+_e4/* s2eed */|0)+_e2/* s2ee7 */|0,E(_eb/* s2eew */),_ec/* s2eex */,E(new T5(0,(1+_e8/* s2eep */|0)+_ef/* s2eeE */|0,E(_e5/* s2eee */),_e6/* s2eef */,E(_e7/* s2eeo */),E(_ed/* s2eey */))),E(new T5(0,1+_e2/* s2ee7 */|0,E(_dX/* s2ee2 */),_dY/* s2ee3 */,E(_dR/* Data.Map.Base.Tip */),E(_e1/* s2ee6 */))));
              },
              _eh/* s2ef2 */ = E(_ed/* s2eey */);
              if(!_eh/* s2ef2 */._){
                return new F(function(){return _ee/* s2eeD */(_eh/* s2ef2 */.a);});
              }else{
                return new F(function(){return _ee/* s2eeD */(0);});
              }
            }else{
              return new T5(0,(1+_e4/* s2eed */|0)+_e2/* s2ee7 */|0,E(_e5/* s2eee */),_e6/* s2eef */,E(_e7/* s2eeo */),E(new T5(0,(1+_e2/* s2ee7 */|0)+_ea/* s2eev */|0,E(_dX/* s2ee2 */),_dY/* s2ee3 */,E(_e9/* s2eeu */),E(_e1/* s2ee6 */))));
            }
          }else{
            return E(_dV/* Data.Map.Base.lvl13 */);
          }
        }else{
          return E(_dV/* Data.Map.Base.lvl13 */);
        }
      }
    }else{
      return new T5(0,1+_e2/* s2ee7 */|0,E(_dX/* s2ee2 */),_dY/* s2ee3 */,E(_dR/* Data.Map.Base.Tip */),E(_e1/* s2ee6 */));
    }
  }else{
    var _ei/* s2efg */ = E(_dZ/* s2ee4 */);
    if(!_ei/* s2efg */._){
      var _ej/* s2efh */ = _ei/* s2efg */.a,
      _ek/* s2efi */ = _ei/* s2efg */.b,
      _el/* s2efj */ = _ei/* s2efg */.c,
      _em/* s2efl */ = _ei/* s2efg */.e,
      _en/* s2efm */ = E(_ei/* s2efg */.d);
      if(!_en/* s2efm */._){
        var _eo/* s2efn */ = _en/* s2efm */.a,
        _ep/* s2efs */ = E(_em/* s2efl */);
        if(!_ep/* s2efs */._){
          var _eq/* s2eft */ = _ep/* s2efs */.a,
          _er/* s2efu */ = _ep/* s2efs */.b,
          _es/* s2efv */ = _ep/* s2efs */.c,
          _et/* s2efw */ = _ep/* s2efs */.d;
          if(_eq/* s2eft */>=(imul/* EXTERNAL */(2, _eo/* s2efn */)|0)){
            var _eu/* s2efB */ = function(_ev/* s2efC */){
              var _ew/* s2efD */ = E(_ep/* s2efs */.e);
              return (_ew/* s2efD */._==0) ? new T5(0,1+_ej/* s2efh */|0,E(_er/* s2efu */),_es/* s2efv */,E(new T5(0,(1+_eo/* s2efn */|0)+_ev/* s2efC */|0,E(_ek/* s2efi */),_el/* s2efj */,E(_en/* s2efm */),E(_et/* s2efw */))),E(new T5(0,1+_ew/* s2efD */.a|0,E(_dX/* s2ee2 */),_dY/* s2ee3 */,E(_ew/* s2efD */),E(_dR/* Data.Map.Base.Tip */)))) : new T5(0,1+_ej/* s2efh */|0,E(_er/* s2efu */),_es/* s2efv */,E(new T5(0,(1+_eo/* s2efn */|0)+_ev/* s2efC */|0,E(_ek/* s2efi */),_el/* s2efj */,E(_en/* s2efm */),E(_et/* s2efw */))),E(new T5(0,1,E(_dX/* s2ee2 */),_dY/* s2ee3 */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */))));
            },
            _ex/* s2efW */ = E(_et/* s2efw */);
            if(!_ex/* s2efW */._){
              return new F(function(){return _eu/* s2efB */(_ex/* s2efW */.a);});
            }else{
              return new F(function(){return _eu/* s2efB */(0);});
            }
          }else{
            return new T5(0,1+_ej/* s2efh */|0,E(_ek/* s2efi */),_el/* s2efj */,E(_en/* s2efm */),E(new T5(0,1+_eq/* s2eft */|0,E(_dX/* s2ee2 */),_dY/* s2ee3 */,E(_ep/* s2efs */),E(_dR/* Data.Map.Base.Tip */))));
          }
        }else{
          return new T5(0,3,E(_ek/* s2efi */),_el/* s2efj */,E(_en/* s2efm */),E(new T5(0,1,E(_dX/* s2ee2 */),_dY/* s2ee3 */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */))));
        }
      }else{
        var _ey/* s2eg8 */ = E(_em/* s2efl */);
        return (_ey/* s2eg8 */._==0) ? new T5(0,3,E(_ey/* s2eg8 */.b),_ey/* s2eg8 */.c,E(new T5(0,1,E(_ek/* s2efi */),_el/* s2efj */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */))),E(new T5(0,1,E(_dX/* s2ee2 */),_dY/* s2ee3 */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)))) : new T5(0,2,E(_dX/* s2ee2 */),_dY/* s2ee3 */,E(_ei/* s2efg */),E(_dR/* Data.Map.Base.Tip */));
      }
    }else{
      return new T5(0,1,E(_dX/* s2ee2 */),_dY/* s2ee3 */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
    }
  }
},
_ez/* lvl15 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Failure in Data.Map.balanceR"));
}),
_eA/* $wpoly_fail3 */ = function(_eB/* s2eiN */){
  return new F(function(){return err/* EXTERNAL */(_ez/* Data.Map.Base.lvl15 */);});
},
_eC/* lvl16 */ = new T(function(){
  return B(_eA/* Data.Map.Base.$wpoly_fail3 */(_/* EXTERNAL */));
}),
_eD/* balanceR */ = function(_eE/* s2eiO */, _eF/* s2eiP */, _eG/* s2eiQ */, _eH/* s2eiR */){
  var _eI/* s2eiS */ = E(_eG/* s2eiQ */);
  if(!_eI/* s2eiS */._){
    var _eJ/* s2eiT */ = _eI/* s2eiS */.a,
    _eK/* s2eiY */ = E(_eH/* s2eiR */);
    if(!_eK/* s2eiY */._){
      var _eL/* s2eiZ */ = _eK/* s2eiY */.a,
      _eM/* s2ej0 */ = _eK/* s2eiY */.b,
      _eN/* s2ej1 */ = _eK/* s2eiY */.c;
      if(_eL/* s2eiZ */<=(imul/* EXTERNAL */(3, _eJ/* s2eiT */)|0)){
        return new T5(0,(1+_eJ/* s2eiT */|0)+_eL/* s2eiZ */|0,E(_eE/* s2eiO */),_eF/* s2eiP */,E(_eI/* s2eiS */),E(_eK/* s2eiY */));
      }else{
        var _eO/* s2eja */ = E(_eK/* s2eiY */.d);
        if(!_eO/* s2eja */._){
          var _eP/* s2ejb */ = _eO/* s2eja */.a,
          _eQ/* s2ejc */ = _eO/* s2eja */.b,
          _eR/* s2ejd */ = _eO/* s2eja */.c,
          _eS/* s2eje */ = _eO/* s2eja */.d,
          _eT/* s2ejg */ = E(_eK/* s2eiY */.e);
          if(!_eT/* s2ejg */._){
            var _eU/* s2ejh */ = _eT/* s2ejg */.a;
            if(_eP/* s2ejb */>=(imul/* EXTERNAL */(2, _eU/* s2ejh */)|0)){
              var _eV/* s2ejp */ = function(_eW/* s2ejq */){
                var _eX/* s2ejr */ = E(_eE/* s2eiO */),
                _eY/* s2ejs */ = E(_eO/* s2eja */.e);
                return (_eY/* s2ejs */._==0) ? new T5(0,(1+_eJ/* s2eiT */|0)+_eL/* s2eiZ */|0,E(_eQ/* s2ejc */),_eR/* s2ejd */,E(new T5(0,(1+_eJ/* s2eiT */|0)+_eW/* s2ejq */|0,E(_eX/* s2ejr */),_eF/* s2eiP */,E(_eI/* s2eiS */),E(_eS/* s2eje */))),E(new T5(0,(1+_eU/* s2ejh */|0)+_eY/* s2ejs */.a|0,E(_eM/* s2ej0 */),_eN/* s2ej1 */,E(_eY/* s2ejs */),E(_eT/* s2ejg */)))) : new T5(0,(1+_eJ/* s2eiT */|0)+_eL/* s2eiZ */|0,E(_eQ/* s2ejc */),_eR/* s2ejd */,E(new T5(0,(1+_eJ/* s2eiT */|0)+_eW/* s2ejq */|0,E(_eX/* s2ejr */),_eF/* s2eiP */,E(_eI/* s2eiS */),E(_eS/* s2eje */))),E(new T5(0,1+_eU/* s2ejh */|0,E(_eM/* s2ej0 */),_eN/* s2ej1 */,E(_dR/* Data.Map.Base.Tip */),E(_eT/* s2ejg */))));
              },
              _eZ/* s2ejN */ = E(_eS/* s2eje */);
              if(!_eZ/* s2ejN */._){
                return new F(function(){return _eV/* s2ejp */(_eZ/* s2ejN */.a);});
              }else{
                return new F(function(){return _eV/* s2ejp */(0);});
              }
            }else{
              return new T5(0,(1+_eJ/* s2eiT */|0)+_eL/* s2eiZ */|0,E(_eM/* s2ej0 */),_eN/* s2ej1 */,E(new T5(0,(1+_eJ/* s2eiT */|0)+_eP/* s2ejb */|0,E(_eE/* s2eiO */),_eF/* s2eiP */,E(_eI/* s2eiS */),E(_eO/* s2eja */))),E(_eT/* s2ejg */));
            }
          }else{
            return E(_eC/* Data.Map.Base.lvl16 */);
          }
        }else{
          return E(_eC/* Data.Map.Base.lvl16 */);
        }
      }
    }else{
      return new T5(0,1+_eJ/* s2eiT */|0,E(_eE/* s2eiO */),_eF/* s2eiP */,E(_eI/* s2eiS */),E(_dR/* Data.Map.Base.Tip */));
    }
  }else{
    var _f0/* s2ek1 */ = E(_eH/* s2eiR */);
    if(!_f0/* s2ek1 */._){
      var _f1/* s2ek2 */ = _f0/* s2ek1 */.a,
      _f2/* s2ek3 */ = _f0/* s2ek1 */.b,
      _f3/* s2ek4 */ = _f0/* s2ek1 */.c,
      _f4/* s2ek6 */ = _f0/* s2ek1 */.e,
      _f5/* s2ek7 */ = E(_f0/* s2ek1 */.d);
      if(!_f5/* s2ek7 */._){
        var _f6/* s2ek8 */ = _f5/* s2ek7 */.a,
        _f7/* s2ek9 */ = _f5/* s2ek7 */.b,
        _f8/* s2eka */ = _f5/* s2ek7 */.c,
        _f9/* s2ekb */ = _f5/* s2ek7 */.d,
        _fa/* s2ekd */ = E(_f4/* s2ek6 */);
        if(!_fa/* s2ekd */._){
          var _fb/* s2eke */ = _fa/* s2ekd */.a;
          if(_f6/* s2ek8 */>=(imul/* EXTERNAL */(2, _fb/* s2eke */)|0)){
            var _fc/* s2ekm */ = function(_fd/* s2ekn */){
              var _fe/* s2eko */ = E(_eE/* s2eiO */),
              _ff/* s2ekp */ = E(_f5/* s2ek7 */.e);
              return (_ff/* s2ekp */._==0) ? new T5(0,1+_f1/* s2ek2 */|0,E(_f7/* s2ek9 */),_f8/* s2eka */,E(new T5(0,1+_fd/* s2ekn */|0,E(_fe/* s2eko */),_eF/* s2eiP */,E(_dR/* Data.Map.Base.Tip */),E(_f9/* s2ekb */))),E(new T5(0,(1+_fb/* s2eke */|0)+_ff/* s2ekp */.a|0,E(_f2/* s2ek3 */),_f3/* s2ek4 */,E(_ff/* s2ekp */),E(_fa/* s2ekd */)))) : new T5(0,1+_f1/* s2ek2 */|0,E(_f7/* s2ek9 */),_f8/* s2eka */,E(new T5(0,1+_fd/* s2ekn */|0,E(_fe/* s2eko */),_eF/* s2eiP */,E(_dR/* Data.Map.Base.Tip */),E(_f9/* s2ekb */))),E(new T5(0,1+_fb/* s2eke */|0,E(_f2/* s2ek3 */),_f3/* s2ek4 */,E(_dR/* Data.Map.Base.Tip */),E(_fa/* s2ekd */))));
            },
            _fg/* s2ekG */ = E(_f9/* s2ekb */);
            if(!_fg/* s2ekG */._){
              return new F(function(){return _fc/* s2ekm */(_fg/* s2ekG */.a);});
            }else{
              return new F(function(){return _fc/* s2ekm */(0);});
            }
          }else{
            return new T5(0,1+_f1/* s2ek2 */|0,E(_f2/* s2ek3 */),_f3/* s2ek4 */,E(new T5(0,1+_f6/* s2ek8 */|0,E(_eE/* s2eiO */),_eF/* s2eiP */,E(_dR/* Data.Map.Base.Tip */),E(_f5/* s2ek7 */))),E(_fa/* s2ekd */));
          }
        }else{
          return new T5(0,3,E(_f7/* s2ek9 */),_f8/* s2eka */,E(new T5(0,1,E(_eE/* s2eiO */),_eF/* s2eiP */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */))),E(new T5(0,1,E(_f2/* s2ek3 */),_f3/* s2ek4 */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */))));
        }
      }else{
        var _fh/* s2ekT */ = E(_f4/* s2ek6 */);
        return (_fh/* s2ekT */._==0) ? new T5(0,3,E(_f2/* s2ek3 */),_f3/* s2ek4 */,E(new T5(0,1,E(_eE/* s2eiO */),_eF/* s2eiP */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */))),E(_fh/* s2ekT */)) : new T5(0,2,E(_eE/* s2eiO */),_eF/* s2eiP */,E(_dR/* Data.Map.Base.Tip */),E(_f0/* s2ek1 */));
      }
    }else{
      return new T5(0,1,E(_eE/* s2eiO */),_eF/* s2eiP */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
    }
  }
},
_fi/* $sinsert_$sgo10 */ = function(_fj/* sgzq */, _fk/* sgzr */, _fl/* sgzs */){
  var _fm/* sgzt */ = E(_fj/* sgzq */),
  _fn/* sgzu */ = E(_fl/* sgzs */);
  if(!_fn/* sgzu */._){
    var _fo/* sgzv */ = _fn/* sgzu */.a,
    _fp/* sgzw */ = _fn/* sgzu */.b,
    _fq/* sgzx */ = _fn/* sgzu */.c,
    _fr/* sgzy */ = _fn/* sgzu */.d,
    _fs/* sgzz */ = _fn/* sgzu */.e;
    switch(E(_fm/* sgzt */)){
      case 0:
        switch(E(_fp/* sgzw */)){
          case 0:
            return new T5(0,_fo/* sgzv */,E(_bn/* LudoJS.Blue */),_fk/* sgzr */,E(_fr/* sgzy */),E(_fs/* sgzz */));
          case 1:
            return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bo/* LudoJS.Green */, _fq/* sgzx */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bn/* LudoJS.Blue */, _fk/* sgzr */, _fr/* sgzy */)), _fs/* sgzz */);});
            break;
          case 2:
            return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bp/* LudoJS.Red */, _fq/* sgzx */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bn/* LudoJS.Blue */, _fk/* sgzr */, _fr/* sgzy */)), _fs/* sgzz */);});
            break;
          default:
            return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bq/* LudoJS.Yellow */, _fq/* sgzx */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bn/* LudoJS.Blue */, _fk/* sgzr */, _fr/* sgzy */)), _fs/* sgzz */);});
        }
        break;
      case 1:
        switch(E(_fp/* sgzw */)){
          case 0:
            return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bn/* LudoJS.Blue */, _fq/* sgzx */, _fr/* sgzy */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bo/* LudoJS.Green */, _fk/* sgzr */, _fs/* sgzz */)));});
            break;
          case 1:
            return new T5(0,_fo/* sgzv */,E(_bo/* LudoJS.Green */),_fk/* sgzr */,E(_fr/* sgzy */),E(_fs/* sgzz */));
          case 2:
            return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bp/* LudoJS.Red */, _fq/* sgzx */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bo/* LudoJS.Green */, _fk/* sgzr */, _fr/* sgzy */)), _fs/* sgzz */);});
            break;
          default:
            return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bq/* LudoJS.Yellow */, _fq/* sgzx */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bo/* LudoJS.Green */, _fk/* sgzr */, _fr/* sgzy */)), _fs/* sgzz */);});
        }
        break;
      case 2:
        switch(E(_fp/* sgzw */)){
          case 0:
            return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bn/* LudoJS.Blue */, _fq/* sgzx */, _fr/* sgzy */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bp/* LudoJS.Red */, _fk/* sgzr */, _fs/* sgzz */)));});
            break;
          case 1:
            return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bo/* LudoJS.Green */, _fq/* sgzx */, _fr/* sgzy */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bp/* LudoJS.Red */, _fk/* sgzr */, _fs/* sgzz */)));});
            break;
          case 2:
            return new T5(0,_fo/* sgzv */,E(_bp/* LudoJS.Red */),_fk/* sgzr */,E(_fr/* sgzy */),E(_fs/* sgzz */));
          default:
            return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bq/* LudoJS.Yellow */, _fq/* sgzx */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bp/* LudoJS.Red */, _fk/* sgzr */, _fr/* sgzy */)), _fs/* sgzz */);});
        }
        break;
      default:
        switch(E(_fp/* sgzw */)){
          case 0:
            return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bn/* LudoJS.Blue */, _fq/* sgzx */, _fr/* sgzy */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bq/* LudoJS.Yellow */, _fk/* sgzr */, _fs/* sgzz */)));});
            break;
          case 1:
            return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bo/* LudoJS.Green */, _fq/* sgzx */, _fr/* sgzy */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bq/* LudoJS.Yellow */, _fk/* sgzr */, _fs/* sgzz */)));});
            break;
          case 2:
            return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bp/* LudoJS.Red */, _fq/* sgzx */, _fr/* sgzy */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bq/* LudoJS.Yellow */, _fk/* sgzr */, _fs/* sgzz */)));});
            break;
          default:
            return new T5(0,_fo/* sgzv */,E(_bq/* LudoJS.Yellow */),_fk/* sgzr */,E(_fr/* sgzy */),E(_fs/* sgzz */));
        }
    }
  }else{
    return new T5(0,1,E(_fm/* sgzt */),_fk/* sgzr */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
  }
},
_ft/* poly_go10 */ = function(_fu/* sgA3 */, _fv/* sgA4 */){
  while(1){
    var _fw/* sgA5 */ = E(_fv/* sgA4 */);
    if(!_fw/* sgA5 */._){
      return E(_fu/* sgA3 */);
    }else{
      var _fx/* sgA8 */ = E(_fw/* sgA5 */.a),
      _fy/*  sgA3 */ = B(_fi/* LudoJS.$sinsert_$sgo10 */(_fx/* sgA8 */.a, _fx/* sgA8 */.b, _fu/* sgA3 */));
      _fu/* sgA3 */ = _fy/*  sgA3 */;
      _fv/* sgA4 */ = _fw/* sgA5 */.b;
      continue;
    }
  }
},
_fz/* $sfromList_$spoly_go10 */ = function(_fA/* sgzY */, _fB/* sgzZ */, _fC/* sgA0 */, _fD/* sgA1 */){
  return new F(function(){return _ft/* LudoJS.poly_go10 */(B(_fi/* LudoJS.$sinsert_$sgo10 */(_fB/* sgzZ */, _fC/* sgA0 */, _fA/* sgzY */)), _fD/* sgA1 */);});
},
_fE/* singleton */ = function(_fF/* s2e3X */, _fG/* s2e3Y */){
  return new T5(0,1,E(_fF/* s2e3X */),_fG/* s2e3Y */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
},
_fH/* insertMax */ = function(_fI/* s2eoG */, _fJ/* s2eoH */, _fK/* s2eoI */){
  var _fL/* s2eoJ */ = E(_fK/* s2eoI */);
  if(!_fL/* s2eoJ */._){
    return new F(function(){return _eD/* Data.Map.Base.balanceR */(_fL/* s2eoJ */.b, _fL/* s2eoJ */.c, _fL/* s2eoJ */.d, B(_fH/* Data.Map.Base.insertMax */(_fI/* s2eoG */, _fJ/* s2eoH */, _fL/* s2eoJ */.e)));});
  }else{
    return new F(function(){return _fE/* Data.Map.Base.singleton */(_fI/* s2eoG */, _fJ/* s2eoH */);});
  }
},
_fM/* insertMin */ = function(_fN/* s2ehs */, _fO/* s2eht */, _fP/* s2ehu */){
  var _fQ/* s2ehv */ = E(_fP/* s2ehu */);
  if(!_fQ/* s2ehv */._){
    return new F(function(){return _dW/* Data.Map.Base.balanceL */(_fQ/* s2ehv */.b, _fQ/* s2ehv */.c, B(_fM/* Data.Map.Base.insertMin */(_fN/* s2ehs */, _fO/* s2eht */, _fQ/* s2ehv */.d)), _fQ/* s2ehv */.e);});
  }else{
    return new F(function(){return _fE/* Data.Map.Base.singleton */(_fN/* s2ehs */, _fO/* s2eht */);});
  }
},
_fR/* link_$sinsertMin */ = function(_fS/* s2ehk */, _fT/* s2ehl */, _fU/* s2ehm */, _fV/* s2ehn */, _fW/* s2eho */, _fX/* s2ehp */, _fY/* s2ehq */){
  return new F(function(){return _dW/* Data.Map.Base.balanceL */(_fV/* s2ehn */, _fW/* s2eho */, B(_fM/* Data.Map.Base.insertMin */(_fS/* s2ehk */, _fT/* s2ehl */, _fX/* s2ehp */)), _fY/* s2ehq */);});
},
_fZ/* link_$slink1 */ = function(_g0/* s2evt */, _g1/* s2evu */, _g2/* s2evv */, _g3/* s2evw */, _g4/* s2evx */, _g5/* s2evy */, _g6/* s2evz */, _g7/* s2evA */){
  var _g8/* s2evB */ = E(_g2/* s2evv */);
  if(!_g8/* s2evB */._){
    var _g9/* s2evC */ = _g8/* s2evB */.a,
    _ga/* s2evD */ = _g8/* s2evB */.b,
    _gb/* s2evE */ = _g8/* s2evB */.c,
    _gc/* s2evF */ = _g8/* s2evB */.d,
    _gd/* s2evG */ = _g8/* s2evB */.e;
    if((imul/* EXTERNAL */(3, _g9/* s2evC */)|0)>=_g3/* s2evw */){
      if((imul/* EXTERNAL */(3, _g3/* s2evw */)|0)>=_g9/* s2evC */){
        return new T5(0,(_g9/* s2evC */+_g3/* s2evw */|0)+1|0,E(_g0/* s2evt */),_g1/* s2evu */,E(_g8/* s2evB */),E(new T5(0,_g3/* s2evw */,E(_g4/* s2evx */),_g5/* s2evy */,E(_g6/* s2evz */),E(_g7/* s2evA */))));
      }else{
        return new F(function(){return _eD/* Data.Map.Base.balanceR */(_ga/* s2evD */, _gb/* s2evE */, _gc/* s2evF */, B(_fZ/* Data.Map.Base.link_$slink1 */(_g0/* s2evt */, _g1/* s2evu */, _gd/* s2evG */, _g3/* s2evw */, _g4/* s2evx */, _g5/* s2evy */, _g6/* s2evz */, _g7/* s2evA */)));});
      }
    }else{
      return new F(function(){return _dW/* Data.Map.Base.balanceL */(_g4/* s2evx */, _g5/* s2evy */, B(_ge/* Data.Map.Base.link_$slink */(_g0/* s2evt */, _g1/* s2evu */, _g9/* s2evC */, _ga/* s2evD */, _gb/* s2evE */, _gc/* s2evF */, _gd/* s2evG */, _g6/* s2evz */)), _g7/* s2evA */);});
    }
  }else{
    return new F(function(){return _fR/* Data.Map.Base.link_$sinsertMin */(_g0/* s2evt */, _g1/* s2evu */, _g3/* s2evw */, _g4/* s2evx */, _g5/* s2evy */, _g6/* s2evz */, _g7/* s2evA */);});
  }
},
_ge/* link_$slink */ = function(_gf/* s2ev2 */, _gg/* s2ev3 */, _gh/* s2ev4 */, _gi/* s2ev5 */, _gj/* s2ev6 */, _gk/* s2ev7 */, _gl/* s2ev8 */, _gm/* s2ev9 */){
  var _gn/* s2eva */ = E(_gm/* s2ev9 */);
  if(!_gn/* s2eva */._){
    var _go/* s2evb */ = _gn/* s2eva */.a,
    _gp/* s2evc */ = _gn/* s2eva */.b,
    _gq/* s2evd */ = _gn/* s2eva */.c,
    _gr/* s2eve */ = _gn/* s2eva */.d,
    _gs/* s2evf */ = _gn/* s2eva */.e;
    if((imul/* EXTERNAL */(3, _gh/* s2ev4 */)|0)>=_go/* s2evb */){
      if((imul/* EXTERNAL */(3, _go/* s2evb */)|0)>=_gh/* s2ev4 */){
        return new T5(0,(_gh/* s2ev4 */+_go/* s2evb */|0)+1|0,E(_gf/* s2ev2 */),_gg/* s2ev3 */,E(new T5(0,_gh/* s2ev4 */,E(_gi/* s2ev5 */),_gj/* s2ev6 */,E(_gk/* s2ev7 */),E(_gl/* s2ev8 */))),E(_gn/* s2eva */));
      }else{
        return new F(function(){return _eD/* Data.Map.Base.balanceR */(_gi/* s2ev5 */, _gj/* s2ev6 */, _gk/* s2ev7 */, B(_fZ/* Data.Map.Base.link_$slink1 */(_gf/* s2ev2 */, _gg/* s2ev3 */, _gl/* s2ev8 */, _go/* s2evb */, _gp/* s2evc */, _gq/* s2evd */, _gr/* s2eve */, _gs/* s2evf */)));});
      }
    }else{
      return new F(function(){return _dW/* Data.Map.Base.balanceL */(_gp/* s2evc */, _gq/* s2evd */, B(_ge/* Data.Map.Base.link_$slink */(_gf/* s2ev2 */, _gg/* s2ev3 */, _gh/* s2ev4 */, _gi/* s2ev5 */, _gj/* s2ev6 */, _gk/* s2ev7 */, _gl/* s2ev8 */, _gr/* s2eve */)), _gs/* s2evf */);});
    }
  }else{
    return new F(function(){return _fH/* Data.Map.Base.insertMax */(_gf/* s2ev2 */, _gg/* s2ev3 */, new T5(0,_gh/* s2ev4 */,E(_gi/* s2ev5 */),_gj/* s2ev6 */,E(_gk/* s2ev7 */),E(_gl/* s2ev8 */)));});
  }
},
_gt/* link */ = function(_gu/* s2evT */, _gv/* s2evU */, _gw/* s2evV */, _gx/* s2evW */){
  var _gy/* s2evX */ = E(_gw/* s2evV */);
  if(!_gy/* s2evX */._){
    var _gz/* s2evY */ = _gy/* s2evX */.a,
    _gA/* s2evZ */ = _gy/* s2evX */.b,
    _gB/* s2ew0 */ = _gy/* s2evX */.c,
    _gC/* s2ew1 */ = _gy/* s2evX */.d,
    _gD/* s2ew2 */ = _gy/* s2evX */.e,
    _gE/* s2ew3 */ = E(_gx/* s2evW */);
    if(!_gE/* s2ew3 */._){
      var _gF/* s2ew4 */ = _gE/* s2ew3 */.a,
      _gG/* s2ew5 */ = _gE/* s2ew3 */.b,
      _gH/* s2ew6 */ = _gE/* s2ew3 */.c,
      _gI/* s2ew7 */ = _gE/* s2ew3 */.d,
      _gJ/* s2ew8 */ = _gE/* s2ew3 */.e;
      if((imul/* EXTERNAL */(3, _gz/* s2evY */)|0)>=_gF/* s2ew4 */){
        if((imul/* EXTERNAL */(3, _gF/* s2ew4 */)|0)>=_gz/* s2evY */){
          return new T5(0,(_gz/* s2evY */+_gF/* s2ew4 */|0)+1|0,E(_gu/* s2evT */),_gv/* s2evU */,E(_gy/* s2evX */),E(_gE/* s2ew3 */));
        }else{
          return new F(function(){return _eD/* Data.Map.Base.balanceR */(_gA/* s2evZ */, _gB/* s2ew0 */, _gC/* s2ew1 */, B(_fZ/* Data.Map.Base.link_$slink1 */(_gu/* s2evT */, _gv/* s2evU */, _gD/* s2ew2 */, _gF/* s2ew4 */, _gG/* s2ew5 */, _gH/* s2ew6 */, _gI/* s2ew7 */, _gJ/* s2ew8 */)));});
        }
      }else{
        return new F(function(){return _dW/* Data.Map.Base.balanceL */(_gG/* s2ew5 */, _gH/* s2ew6 */, B(_ge/* Data.Map.Base.link_$slink */(_gu/* s2evT */, _gv/* s2evU */, _gz/* s2evY */, _gA/* s2evZ */, _gB/* s2ew0 */, _gC/* s2ew1 */, _gD/* s2ew2 */, _gI/* s2ew7 */)), _gJ/* s2ew8 */);});
      }
    }else{
      return new F(function(){return _fH/* Data.Map.Base.insertMax */(_gu/* s2evT */, _gv/* s2evU */, _gy/* s2evX */);});
    }
  }else{
    return new F(function(){return _fM/* Data.Map.Base.insertMin */(_gu/* s2evT */, _gv/* s2evU */, _gx/* s2evW */);});
  }
},
_gK/* $s$wpoly_create */ = function(_gL/* sgxr */, _gM/* sgxs */, _gN/* sgxt */, _gO/* sgxu */){
  var _gP/* sgxv */ = E(_gL/* sgxr */);
  if(_gP/* sgxv */==1){
    var _gQ/* sgyt */ = E(_gO/* sgxu */);
    if(!_gQ/* sgyt */._){
      return new T3(0,new T(function(){
        return new T5(0,1,E(_gM/* sgxs */),_gN/* sgxt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
      }),_4/* GHC.Types.[] */,_4/* GHC.Types.[] */);
    }else{
      var _gR/* sgyz */ = E(_gQ/* sgyt */.a).a;
      switch(E(_gM/* sgxs */)){
        case 0:
          switch(E(_gR/* sgyz */)){
            case 0:
              return new T3(0,new T5(0,1,E(_bn/* LudoJS.Blue */),_gN/* sgxt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_gQ/* sgyt */);
            case 1:
              return new T3(0,new T5(0,1,E(_bn/* LudoJS.Blue */),_gN/* sgxt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_gQ/* sgyt */,_4/* GHC.Types.[] */);
            case 2:
              return new T3(0,new T5(0,1,E(_bn/* LudoJS.Blue */),_gN/* sgxt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_gQ/* sgyt */,_4/* GHC.Types.[] */);
            default:
              return new T3(0,new T5(0,1,E(_bn/* LudoJS.Blue */),_gN/* sgxt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_gQ/* sgyt */,_4/* GHC.Types.[] */);
          }
          break;
        case 1:
          switch(E(_gR/* sgyz */)){
            case 2:
              return new T3(0,new T5(0,1,E(_bo/* LudoJS.Green */),_gN/* sgxt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_gQ/* sgyt */,_4/* GHC.Types.[] */);
            case 3:
              return new T3(0,new T5(0,1,E(_bo/* LudoJS.Green */),_gN/* sgxt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_gQ/* sgyt */,_4/* GHC.Types.[] */);
            default:
              return new T3(0,new T5(0,1,E(_bo/* LudoJS.Green */),_gN/* sgxt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_gQ/* sgyt */);
          }
          break;
        case 2:
          return (E(_gR/* sgyz */)==3) ? new T3(0,new T5(0,1,E(_bp/* LudoJS.Red */),_gN/* sgxt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_gQ/* sgyt */,_4/* GHC.Types.[] */) : new T3(0,new T5(0,1,E(_bp/* LudoJS.Red */),_gN/* sgxt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_gQ/* sgyt */);
        default:
          var _gS/* sgyO */ = E(_gR/* sgyz */);
          return new T3(0,new T5(0,1,E(_bq/* LudoJS.Yellow */),_gN/* sgxt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_gQ/* sgyt */);
      }
    }
  }else{
    var _gT/* sgxx */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgxv */>>1, _gM/* sgxs */, _gN/* sgxt */, _gO/* sgxu */)),
    _gU/* sgxy */ = _gT/* sgxx */.a,
    _gV/* sgxA */ = _gT/* sgxx */.c,
    _gW/* sgxB */ = E(_gT/* sgxx */.b);
    if(!_gW/* sgxB */._){
      return new T3(0,_gU/* sgxy */,_4/* GHC.Types.[] */,_gV/* sgxA */);
    }else{
      var _gX/* sgxE */ = E(_gW/* sgxB */.a),
      _gY/* sgxF */ = _gX/* sgxE */.a,
      _gZ/* sgxG */ = _gX/* sgxE */.b,
      _h0/* sgxH */ = E(_gW/* sgxB */.b);
      if(!_h0/* sgxH */._){
        return new T3(0,new T(function(){
          return B(_fH/* Data.Map.Base.insertMax */(_gY/* sgxF */, _gZ/* sgxG */, _gU/* sgxy */));
        }),_4/* GHC.Types.[] */,_gV/* sgxA */);
      }else{
        var _h1/* sgxK */ = _h0/* sgxH */.b,
        _h2/* sgxL */ = E(_h0/* sgxH */.a),
        _h3/* sgxM */ = _h2/* sgxL */.a,
        _h4/* sgxN */ = _h2/* sgxL */.b;
        switch(E(_gY/* sgxF */)){
          case 0:
            switch(E(_h3/* sgxM */)){
              case 0:
                return new T3(0,_gU/* sgxy */,_4/* GHC.Types.[] */,_gW/* sgxB */);
              case 1:
                var _h5/* sgxR */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgxv */>>1, _bo/* LudoJS.Green */, _h4/* sgxN */, _h1/* sgxK */));
                return new T3(0,new T(function(){
                  return B(_gt/* Data.Map.Base.link */(_bn/* LudoJS.Blue */, _gZ/* sgxG */, _gU/* sgxy */, _h5/* sgxR */.a));
                }),_h5/* sgxR */.b,_h5/* sgxR */.c);
              case 2:
                var _h6/* sgxX */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgxv */>>1, _bp/* LudoJS.Red */, _h4/* sgxN */, _h1/* sgxK */));
                return new T3(0,new T(function(){
                  return B(_gt/* Data.Map.Base.link */(_bn/* LudoJS.Blue */, _gZ/* sgxG */, _gU/* sgxy */, _h6/* sgxX */.a));
                }),_h6/* sgxX */.b,_h6/* sgxX */.c);
              default:
                var _h7/* sgy3 */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgxv */>>1, _bq/* LudoJS.Yellow */, _h4/* sgxN */, _h1/* sgxK */));
                return new T3(0,new T(function(){
                  return B(_gt/* Data.Map.Base.link */(_bn/* LudoJS.Blue */, _gZ/* sgxG */, _gU/* sgxy */, _h7/* sgy3 */.a));
                }),_h7/* sgy3 */.b,_h7/* sgy3 */.c);
            }
            break;
          case 1:
            switch(E(_h3/* sgxM */)){
              case 2:
                var _h8/* sgya */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgxv */>>1, _bp/* LudoJS.Red */, _h4/* sgxN */, _h1/* sgxK */));
                return new T3(0,new T(function(){
                  return B(_gt/* Data.Map.Base.link */(_bo/* LudoJS.Green */, _gZ/* sgxG */, _gU/* sgxy */, _h8/* sgya */.a));
                }),_h8/* sgya */.b,_h8/* sgya */.c);
              case 3:
                var _h9/* sgyg */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgxv */>>1, _bq/* LudoJS.Yellow */, _h4/* sgxN */, _h1/* sgxK */));
                return new T3(0,new T(function(){
                  return B(_gt/* Data.Map.Base.link */(_bo/* LudoJS.Green */, _gZ/* sgxG */, _gU/* sgxy */, _h9/* sgyg */.a));
                }),_h9/* sgyg */.b,_h9/* sgyg */.c);
              default:
                return new T3(0,_gU/* sgxy */,_4/* GHC.Types.[] */,_gW/* sgxB */);
            }
            break;
          case 2:
            if(E(_h3/* sgxM */)==3){
              var _ha/* sgyn */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgxv */>>1, _bq/* LudoJS.Yellow */, _h4/* sgxN */, _h1/* sgxK */));
              return new T3(0,new T(function(){
                return B(_gt/* Data.Map.Base.link */(_bp/* LudoJS.Red */, _gZ/* sgxG */, _gU/* sgxy */, _ha/* sgyn */.a));
              }),_ha/* sgyn */.b,_ha/* sgyn */.c);
            }else{
              return new T3(0,_gU/* sgxy */,_4/* GHC.Types.[] */,_gW/* sgxB */);
            }
            break;
          default:
            var _hb/* sgys */ = E(_h3/* sgxM */);
            return new T3(0,_gU/* sgxy */,_4/* GHC.Types.[] */,_gW/* sgxB */);
        }
      }
    }
  }
},
_hc/* $spoly_go10 */ = function(_hd/* sgzR */, _he/* sgzS */, _hf/* sgzT */){
  var _hg/* sgzU */ = E(_he/* sgzS */);
  return new F(function(){return _ft/* LudoJS.poly_go10 */(B(_fi/* LudoJS.$sinsert_$sgo10 */(_hg/* sgzU */.a, _hg/* sgzU */.b, _hd/* sgzR */)), _hf/* sgzT */);});
},
_hh/* $wpoly_go10 */ = function(_hi/* sgAU */, _hj/* sgAV */, _hk/* sgAW */){
  var _hl/* sgAX */ = E(_hk/* sgAW */);
  if(!_hl/* sgAX */._){
    return E(_hj/* sgAV */);
  }else{
    var _hm/* sgB0 */ = E(_hl/* sgAX */.a),
    _hn/* sgB1 */ = _hm/* sgB0 */.a,
    _ho/* sgB2 */ = _hm/* sgB0 */.b,
    _hp/* sgB3 */ = E(_hl/* sgAX */.b);
    if(!_hp/* sgB3 */._){
      return new F(function(){return _fH/* Data.Map.Base.insertMax */(_hn/* sgB1 */, _ho/* sgB2 */, _hj/* sgAV */);});
    }else{
      var _hq/* sgB6 */ = E(_hp/* sgB3 */.a),
      _hr/* sgB7 */ = _hq/* sgB6 */.a,
      _hs/* sgB9 */ = function(_ht/* sgBa */){
        var _hu/* sgBb */ = B(_gK/* LudoJS.$s$wpoly_create */(_hi/* sgAU */, _hr/* sgB7 */, _hq/* sgB6 */.b, _hp/* sgB3 */.b)),
        _hv/* sgBc */ = _hu/* sgBb */.a,
        _hw/* sgBf */ = E(_hu/* sgBb */.c);
        if(!_hw/* sgBf */._){
          return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(_hi/* sgAU */<<1, B(_gt/* Data.Map.Base.link */(_hn/* sgB1 */, _ho/* sgB2 */, _hj/* sgAV */, _hv/* sgBc */)), _hu/* sgBb */.b);});
        }else{
          return new F(function(){return _hc/* LudoJS.$spoly_go10 */(B(_gt/* Data.Map.Base.link */(_hn/* sgB1 */, _ho/* sgB2 */, _hj/* sgAV */, _hv/* sgBc */)), _hw/* sgBf */.a, _hw/* sgBf */.b);});
        }
      };
      switch(E(_hn/* sgB1 */)){
        case 0:
          switch(E(_hr/* sgB7 */)){
            case 0:
              return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(_hj/* sgAV */, _bn/* LudoJS.Blue */, _ho/* sgB2 */, _hp/* sgB3 */);});
              break;
            case 1:
              return new F(function(){return _hs/* sgB9 */(_/* EXTERNAL */);});
              break;
            case 2:
              return new F(function(){return _hs/* sgB9 */(_/* EXTERNAL */);});
              break;
            default:
              return new F(function(){return _hs/* sgB9 */(_/* EXTERNAL */);});
          }
          break;
        case 1:
          switch(E(_hr/* sgB7 */)){
            case 2:
              return new F(function(){return _hs/* sgB9 */(_/* EXTERNAL */);});
              break;
            case 3:
              return new F(function(){return _hs/* sgB9 */(_/* EXTERNAL */);});
              break;
            default:
              return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(_hj/* sgAV */, _bo/* LudoJS.Green */, _ho/* sgB2 */, _hp/* sgB3 */);});
          }
          break;
        case 2:
          if(E(_hr/* sgB7 */)==3){
            return new F(function(){return _hs/* sgB9 */(_/* EXTERNAL */);});
          }else{
            return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(_hj/* sgAV */, _bp/* LudoJS.Red */, _ho/* sgB2 */, _hp/* sgB3 */);});
          }
          break;
        default:
          var _hx/* sgBp */ = E(_hr/* sgB7 */);
          return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(_hj/* sgAV */, _bq/* LudoJS.Yellow */, _ho/* sgB2 */, _hp/* sgB3 */);});
      }
    }
  }
},
_hy/* $sfromList */ = function(_hz/* sgDf */){
  var _hA/* sgDg */ = E(_hz/* sgDf */);
  if(!_hA/* sgDg */._){
    return new T0(1);
  }else{
    var _hB/* sgDj */ = E(_hA/* sgDg */.a),
    _hC/* sgDk */ = _hB/* sgDj */.a,
    _hD/* sgDl */ = _hB/* sgDj */.b,
    _hE/* sgDm */ = E(_hA/* sgDg */.b);
    if(!_hE/* sgDm */._){
      return new T5(0,1,E(_hC/* sgDk */),_hD/* sgDl */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
    }else{
      var _hF/* sgDp */ = _hE/* sgDm */.b,
      _hG/* sgDq */ = E(_hE/* sgDm */.a),
      _hH/* sgDr */ = _hG/* sgDq */.a,
      _hI/* sgDs */ = _hG/* sgDq */.b;
      switch(E(_hC/* sgDk */)){
        case 0:
          switch(E(_hH/* sgDr */)){
            case 0:
              return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_bn/* LudoJS.Blue */),_hD/* sgDl */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _bn/* LudoJS.Blue */, _hI/* sgDs */, _hF/* sgDp */);});
              break;
            case 1:
              return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_bn/* LudoJS.Blue */),_hD/* sgDl */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hE/* sgDm */);});
              break;
            case 2:
              return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_bn/* LudoJS.Blue */),_hD/* sgDl */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hE/* sgDm */);});
              break;
            default:
              return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_bn/* LudoJS.Blue */),_hD/* sgDl */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hE/* sgDm */);});
          }
          break;
        case 1:
          var _hJ/* sgDz */ = E(_hH/* sgDr */);
          switch(_hJ/* sgDz */){
            case 2:
              return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_bo/* LudoJS.Green */),_hD/* sgDl */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hE/* sgDm */);});
              break;
            case 3:
              return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_bo/* LudoJS.Green */),_hD/* sgDl */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hE/* sgDm */);});
              break;
            default:
              return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_bo/* LudoJS.Green */),_hD/* sgDl */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hJ/* sgDz */, _hI/* sgDs */, _hF/* sgDp */);});
          }
          break;
        case 2:
          var _hK/* sgDD */ = E(_hH/* sgDr */);
          if(_hK/* sgDD */==3){
            return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_bp/* LudoJS.Red */),_hD/* sgDl */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hE/* sgDm */);});
          }else{
            return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_bp/* LudoJS.Red */),_hD/* sgDl */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hK/* sgDD */, _hI/* sgDs */, _hF/* sgDp */);});
          }
          break;
        default:
          return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_bq/* LudoJS.Yellow */),_hD/* sgDl */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), E(_hH/* sgDr */), _hI/* sgDs */, _hF/* sgDp */);});
      }
    }
  }
},
_hL/* $w$cshowsPrec */ = function(_hM/* sh1G */, _hN/* sh1H */){
  switch(E(_hM/* sh1G */)){
    case 0:
      return new F(function(){return _q/* GHC.Base.++ */(_1u/* LudoJS.$fFromAnyGameState14 */, _hN/* sh1H */);});
      break;
    case 1:
      return new F(function(){return _q/* GHC.Base.++ */(_1t/* LudoJS.$fFromAnyGameState13 */, _hN/* sh1H */);});
      break;
    case 2:
      return new F(function(){return _q/* GHC.Base.++ */(_1s/* LudoJS.$fFromAnyGameState12 */, _hN/* sh1H */);});
      break;
    default:
      return new F(function(){return _q/* GHC.Base.++ */(_1r/* LudoJS.$fFromAnyGameState11 */, _hN/* sh1H */);});
  }
},
_hO/* Out */ = __Z/* EXTERNAL */,
_hP/* lvl15 */ = 4,
_hQ/* a32 */ = new T2(1,_hP/* LudoJS.lvl15 */,_4/* GHC.Types.[] */),
_hR/* a33 */ = new T2(1,_b7/* LudoJS.play10 */,_hQ/* LudoJS.a32 */),
_hS/* lvl20 */ = 2,
_hT/* a34 */ = new T2(1,_hS/* LudoJS.lvl20 */,_hR/* LudoJS.a33 */),
_hU/* lvl33 */ = new T(function(){
  return B(_1o/* Control.Exception.Base.patError */("LudoJS.hs:(112,17)-(114,114)|case"));
}),
_hV/* lvl34 */ = "Active",
_hW/* lvl35 */ = "Out",
_hX/* lvl8 */ = new T2(1,_bp/* LudoJS.Red */,_4/* GHC.Types.[] */),
_hY/* lvl9 */ = new T2(1,_bq/* LudoJS.Yellow */,_hX/* LudoJS.lvl8 */),
_hZ/* $fFromAnyGameState6 */ = function(_i0/* sh1N */, _/* EXTERNAL */){
  var _i1/* sh1P */ = E(_i0/* sh1N */),
  _i2/* sh1X */ = __get/* EXTERNAL */(_i1/* sh1P */, toJSStr/* EXTERNAL */(B(_q/* GHC.Base.++ */(_1u/* LudoJS.$fFromAnyGameState14 */, _4/* GHC.Types.[] */)))),
  _i3/* sh1Z */ = _i2/* sh1X */,
  _i4/* sh21 */ = function(_i5/*  sh5b */, _i6/*  sh5c */, _/* EXTERNAL */){
    while(1){
      var _i7/*  sh21 */ = B((function(_i8/* sh5b */, _i9/* sh5c */, _/* EXTERNAL */){
        var _ia/* sh5e */ = E(_i8/* sh5b */);
        if(!_ia/* sh5e */._){
          return _i9/* sh5c */;
        }else{
          var _ib/* sh5g */ = _ia/* sh5e */.b,
          _ic/* sh5h */ = E(_ia/* sh5e */.a),
          _id/* sh5l */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _ic/* sh5h */, _4/* GHC.Types.[] */))),
          _ie/* sh5p */ = __has/* EXTERNAL */(_i3/* sh1Z */, _id/* sh5l */);
          if(!E(_ie/* sh5p */)){
            var _if/*   sh5c */ = _i9/* sh5c */;
            _i5/*  sh5b */ = _ib/* sh5g */;
            _i6/*  sh5c */ = _if/*   sh5c */;
            return __continue/* EXTERNAL */;
          }else{
            var _ig/* sh5u */ = __get/* EXTERNAL */(_i3/* sh1Z */, _id/* sh5l */),
            _ih/* sh5x */ = E(_9N/* LudoJS.$fToAnyOption5 */),
            _ii/* sh5A */ = __get/* EXTERNAL */(_ig/* sh5u */, _ih/* sh5x */),
            _ij/* sh5E */ = String/* EXTERNAL */(_ii/* sh5A */),
            _ik/* sh5H */ = E(_hW/* LudoJS.lvl35 */),
            _il/* sh5K */ = strEq/* EXTERNAL */(_ij/* sh5E */, _ik/* sh5H */);
            if(!E(_il/* sh5K */)){
              var _im/* sh6V */ = E(_hV/* LudoJS.lvl34 */),
              _in/* sh6Y */ = strEq/* EXTERNAL */(_ij/* sh5E */, _im/* sh6V */);
              if(!E(_in/* sh6Y */)){
                return E(_hU/* LudoJS.lvl33 */);
              }else{
                var _io/* sh72 */ = E(_9M/* LudoJS.$fToAnyOption1 */),
                _ip/* sh75 */ = __get/* EXTERNAL */(_ig/* sh5u */, _io/* sh72 */),
                _iq/* sh78 */ = function(_ir/*  sh79 */, _is/*  sh7a */, _/* EXTERNAL */){
                  while(1){
                    var _it/*  sh78 */ = B((function(_iu/* sh79 */, _iv/* sh7a */, _/* EXTERNAL */){
                      var _iw/* sh7c */ = E(_iu/* sh79 */);
                      if(!_iw/* sh7c */._){
                        return _iv/* sh7a */;
                      }else{
                        var _ix/* sh7e */ = _iw/* sh7c */.b,
                        _iy/* sh7f */ = E(_iw/* sh7c */.a),
                        _iz/* sh7j */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _iy/* sh7f */, _4/* GHC.Types.[] */))),
                        _iA/* sh7n */ = __has/* EXTERNAL */(_i3/* sh1Z */, _iz/* sh7j */);
                        if(!E(_iA/* sh7n */)){
                          var _iB/*   sh7a */ = _iv/* sh7a */;
                          _ir/*  sh79 */ = _ix/* sh7e */;
                          _is/*  sh7a */ = _iB/*   sh7a */;
                          return __continue/* EXTERNAL */;
                        }else{
                          var _iC/* sh7s */ = __get/* EXTERNAL */(_i3/* sh1Z */, _iz/* sh7j */),
                          _iD/* sh7w */ = __get/* EXTERNAL */(_iC/* sh7s */, _ih/* sh5x */),
                          _iE/* sh7A */ = String/* EXTERNAL */(_iD/* sh7w */),
                          _iF/* sh7E */ = strEq/* EXTERNAL */(_iE/* sh7A */, _ik/* sh5H */);
                          if(!E(_iF/* sh7E */)){
                            var _iG/* sh7M */ = strEq/* EXTERNAL */(_iE/* sh7A */, _im/* sh6V */);
                            if(!E(_iG/* sh7M */)){
                              return E(_hU/* LudoJS.lvl33 */);
                            }else{
                              var _iH/* sh7R */ = __get/* EXTERNAL */(_iC/* sh7s */, _io/* sh72 */),
                              _iI/* sh86 */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_iv/* sh7a */, new T2(1,new T2(0,_iy/* sh7f */,new T1(1,new T(function(){
                                  var _iJ/* sh7V */ = Number/* EXTERNAL */(_iH/* sh7R */);
                                  return jsTrunc/* EXTERNAL */(_iJ/* sh7V */);
                                }))),_4/* GHC.Types.[] */)));
                              });
                              _ir/*  sh79 */ = _ix/* sh7e */;
                              _is/*  sh7a */ = _iI/* sh86 */;
                              return __continue/* EXTERNAL */;
                            }
                          }else{
                            _ir/*  sh79 */ = _ix/* sh7e */;
                            _is/*  sh7a */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_iv/* sh7a */, new T2(1,new T2(0,_iy/* sh7f */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                            });
                            return __continue/* EXTERNAL */;
                          }
                        }
                      }
                    })(_ir/*  sh79 */, _is/*  sh7a */, _/* EXTERNAL */));
                    if(_it/*  sh78 */!=__continue/* EXTERNAL */){
                      return _it/*  sh78 */;
                    }
                  }
                },
                _iK/* sh8k */ = new T(function(){
                  return B(_q/* GHC.Base.++ */(_i9/* sh5c */, new T2(1,new T2(0,_ic/* sh5h */,new T1(1,new T(function(){
                    var _iL/* sh89 */ = Number/* EXTERNAL */(_ip/* sh75 */);
                    return jsTrunc/* EXTERNAL */(_iL/* sh89 */);
                  }))),_4/* GHC.Types.[] */)));
                });
                return new F(function(){return _iq/* sh78 */(_ib/* sh5g */, _iK/* sh8k */, _/* EXTERNAL */);});
              }
            }else{
              var _iM/* sh5O */ = function(_iN/*  sh5P */, _iO/*  sh5Q */, _/* EXTERNAL */){
                while(1){
                  var _iP/*  sh5O */ = B((function(_iQ/* sh5P */, _iR/* sh5Q */, _/* EXTERNAL */){
                    var _iS/* sh5S */ = E(_iQ/* sh5P */);
                    if(!_iS/* sh5S */._){
                      return _iR/* sh5Q */;
                    }else{
                      var _iT/* sh5U */ = _iS/* sh5S */.b,
                      _iU/* sh5V */ = E(_iS/* sh5S */.a),
                      _iV/* sh5Z */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _iU/* sh5V */, _4/* GHC.Types.[] */))),
                      _iW/* sh63 */ = __has/* EXTERNAL */(_i3/* sh1Z */, _iV/* sh5Z */);
                      if(!E(_iW/* sh63 */)){
                        var _iX/*   sh5Q */ = _iR/* sh5Q */;
                        _iN/*  sh5P */ = _iT/* sh5U */;
                        _iO/*  sh5Q */ = _iX/*   sh5Q */;
                        return __continue/* EXTERNAL */;
                      }else{
                        var _iY/* sh68 */ = __get/* EXTERNAL */(_i3/* sh1Z */, _iV/* sh5Z */),
                        _iZ/* sh6c */ = __get/* EXTERNAL */(_iY/* sh68 */, _ih/* sh5x */),
                        _j0/* sh6g */ = String/* EXTERNAL */(_iZ/* sh6c */),
                        _j1/* sh6k */ = strEq/* EXTERNAL */(_j0/* sh6g */, _ik/* sh5H */);
                        if(!E(_j1/* sh6k */)){
                          var _j2/* sh6u */ = strEq/* EXTERNAL */(_j0/* sh6g */, E(_hV/* LudoJS.lvl34 */));
                          if(!E(_j2/* sh6u */)){
                            return E(_hU/* LudoJS.lvl33 */);
                          }else{
                            var _j3/* sh6B */ = __get/* EXTERNAL */(_iY/* sh68 */, E(_9M/* LudoJS.$fToAnyOption1 */)),
                            _j4/* sh6Q */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_iR/* sh5Q */, new T2(1,new T2(0,_iU/* sh5V */,new T1(1,new T(function(){
                                var _j5/* sh6F */ = Number/* EXTERNAL */(_j3/* sh6B */);
                                return jsTrunc/* EXTERNAL */(_j5/* sh6F */);
                              }))),_4/* GHC.Types.[] */)));
                            });
                            _iN/*  sh5P */ = _iT/* sh5U */;
                            _iO/*  sh5Q */ = _j4/* sh6Q */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          _iN/*  sh5P */ = _iT/* sh5U */;
                          _iO/*  sh5Q */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_iR/* sh5Q */, new T2(1,new T2(0,_iU/* sh5V */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                          });
                          return __continue/* EXTERNAL */;
                        }
                      }
                    }
                  })(_iN/*  sh5P */, _iO/*  sh5Q */, _/* EXTERNAL */));
                  if(_iP/*  sh5O */!=__continue/* EXTERNAL */){
                    return _iP/*  sh5O */;
                  }
                }
              };
              return new F(function(){return _iM/* sh5O */(_ib/* sh5g */, new T(function(){
                return B(_q/* GHC.Base.++ */(_i9/* sh5c */, new T2(1,new T2(0,_ic/* sh5h */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
              }), _/* EXTERNAL */);});
            }
          }
        }
      })(_i5/*  sh5b */, _i6/*  sh5c */, _/* EXTERNAL */));
      if(_i7/*  sh21 */!=__continue/* EXTERNAL */){
        return _i7/*  sh21 */;
      }
    }
  },
  _j6/* sh20 */ = function(_j7/* sh22 */, _j8/* sh23 */, _j9/* sh24 */, _/* EXTERNAL */){
    var _ja/* sh28 */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _j7/* sh22 */, _4/* GHC.Types.[] */))),
    _jb/* sh2c */ = __has/* EXTERNAL */(_i3/* sh1Z */, _ja/* sh28 */);
    if(!E(_jb/* sh2c */)){
      return new F(function(){return _i4/* sh21 */(_j8/* sh23 */, _j9/* sh24 */, _/* EXTERNAL */);});
    }else{
      var _jc/* sh2h */ = __get/* EXTERNAL */(_i3/* sh1Z */, _ja/* sh28 */),
      _jd/* sh2k */ = E(_9N/* LudoJS.$fToAnyOption5 */),
      _je/* sh2n */ = __get/* EXTERNAL */(_jc/* sh2h */, _jd/* sh2k */),
      _jf/* sh2r */ = String/* EXTERNAL */(_je/* sh2n */),
      _jg/* sh2u */ = E(_hW/* LudoJS.lvl35 */),
      _jh/* sh2x */ = strEq/* EXTERNAL */(_jf/* sh2r */, _jg/* sh2u */);
      if(!E(_jh/* sh2x */)){
        var _ji/* sh3J */ = E(_hV/* LudoJS.lvl34 */),
        _jj/* sh3M */ = strEq/* EXTERNAL */(_jf/* sh2r */, _ji/* sh3J */);
        if(!E(_jj/* sh3M */)){
          return E(_hU/* LudoJS.lvl33 */);
        }else{
          var _jk/* sh3Q */ = E(_9M/* LudoJS.$fToAnyOption1 */),
          _jl/* sh3T */ = __get/* EXTERNAL */(_jc/* sh2h */, _jk/* sh3Q */),
          _jm/* sh3W */ = function(_jn/*  sh3X */, _jo/*  sh3Y */, _/* EXTERNAL */){
            while(1){
              var _jp/*  sh3W */ = B((function(_jq/* sh3X */, _jr/* sh3Y */, _/* EXTERNAL */){
                var _js/* sh40 */ = E(_jq/* sh3X */);
                if(!_js/* sh40 */._){
                  return _jr/* sh3Y */;
                }else{
                  var _jt/* sh42 */ = _js/* sh40 */.b,
                  _ju/* sh43 */ = E(_js/* sh40 */.a),
                  _jv/* sh47 */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _ju/* sh43 */, _4/* GHC.Types.[] */))),
                  _jw/* sh4b */ = __has/* EXTERNAL */(_i3/* sh1Z */, _jv/* sh47 */);
                  if(!E(_jw/* sh4b */)){
                    var _jx/*   sh3Y */ = _jr/* sh3Y */;
                    _jn/*  sh3X */ = _jt/* sh42 */;
                    _jo/*  sh3Y */ = _jx/*   sh3Y */;
                    return __continue/* EXTERNAL */;
                  }else{
                    var _jy/* sh4g */ = __get/* EXTERNAL */(_i3/* sh1Z */, _jv/* sh47 */),
                    _jz/* sh4k */ = __get/* EXTERNAL */(_jy/* sh4g */, _jd/* sh2k */),
                    _jA/* sh4o */ = String/* EXTERNAL */(_jz/* sh4k */),
                    _jB/* sh4s */ = strEq/* EXTERNAL */(_jA/* sh4o */, _jg/* sh2u */);
                    if(!E(_jB/* sh4s */)){
                      var _jC/* sh4A */ = strEq/* EXTERNAL */(_jA/* sh4o */, _ji/* sh3J */);
                      if(!E(_jC/* sh4A */)){
                        return E(_hU/* LudoJS.lvl33 */);
                      }else{
                        var _jD/* sh4F */ = __get/* EXTERNAL */(_jy/* sh4g */, _jk/* sh3Q */),
                        _jE/* sh4U */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_jr/* sh3Y */, new T2(1,new T2(0,_ju/* sh43 */,new T1(1,new T(function(){
                            var _jF/* sh4J */ = Number/* EXTERNAL */(_jD/* sh4F */);
                            return jsTrunc/* EXTERNAL */(_jF/* sh4J */);
                          }))),_4/* GHC.Types.[] */)));
                        });
                        _jn/*  sh3X */ = _jt/* sh42 */;
                        _jo/*  sh3Y */ = _jE/* sh4U */;
                        return __continue/* EXTERNAL */;
                      }
                    }else{
                      _jn/*  sh3X */ = _jt/* sh42 */;
                      _jo/*  sh3Y */ = new T(function(){
                        return B(_q/* GHC.Base.++ */(_jr/* sh3Y */, new T2(1,new T2(0,_ju/* sh43 */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                      });
                      return __continue/* EXTERNAL */;
                    }
                  }
                }
              })(_jn/*  sh3X */, _jo/*  sh3Y */, _/* EXTERNAL */));
              if(_jp/*  sh3W */!=__continue/* EXTERNAL */){
                return _jp/*  sh3W */;
              }
            }
          },
          _jG/* sh59 */ = new T(function(){
            return B(_q/* GHC.Base.++ */(_j9/* sh24 */, new T2(1,new T2(0,_j7/* sh22 */,new T1(1,new T(function(){
              var _jH/* sh4Y */ = Number/* EXTERNAL */(_jl/* sh3T */);
              return jsTrunc/* EXTERNAL */(_jH/* sh4Y */);
            }))),_4/* GHC.Types.[] */)));
          });
          return new F(function(){return _jm/* sh3W */(_j8/* sh23 */, _jG/* sh59 */, _/* EXTERNAL */);});
        }
      }else{
        var _jI/* sh2B */ = function(_jJ/*  sh2C */, _jK/*  sh2D */, _/* EXTERNAL */){
          while(1){
            var _jL/*  sh2B */ = B((function(_jM/* sh2C */, _jN/* sh2D */, _/* EXTERNAL */){
              var _jO/* sh2F */ = E(_jM/* sh2C */);
              if(!_jO/* sh2F */._){
                return _jN/* sh2D */;
              }else{
                var _jP/* sh2H */ = _jO/* sh2F */.b,
                _jQ/* sh2I */ = E(_jO/* sh2F */.a),
                _jR/* sh2M */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _jQ/* sh2I */, _4/* GHC.Types.[] */))),
                _jS/* sh2Q */ = __has/* EXTERNAL */(_i3/* sh1Z */, _jR/* sh2M */);
                if(!E(_jS/* sh2Q */)){
                  var _jT/*   sh2D */ = _jN/* sh2D */;
                  _jJ/*  sh2C */ = _jP/* sh2H */;
                  _jK/*  sh2D */ = _jT/*   sh2D */;
                  return __continue/* EXTERNAL */;
                }else{
                  var _jU/* sh2V */ = __get/* EXTERNAL */(_i3/* sh1Z */, _jR/* sh2M */),
                  _jV/* sh2Z */ = __get/* EXTERNAL */(_jU/* sh2V */, _jd/* sh2k */),
                  _jW/* sh33 */ = String/* EXTERNAL */(_jV/* sh2Z */),
                  _jX/* sh37 */ = strEq/* EXTERNAL */(_jW/* sh33 */, _jg/* sh2u */);
                  if(!E(_jX/* sh37 */)){
                    var _jY/* sh3h */ = strEq/* EXTERNAL */(_jW/* sh33 */, E(_hV/* LudoJS.lvl34 */));
                    if(!E(_jY/* sh3h */)){
                      return E(_hU/* LudoJS.lvl33 */);
                    }else{
                      var _jZ/* sh3o */ = __get/* EXTERNAL */(_jU/* sh2V */, E(_9M/* LudoJS.$fToAnyOption1 */)),
                      _k0/* sh3D */ = new T(function(){
                        return B(_q/* GHC.Base.++ */(_jN/* sh2D */, new T2(1,new T2(0,_jQ/* sh2I */,new T1(1,new T(function(){
                          var _k1/* sh3s */ = Number/* EXTERNAL */(_jZ/* sh3o */);
                          return jsTrunc/* EXTERNAL */(_k1/* sh3s */);
                        }))),_4/* GHC.Types.[] */)));
                      });
                      _jJ/*  sh2C */ = _jP/* sh2H */;
                      _jK/*  sh2D */ = _k0/* sh3D */;
                      return __continue/* EXTERNAL */;
                    }
                  }else{
                    _jJ/*  sh2C */ = _jP/* sh2H */;
                    _jK/*  sh2D */ = new T(function(){
                      return B(_q/* GHC.Base.++ */(_jN/* sh2D */, new T2(1,new T2(0,_jQ/* sh2I */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                    });
                    return __continue/* EXTERNAL */;
                  }
                }
              }
            })(_jJ/*  sh2C */, _jK/*  sh2D */, _/* EXTERNAL */));
            if(_jL/*  sh2B */!=__continue/* EXTERNAL */){
              return _jL/*  sh2B */;
            }
          }
        };
        return new F(function(){return _jI/* sh2B */(_j8/* sh23 */, new T(function(){
          return B(_q/* GHC.Base.++ */(_j9/* sh24 */, new T2(1,new T2(0,_j7/* sh22 */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
        }), _/* EXTERNAL */);});
      }
    }
  },
  _k2/* sh8m */ = B(_j6/* sh20 */(1, _hT/* LudoJS.a34 */, _4/* GHC.Types.[] */, _/* EXTERNAL */)),
  _k3/* sh8q */ = function(_k4/* shf7 */, _/* EXTERNAL */){
    var _k5/* shf9 */ = E(_k4/* shf7 */);
    if(!_k5/* shf9 */._){
      return _4/* GHC.Types.[] */;
    }else{
      var _k6/* shfa */ = _k5/* shf9 */.a,
      _k7/* shfi */ = __get/* EXTERNAL */(_i1/* sh1P */, toJSStr/* EXTERNAL */(B(_hL/* LudoJS.$w$cshowsPrec */(_k6/* shfa */, _4/* GHC.Types.[] */)))),
      _k8/* shfk */ = _k7/* shfi */,
      _k9/* shfm */ = function(_ka/*  shiw */, _kb/*  shix */, _/* EXTERNAL */){
        while(1){
          var _kc/*  shfm */ = B((function(_kd/* shiw */, _ke/* shix */, _/* EXTERNAL */){
            var _kf/* shiz */ = E(_kd/* shiw */);
            if(!_kf/* shiz */._){
              return _ke/* shix */;
            }else{
              var _kg/* shiB */ = _kf/* shiz */.b,
              _kh/* shiC */ = E(_kf/* shiz */.a),
              _ki/* shiG */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _kh/* shiC */, _4/* GHC.Types.[] */))),
              _kj/* shiK */ = __has/* EXTERNAL */(_k8/* shfk */, _ki/* shiG */);
              if(!E(_kj/* shiK */)){
                var _kk/*   shix */ = _ke/* shix */;
                _ka/*  shiw */ = _kg/* shiB */;
                _kb/*  shix */ = _kk/*   shix */;
                return __continue/* EXTERNAL */;
              }else{
                var _kl/* shiP */ = __get/* EXTERNAL */(_k8/* shfk */, _ki/* shiG */),
                _km/* shiS */ = E(_9N/* LudoJS.$fToAnyOption5 */),
                _kn/* shiV */ = __get/* EXTERNAL */(_kl/* shiP */, _km/* shiS */),
                _ko/* shiZ */ = String/* EXTERNAL */(_kn/* shiV */),
                _kp/* shj2 */ = E(_hW/* LudoJS.lvl35 */),
                _kq/* shj5 */ = strEq/* EXTERNAL */(_ko/* shiZ */, _kp/* shj2 */);
                if(!E(_kq/* shj5 */)){
                  var _kr/* shkg */ = E(_hV/* LudoJS.lvl34 */),
                  _ks/* shkj */ = strEq/* EXTERNAL */(_ko/* shiZ */, _kr/* shkg */);
                  if(!E(_ks/* shkj */)){
                    return E(_hU/* LudoJS.lvl33 */);
                  }else{
                    var _kt/* shkn */ = E(_9M/* LudoJS.$fToAnyOption1 */),
                    _ku/* shkq */ = __get/* EXTERNAL */(_kl/* shiP */, _kt/* shkn */),
                    _kv/* shkt */ = function(_kw/*  shku */, _kx/*  shkv */, _/* EXTERNAL */){
                      while(1){
                        var _ky/*  shkt */ = B((function(_kz/* shku */, _kA/* shkv */, _/* EXTERNAL */){
                          var _kB/* shkx */ = E(_kz/* shku */);
                          if(!_kB/* shkx */._){
                            return _kA/* shkv */;
                          }else{
                            var _kC/* shkz */ = _kB/* shkx */.b,
                            _kD/* shkA */ = E(_kB/* shkx */.a),
                            _kE/* shkE */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _kD/* shkA */, _4/* GHC.Types.[] */))),
                            _kF/* shkI */ = __has/* EXTERNAL */(_k8/* shfk */, _kE/* shkE */);
                            if(!E(_kF/* shkI */)){
                              var _kG/*   shkv */ = _kA/* shkv */;
                              _kw/*  shku */ = _kC/* shkz */;
                              _kx/*  shkv */ = _kG/*   shkv */;
                              return __continue/* EXTERNAL */;
                            }else{
                              var _kH/* shkN */ = __get/* EXTERNAL */(_k8/* shfk */, _kE/* shkE */),
                              _kI/* shkR */ = __get/* EXTERNAL */(_kH/* shkN */, _km/* shiS */),
                              _kJ/* shkV */ = String/* EXTERNAL */(_kI/* shkR */),
                              _kK/* shkZ */ = strEq/* EXTERNAL */(_kJ/* shkV */, _kp/* shj2 */);
                              if(!E(_kK/* shkZ */)){
                                var _kL/* shl7 */ = strEq/* EXTERNAL */(_kJ/* shkV */, _kr/* shkg */);
                                if(!E(_kL/* shl7 */)){
                                  return E(_hU/* LudoJS.lvl33 */);
                                }else{
                                  var _kM/* shlc */ = __get/* EXTERNAL */(_kH/* shkN */, _kt/* shkn */),
                                  _kN/* shlr */ = new T(function(){
                                    return B(_q/* GHC.Base.++ */(_kA/* shkv */, new T2(1,new T2(0,_kD/* shkA */,new T1(1,new T(function(){
                                      var _kO/* shlg */ = Number/* EXTERNAL */(_kM/* shlc */);
                                      return jsTrunc/* EXTERNAL */(_kO/* shlg */);
                                    }))),_4/* GHC.Types.[] */)));
                                  });
                                  _kw/*  shku */ = _kC/* shkz */;
                                  _kx/*  shkv */ = _kN/* shlr */;
                                  return __continue/* EXTERNAL */;
                                }
                              }else{
                                _kw/*  shku */ = _kC/* shkz */;
                                _kx/*  shkv */ = new T(function(){
                                  return B(_q/* GHC.Base.++ */(_kA/* shkv */, new T2(1,new T2(0,_kD/* shkA */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                                });
                                return __continue/* EXTERNAL */;
                              }
                            }
                          }
                        })(_kw/*  shku */, _kx/*  shkv */, _/* EXTERNAL */));
                        if(_ky/*  shkt */!=__continue/* EXTERNAL */){
                          return _ky/*  shkt */;
                        }
                      }
                    },
                    _kP/* shlF */ = new T(function(){
                      return B(_q/* GHC.Base.++ */(_ke/* shix */, new T2(1,new T2(0,_kh/* shiC */,new T1(1,new T(function(){
                        var _kQ/* shlu */ = Number/* EXTERNAL */(_ku/* shkq */);
                        return jsTrunc/* EXTERNAL */(_kQ/* shlu */);
                      }))),_4/* GHC.Types.[] */)));
                    });
                    return new F(function(){return _kv/* shkt */(_kg/* shiB */, _kP/* shlF */, _/* EXTERNAL */);});
                  }
                }else{
                  var _kR/* shj9 */ = function(_kS/*  shja */, _kT/*  shjb */, _/* EXTERNAL */){
                    while(1){
                      var _kU/*  shj9 */ = B((function(_kV/* shja */, _kW/* shjb */, _/* EXTERNAL */){
                        var _kX/* shjd */ = E(_kV/* shja */);
                        if(!_kX/* shjd */._){
                          return _kW/* shjb */;
                        }else{
                          var _kY/* shjf */ = _kX/* shjd */.b,
                          _kZ/* shjg */ = E(_kX/* shjd */.a),
                          _l0/* shjk */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _kZ/* shjg */, _4/* GHC.Types.[] */))),
                          _l1/* shjo */ = __has/* EXTERNAL */(_k8/* shfk */, _l0/* shjk */);
                          if(!E(_l1/* shjo */)){
                            var _l2/*   shjb */ = _kW/* shjb */;
                            _kS/*  shja */ = _kY/* shjf */;
                            _kT/*  shjb */ = _l2/*   shjb */;
                            return __continue/* EXTERNAL */;
                          }else{
                            var _l3/* shjt */ = __get/* EXTERNAL */(_k8/* shfk */, _l0/* shjk */),
                            _l4/* shjx */ = __get/* EXTERNAL */(_l3/* shjt */, _km/* shiS */),
                            _l5/* shjB */ = String/* EXTERNAL */(_l4/* shjx */),
                            _l6/* shjF */ = strEq/* EXTERNAL */(_l5/* shjB */, _kp/* shj2 */);
                            if(!E(_l6/* shjF */)){
                              var _l7/* shjP */ = strEq/* EXTERNAL */(_l5/* shjB */, E(_hV/* LudoJS.lvl34 */));
                              if(!E(_l7/* shjP */)){
                                return E(_hU/* LudoJS.lvl33 */);
                              }else{
                                var _l8/* shjW */ = __get/* EXTERNAL */(_l3/* shjt */, E(_9M/* LudoJS.$fToAnyOption1 */)),
                                _l9/* shkb */ = new T(function(){
                                  return B(_q/* GHC.Base.++ */(_kW/* shjb */, new T2(1,new T2(0,_kZ/* shjg */,new T1(1,new T(function(){
                                    var _la/* shk0 */ = Number/* EXTERNAL */(_l8/* shjW */);
                                    return jsTrunc/* EXTERNAL */(_la/* shk0 */);
                                  }))),_4/* GHC.Types.[] */)));
                                });
                                _kS/*  shja */ = _kY/* shjf */;
                                _kT/*  shjb */ = _l9/* shkb */;
                                return __continue/* EXTERNAL */;
                              }
                            }else{
                              _kS/*  shja */ = _kY/* shjf */;
                              _kT/*  shjb */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_kW/* shjb */, new T2(1,new T2(0,_kZ/* shjg */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                              });
                              return __continue/* EXTERNAL */;
                            }
                          }
                        }
                      })(_kS/*  shja */, _kT/*  shjb */, _/* EXTERNAL */));
                      if(_kU/*  shj9 */!=__continue/* EXTERNAL */){
                        return _kU/*  shj9 */;
                      }
                    }
                  };
                  return new F(function(){return _kR/* shj9 */(_kg/* shiB */, new T(function(){
                    return B(_q/* GHC.Base.++ */(_ke/* shix */, new T2(1,new T2(0,_kh/* shiC */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                  }), _/* EXTERNAL */);});
                }
              }
            }
          })(_ka/*  shiw */, _kb/*  shix */, _/* EXTERNAL */));
          if(_kc/*  shfm */!=__continue/* EXTERNAL */){
            return _kc/*  shfm */;
          }
        }
      },
      _lb/* shfl */ = function(_lc/* shfn */, _ld/* shfo */, _le/* shfp */, _/* EXTERNAL */){
        var _lf/* shft */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _lc/* shfn */, _4/* GHC.Types.[] */))),
        _lg/* shfx */ = __has/* EXTERNAL */(_k8/* shfk */, _lf/* shft */);
        if(!E(_lg/* shfx */)){
          return new F(function(){return _k9/* shfm */(_ld/* shfo */, _le/* shfp */, _/* EXTERNAL */);});
        }else{
          var _lh/* shfC */ = __get/* EXTERNAL */(_k8/* shfk */, _lf/* shft */),
          _li/* shfF */ = E(_9N/* LudoJS.$fToAnyOption5 */),
          _lj/* shfI */ = __get/* EXTERNAL */(_lh/* shfC */, _li/* shfF */),
          _lk/* shfM */ = String/* EXTERNAL */(_lj/* shfI */),
          _ll/* shfP */ = E(_hW/* LudoJS.lvl35 */),
          _lm/* shfS */ = strEq/* EXTERNAL */(_lk/* shfM */, _ll/* shfP */);
          if(!E(_lm/* shfS */)){
            var _ln/* shh4 */ = E(_hV/* LudoJS.lvl34 */),
            _lo/* shh7 */ = strEq/* EXTERNAL */(_lk/* shfM */, _ln/* shh4 */);
            if(!E(_lo/* shh7 */)){
              return E(_hU/* LudoJS.lvl33 */);
            }else{
              var _lp/* shhb */ = E(_9M/* LudoJS.$fToAnyOption1 */),
              _lq/* shhe */ = __get/* EXTERNAL */(_lh/* shfC */, _lp/* shhb */),
              _lr/* shhh */ = function(_ls/*  shhi */, _lt/*  shhj */, _/* EXTERNAL */){
                while(1){
                  var _lu/*  shhh */ = B((function(_lv/* shhi */, _lw/* shhj */, _/* EXTERNAL */){
                    var _lx/* shhl */ = E(_lv/* shhi */);
                    if(!_lx/* shhl */._){
                      return _lw/* shhj */;
                    }else{
                      var _ly/* shhn */ = _lx/* shhl */.b,
                      _lz/* shho */ = E(_lx/* shhl */.a),
                      _lA/* shhs */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _lz/* shho */, _4/* GHC.Types.[] */))),
                      _lB/* shhw */ = __has/* EXTERNAL */(_k8/* shfk */, _lA/* shhs */);
                      if(!E(_lB/* shhw */)){
                        var _lC/*   shhj */ = _lw/* shhj */;
                        _ls/*  shhi */ = _ly/* shhn */;
                        _lt/*  shhj */ = _lC/*   shhj */;
                        return __continue/* EXTERNAL */;
                      }else{
                        var _lD/* shhB */ = __get/* EXTERNAL */(_k8/* shfk */, _lA/* shhs */),
                        _lE/* shhF */ = __get/* EXTERNAL */(_lD/* shhB */, _li/* shfF */),
                        _lF/* shhJ */ = String/* EXTERNAL */(_lE/* shhF */),
                        _lG/* shhN */ = strEq/* EXTERNAL */(_lF/* shhJ */, _ll/* shfP */);
                        if(!E(_lG/* shhN */)){
                          var _lH/* shhV */ = strEq/* EXTERNAL */(_lF/* shhJ */, _ln/* shh4 */);
                          if(!E(_lH/* shhV */)){
                            return E(_hU/* LudoJS.lvl33 */);
                          }else{
                            var _lI/* shi0 */ = __get/* EXTERNAL */(_lD/* shhB */, _lp/* shhb */),
                            _lJ/* shif */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_lw/* shhj */, new T2(1,new T2(0,_lz/* shho */,new T1(1,new T(function(){
                                var _lK/* shi4 */ = Number/* EXTERNAL */(_lI/* shi0 */);
                                return jsTrunc/* EXTERNAL */(_lK/* shi4 */);
                              }))),_4/* GHC.Types.[] */)));
                            });
                            _ls/*  shhi */ = _ly/* shhn */;
                            _lt/*  shhj */ = _lJ/* shif */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          _ls/*  shhi */ = _ly/* shhn */;
                          _lt/*  shhj */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_lw/* shhj */, new T2(1,new T2(0,_lz/* shho */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                          });
                          return __continue/* EXTERNAL */;
                        }
                      }
                    }
                  })(_ls/*  shhi */, _lt/*  shhj */, _/* EXTERNAL */));
                  if(_lu/*  shhh */!=__continue/* EXTERNAL */){
                    return _lu/*  shhh */;
                  }
                }
              },
              _lL/* shiu */ = new T(function(){
                return B(_q/* GHC.Base.++ */(_le/* shfp */, new T2(1,new T2(0,_lc/* shfn */,new T1(1,new T(function(){
                  var _lM/* shij */ = Number/* EXTERNAL */(_lq/* shhe */);
                  return jsTrunc/* EXTERNAL */(_lM/* shij */);
                }))),_4/* GHC.Types.[] */)));
              });
              return new F(function(){return _lr/* shhh */(_ld/* shfo */, _lL/* shiu */, _/* EXTERNAL */);});
            }
          }else{
            var _lN/* shfW */ = function(_lO/*  shfX */, _lP/*  shfY */, _/* EXTERNAL */){
              while(1){
                var _lQ/*  shfW */ = B((function(_lR/* shfX */, _lS/* shfY */, _/* EXTERNAL */){
                  var _lT/* shg0 */ = E(_lR/* shfX */);
                  if(!_lT/* shg0 */._){
                    return _lS/* shfY */;
                  }else{
                    var _lU/* shg2 */ = _lT/* shg0 */.b,
                    _lV/* shg3 */ = E(_lT/* shg0 */.a),
                    _lW/* shg7 */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _lV/* shg3 */, _4/* GHC.Types.[] */))),
                    _lX/* shgb */ = __has/* EXTERNAL */(_k8/* shfk */, _lW/* shg7 */);
                    if(!E(_lX/* shgb */)){
                      var _lY/*   shfY */ = _lS/* shfY */;
                      _lO/*  shfX */ = _lU/* shg2 */;
                      _lP/*  shfY */ = _lY/*   shfY */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _lZ/* shgg */ = __get/* EXTERNAL */(_k8/* shfk */, _lW/* shg7 */),
                      _m0/* shgk */ = __get/* EXTERNAL */(_lZ/* shgg */, _li/* shfF */),
                      _m1/* shgo */ = String/* EXTERNAL */(_m0/* shgk */),
                      _m2/* shgs */ = strEq/* EXTERNAL */(_m1/* shgo */, _ll/* shfP */);
                      if(!E(_m2/* shgs */)){
                        var _m3/* shgC */ = strEq/* EXTERNAL */(_m1/* shgo */, E(_hV/* LudoJS.lvl34 */));
                        if(!E(_m3/* shgC */)){
                          return E(_hU/* LudoJS.lvl33 */);
                        }else{
                          var _m4/* shgJ */ = __get/* EXTERNAL */(_lZ/* shgg */, E(_9M/* LudoJS.$fToAnyOption1 */)),
                          _m5/* shgY */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_lS/* shfY */, new T2(1,new T2(0,_lV/* shg3 */,new T1(1,new T(function(){
                              var _m6/* shgN */ = Number/* EXTERNAL */(_m4/* shgJ */);
                              return jsTrunc/* EXTERNAL */(_m6/* shgN */);
                            }))),_4/* GHC.Types.[] */)));
                          });
                          _lO/*  shfX */ = _lU/* shg2 */;
                          _lP/*  shfY */ = _m5/* shgY */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        _lO/*  shfX */ = _lU/* shg2 */;
                        _lP/*  shfY */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_lS/* shfY */, new T2(1,new T2(0,_lV/* shg3 */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                        });
                        return __continue/* EXTERNAL */;
                      }
                    }
                  }
                })(_lO/*  shfX */, _lP/*  shfY */, _/* EXTERNAL */));
                if(_lQ/*  shfW */!=__continue/* EXTERNAL */){
                  return _lQ/*  shfW */;
                }
              }
            };
            return new F(function(){return _lN/* shfW */(_ld/* shfo */, new T(function(){
              return B(_q/* GHC.Base.++ */(_le/* shfp */, new T2(1,new T2(0,_lc/* shfn */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
            }), _/* EXTERNAL */);});
          }
        }
      },
      _m7/* shlH */ = B(_lb/* shfl */(1, _hT/* LudoJS.a34 */, _4/* GHC.Types.[] */, _/* EXTERNAL */)),
      _m8/* shlK */ = B(_k3/* sh8q */(_k5/* shf9 */.b, _/* EXTERNAL */));
      return new T2(1,new T2(0,_k6/* shfa */,_m7/* shlH */),_m8/* shlK */);
    }
  },
  _m9/* sh8p */ = function(_ma/* sh8r */, _mb/* sh8s */, _/* EXTERNAL */){
    var _mc/* sh8A */ = __get/* EXTERNAL */(_i1/* sh1P */, toJSStr/* EXTERNAL */(B(_hL/* LudoJS.$w$cshowsPrec */(_ma/* sh8r */, _4/* GHC.Types.[] */)))),
    _md/* sh8C */ = _mc/* sh8A */,
    _me/* sh8E */ = function(_mf/*  shbO */, _mg/*  shbP */, _/* EXTERNAL */){
      while(1){
        var _mh/*  sh8E */ = B((function(_mi/* shbO */, _mj/* shbP */, _/* EXTERNAL */){
          var _mk/* shbR */ = E(_mi/* shbO */);
          if(!_mk/* shbR */._){
            return _mj/* shbP */;
          }else{
            var _ml/* shbT */ = _mk/* shbR */.b,
            _mm/* shbU */ = E(_mk/* shbR */.a),
            _mn/* shbY */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _mm/* shbU */, _4/* GHC.Types.[] */))),
            _mo/* shc2 */ = __has/* EXTERNAL */(_md/* sh8C */, _mn/* shbY */);
            if(!E(_mo/* shc2 */)){
              var _mp/*   shbP */ = _mj/* shbP */;
              _mf/*  shbO */ = _ml/* shbT */;
              _mg/*  shbP */ = _mp/*   shbP */;
              return __continue/* EXTERNAL */;
            }else{
              var _mq/* shc7 */ = __get/* EXTERNAL */(_md/* sh8C */, _mn/* shbY */),
              _mr/* shca */ = E(_9N/* LudoJS.$fToAnyOption5 */),
              _ms/* shcd */ = __get/* EXTERNAL */(_mq/* shc7 */, _mr/* shca */),
              _mt/* shch */ = String/* EXTERNAL */(_ms/* shcd */),
              _mu/* shck */ = E(_hW/* LudoJS.lvl35 */),
              _mv/* shcn */ = strEq/* EXTERNAL */(_mt/* shch */, _mu/* shck */);
              if(!E(_mv/* shcn */)){
                var _mw/* shdy */ = E(_hV/* LudoJS.lvl34 */),
                _mx/* shdB */ = strEq/* EXTERNAL */(_mt/* shch */, _mw/* shdy */);
                if(!E(_mx/* shdB */)){
                  return E(_hU/* LudoJS.lvl33 */);
                }else{
                  var _my/* shdF */ = E(_9M/* LudoJS.$fToAnyOption1 */),
                  _mz/* shdI */ = __get/* EXTERNAL */(_mq/* shc7 */, _my/* shdF */),
                  _mA/* shdL */ = function(_mB/*  shdM */, _mC/*  shdN */, _/* EXTERNAL */){
                    while(1){
                      var _mD/*  shdL */ = B((function(_mE/* shdM */, _mF/* shdN */, _/* EXTERNAL */){
                        var _mG/* shdP */ = E(_mE/* shdM */);
                        if(!_mG/* shdP */._){
                          return _mF/* shdN */;
                        }else{
                          var _mH/* shdR */ = _mG/* shdP */.b,
                          _mI/* shdS */ = E(_mG/* shdP */.a),
                          _mJ/* shdW */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _mI/* shdS */, _4/* GHC.Types.[] */))),
                          _mK/* she0 */ = __has/* EXTERNAL */(_md/* sh8C */, _mJ/* shdW */);
                          if(!E(_mK/* she0 */)){
                            var _mL/*   shdN */ = _mF/* shdN */;
                            _mB/*  shdM */ = _mH/* shdR */;
                            _mC/*  shdN */ = _mL/*   shdN */;
                            return __continue/* EXTERNAL */;
                          }else{
                            var _mM/* she5 */ = __get/* EXTERNAL */(_md/* sh8C */, _mJ/* shdW */),
                            _mN/* she9 */ = __get/* EXTERNAL */(_mM/* she5 */, _mr/* shca */),
                            _mO/* shed */ = String/* EXTERNAL */(_mN/* she9 */),
                            _mP/* sheh */ = strEq/* EXTERNAL */(_mO/* shed */, _mu/* shck */);
                            if(!E(_mP/* sheh */)){
                              var _mQ/* shep */ = strEq/* EXTERNAL */(_mO/* shed */, _mw/* shdy */);
                              if(!E(_mQ/* shep */)){
                                return E(_hU/* LudoJS.lvl33 */);
                              }else{
                                var _mR/* sheu */ = __get/* EXTERNAL */(_mM/* she5 */, _my/* shdF */),
                                _mS/* sheJ */ = new T(function(){
                                  return B(_q/* GHC.Base.++ */(_mF/* shdN */, new T2(1,new T2(0,_mI/* shdS */,new T1(1,new T(function(){
                                    var _mT/* shey */ = Number/* EXTERNAL */(_mR/* sheu */);
                                    return jsTrunc/* EXTERNAL */(_mT/* shey */);
                                  }))),_4/* GHC.Types.[] */)));
                                });
                                _mB/*  shdM */ = _mH/* shdR */;
                                _mC/*  shdN */ = _mS/* sheJ */;
                                return __continue/* EXTERNAL */;
                              }
                            }else{
                              _mB/*  shdM */ = _mH/* shdR */;
                              _mC/*  shdN */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_mF/* shdN */, new T2(1,new T2(0,_mI/* shdS */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                              });
                              return __continue/* EXTERNAL */;
                            }
                          }
                        }
                      })(_mB/*  shdM */, _mC/*  shdN */, _/* EXTERNAL */));
                      if(_mD/*  shdL */!=__continue/* EXTERNAL */){
                        return _mD/*  shdL */;
                      }
                    }
                  },
                  _mU/* sheX */ = new T(function(){
                    return B(_q/* GHC.Base.++ */(_mj/* shbP */, new T2(1,new T2(0,_mm/* shbU */,new T1(1,new T(function(){
                      var _mV/* sheM */ = Number/* EXTERNAL */(_mz/* shdI */);
                      return jsTrunc/* EXTERNAL */(_mV/* sheM */);
                    }))),_4/* GHC.Types.[] */)));
                  });
                  return new F(function(){return _mA/* shdL */(_ml/* shbT */, _mU/* sheX */, _/* EXTERNAL */);});
                }
              }else{
                var _mW/* shcr */ = function(_mX/*  shcs */, _mY/*  shct */, _/* EXTERNAL */){
                  while(1){
                    var _mZ/*  shcr */ = B((function(_n0/* shcs */, _n1/* shct */, _/* EXTERNAL */){
                      var _n2/* shcv */ = E(_n0/* shcs */);
                      if(!_n2/* shcv */._){
                        return _n1/* shct */;
                      }else{
                        var _n3/* shcx */ = _n2/* shcv */.b,
                        _n4/* shcy */ = E(_n2/* shcv */.a),
                        _n5/* shcC */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _n4/* shcy */, _4/* GHC.Types.[] */))),
                        _n6/* shcG */ = __has/* EXTERNAL */(_md/* sh8C */, _n5/* shcC */);
                        if(!E(_n6/* shcG */)){
                          var _n7/*   shct */ = _n1/* shct */;
                          _mX/*  shcs */ = _n3/* shcx */;
                          _mY/*  shct */ = _n7/*   shct */;
                          return __continue/* EXTERNAL */;
                        }else{
                          var _n8/* shcL */ = __get/* EXTERNAL */(_md/* sh8C */, _n5/* shcC */),
                          _n9/* shcP */ = __get/* EXTERNAL */(_n8/* shcL */, _mr/* shca */),
                          _na/* shcT */ = String/* EXTERNAL */(_n9/* shcP */),
                          _nb/* shcX */ = strEq/* EXTERNAL */(_na/* shcT */, _mu/* shck */);
                          if(!E(_nb/* shcX */)){
                            var _nc/* shd7 */ = strEq/* EXTERNAL */(_na/* shcT */, E(_hV/* LudoJS.lvl34 */));
                            if(!E(_nc/* shd7 */)){
                              return E(_hU/* LudoJS.lvl33 */);
                            }else{
                              var _nd/* shde */ = __get/* EXTERNAL */(_n8/* shcL */, E(_9M/* LudoJS.$fToAnyOption1 */)),
                              _ne/* shdt */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_n1/* shct */, new T2(1,new T2(0,_n4/* shcy */,new T1(1,new T(function(){
                                  var _nf/* shdi */ = Number/* EXTERNAL */(_nd/* shde */);
                                  return jsTrunc/* EXTERNAL */(_nf/* shdi */);
                                }))),_4/* GHC.Types.[] */)));
                              });
                              _mX/*  shcs */ = _n3/* shcx */;
                              _mY/*  shct */ = _ne/* shdt */;
                              return __continue/* EXTERNAL */;
                            }
                          }else{
                            _mX/*  shcs */ = _n3/* shcx */;
                            _mY/*  shct */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_n1/* shct */, new T2(1,new T2(0,_n4/* shcy */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                            });
                            return __continue/* EXTERNAL */;
                          }
                        }
                      }
                    })(_mX/*  shcs */, _mY/*  shct */, _/* EXTERNAL */));
                    if(_mZ/*  shcr */!=__continue/* EXTERNAL */){
                      return _mZ/*  shcr */;
                    }
                  }
                };
                return new F(function(){return _mW/* shcr */(_ml/* shbT */, new T(function(){
                  return B(_q/* GHC.Base.++ */(_mj/* shbP */, new T2(1,new T2(0,_mm/* shbU */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                }), _/* EXTERNAL */);});
              }
            }
          }
        })(_mf/*  shbO */, _mg/*  shbP */, _/* EXTERNAL */));
        if(_mh/*  sh8E */!=__continue/* EXTERNAL */){
          return _mh/*  sh8E */;
        }
      }
    },
    _ng/* sh8D */ = function(_nh/* sh8F */, _ni/* sh8G */, _nj/* sh8H */, _/* EXTERNAL */){
      var _nk/* sh8L */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _nh/* sh8F */, _4/* GHC.Types.[] */))),
      _nl/* sh8P */ = __has/* EXTERNAL */(_md/* sh8C */, _nk/* sh8L */);
      if(!E(_nl/* sh8P */)){
        return new F(function(){return _me/* sh8E */(_ni/* sh8G */, _nj/* sh8H */, _/* EXTERNAL */);});
      }else{
        var _nm/* sh8U */ = __get/* EXTERNAL */(_md/* sh8C */, _nk/* sh8L */),
        _nn/* sh8X */ = E(_9N/* LudoJS.$fToAnyOption5 */),
        _no/* sh90 */ = __get/* EXTERNAL */(_nm/* sh8U */, _nn/* sh8X */),
        _np/* sh94 */ = String/* EXTERNAL */(_no/* sh90 */),
        _nq/* sh97 */ = E(_hW/* LudoJS.lvl35 */),
        _nr/* sh9a */ = strEq/* EXTERNAL */(_np/* sh94 */, _nq/* sh97 */);
        if(!E(_nr/* sh9a */)){
          var _ns/* sham */ = E(_hV/* LudoJS.lvl34 */),
          _nt/* shap */ = strEq/* EXTERNAL */(_np/* sh94 */, _ns/* sham */);
          if(!E(_nt/* shap */)){
            return E(_hU/* LudoJS.lvl33 */);
          }else{
            var _nu/* shat */ = E(_9M/* LudoJS.$fToAnyOption1 */),
            _nv/* shaw */ = __get/* EXTERNAL */(_nm/* sh8U */, _nu/* shat */),
            _nw/* shaz */ = function(_nx/*  shaA */, _ny/*  shaB */, _/* EXTERNAL */){
              while(1){
                var _nz/*  shaz */ = B((function(_nA/* shaA */, _nB/* shaB */, _/* EXTERNAL */){
                  var _nC/* shaD */ = E(_nA/* shaA */);
                  if(!_nC/* shaD */._){
                    return _nB/* shaB */;
                  }else{
                    var _nD/* shaF */ = _nC/* shaD */.b,
                    _nE/* shaG */ = E(_nC/* shaD */.a),
                    _nF/* shaK */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _nE/* shaG */, _4/* GHC.Types.[] */))),
                    _nG/* shaO */ = __has/* EXTERNAL */(_md/* sh8C */, _nF/* shaK */);
                    if(!E(_nG/* shaO */)){
                      var _nH/*   shaB */ = _nB/* shaB */;
                      _nx/*  shaA */ = _nD/* shaF */;
                      _ny/*  shaB */ = _nH/*   shaB */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _nI/* shaT */ = __get/* EXTERNAL */(_md/* sh8C */, _nF/* shaK */),
                      _nJ/* shaX */ = __get/* EXTERNAL */(_nI/* shaT */, _nn/* sh8X */),
                      _nK/* shb1 */ = String/* EXTERNAL */(_nJ/* shaX */),
                      _nL/* shb5 */ = strEq/* EXTERNAL */(_nK/* shb1 */, _nq/* sh97 */);
                      if(!E(_nL/* shb5 */)){
                        var _nM/* shbd */ = strEq/* EXTERNAL */(_nK/* shb1 */, _ns/* sham */);
                        if(!E(_nM/* shbd */)){
                          return E(_hU/* LudoJS.lvl33 */);
                        }else{
                          var _nN/* shbi */ = __get/* EXTERNAL */(_nI/* shaT */, _nu/* shat */),
                          _nO/* shbx */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_nB/* shaB */, new T2(1,new T2(0,_nE/* shaG */,new T1(1,new T(function(){
                              var _nP/* shbm */ = Number/* EXTERNAL */(_nN/* shbi */);
                              return jsTrunc/* EXTERNAL */(_nP/* shbm */);
                            }))),_4/* GHC.Types.[] */)));
                          });
                          _nx/*  shaA */ = _nD/* shaF */;
                          _ny/*  shaB */ = _nO/* shbx */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        _nx/*  shaA */ = _nD/* shaF */;
                        _ny/*  shaB */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_nB/* shaB */, new T2(1,new T2(0,_nE/* shaG */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                        });
                        return __continue/* EXTERNAL */;
                      }
                    }
                  }
                })(_nx/*  shaA */, _ny/*  shaB */, _/* EXTERNAL */));
                if(_nz/*  shaz */!=__continue/* EXTERNAL */){
                  return _nz/*  shaz */;
                }
              }
            },
            _nQ/* shbM */ = new T(function(){
              return B(_q/* GHC.Base.++ */(_nj/* sh8H */, new T2(1,new T2(0,_nh/* sh8F */,new T1(1,new T(function(){
                var _nR/* shbB */ = Number/* EXTERNAL */(_nv/* shaw */);
                return jsTrunc/* EXTERNAL */(_nR/* shbB */);
              }))),_4/* GHC.Types.[] */)));
            });
            return new F(function(){return _nw/* shaz */(_ni/* sh8G */, _nQ/* shbM */, _/* EXTERNAL */);});
          }
        }else{
          var _nS/* sh9e */ = function(_nT/*  sh9f */, _nU/*  sh9g */, _/* EXTERNAL */){
            while(1){
              var _nV/*  sh9e */ = B((function(_nW/* sh9f */, _nX/* sh9g */, _/* EXTERNAL */){
                var _nY/* sh9i */ = E(_nW/* sh9f */);
                if(!_nY/* sh9i */._){
                  return _nX/* sh9g */;
                }else{
                  var _nZ/* sh9k */ = _nY/* sh9i */.b,
                  _o0/* sh9l */ = E(_nY/* sh9i */.a),
                  _o1/* sh9p */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _o0/* sh9l */, _4/* GHC.Types.[] */))),
                  _o2/* sh9t */ = __has/* EXTERNAL */(_md/* sh8C */, _o1/* sh9p */);
                  if(!E(_o2/* sh9t */)){
                    var _o3/*   sh9g */ = _nX/* sh9g */;
                    _nT/*  sh9f */ = _nZ/* sh9k */;
                    _nU/*  sh9g */ = _o3/*   sh9g */;
                    return __continue/* EXTERNAL */;
                  }else{
                    var _o4/* sh9y */ = __get/* EXTERNAL */(_md/* sh8C */, _o1/* sh9p */),
                    _o5/* sh9C */ = __get/* EXTERNAL */(_o4/* sh9y */, _nn/* sh8X */),
                    _o6/* sh9G */ = String/* EXTERNAL */(_o5/* sh9C */),
                    _o7/* sh9K */ = strEq/* EXTERNAL */(_o6/* sh9G */, _nq/* sh97 */);
                    if(!E(_o7/* sh9K */)){
                      var _o8/* sh9U */ = strEq/* EXTERNAL */(_o6/* sh9G */, E(_hV/* LudoJS.lvl34 */));
                      if(!E(_o8/* sh9U */)){
                        return E(_hU/* LudoJS.lvl33 */);
                      }else{
                        var _o9/* sha1 */ = __get/* EXTERNAL */(_o4/* sh9y */, E(_9M/* LudoJS.$fToAnyOption1 */)),
                        _oa/* shag */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_nX/* sh9g */, new T2(1,new T2(0,_o0/* sh9l */,new T1(1,new T(function(){
                            var _ob/* sha5 */ = Number/* EXTERNAL */(_o9/* sha1 */);
                            return jsTrunc/* EXTERNAL */(_ob/* sha5 */);
                          }))),_4/* GHC.Types.[] */)));
                        });
                        _nT/*  sh9f */ = _nZ/* sh9k */;
                        _nU/*  sh9g */ = _oa/* shag */;
                        return __continue/* EXTERNAL */;
                      }
                    }else{
                      _nT/*  sh9f */ = _nZ/* sh9k */;
                      _nU/*  sh9g */ = new T(function(){
                        return B(_q/* GHC.Base.++ */(_nX/* sh9g */, new T2(1,new T2(0,_o0/* sh9l */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                      });
                      return __continue/* EXTERNAL */;
                    }
                  }
                }
              })(_nT/*  sh9f */, _nU/*  sh9g */, _/* EXTERNAL */));
              if(_nV/*  sh9e */!=__continue/* EXTERNAL */){
                return _nV/*  sh9e */;
              }
            }
          };
          return new F(function(){return _nS/* sh9e */(_ni/* sh8G */, new T(function(){
            return B(_q/* GHC.Base.++ */(_nj/* sh8H */, new T2(1,new T2(0,_nh/* sh8F */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
          }), _/* EXTERNAL */);});
        }
      }
    },
    _oc/* sheZ */ = B(_ng/* sh8D */(1, _hT/* LudoJS.a34 */, _4/* GHC.Types.[] */, _/* EXTERNAL */)),
    _od/* shf2 */ = B(_k3/* sh8q */(_mb/* sh8s */, _/* EXTERNAL */));
    return new T2(1,new T2(0,_ma/* sh8r */,_oc/* sheZ */),_od/* shf2 */);
  },
  _oe/* shlP */ = B(_m9/* sh8p */(_bo/* LudoJS.Green */, _hY/* LudoJS.lvl9 */, _/* EXTERNAL */));
  return new T(function(){
    return B(_hy/* LudoJS.$sfromList */(new T2(1,new T2(0,_bn/* LudoJS.Blue */,_k2/* sh8m */),_oe/* shlP */)));
  });
},
_of/* GameFinished */ = new T0(3),
_og/* lvl28 */ = new T(function(){
  return B(_1o/* Control.Exception.Base.patError */("LudoJS.hs:(90,5)-(94,45)|case"));
}),
_oh/* lvl29 */ = "GameFinished",
_oi/* lvl30 */ = "SelectField",
_oj/* lvl31 */ = "SelectPiece",
_ok/* lvl32 */ = "Roll",
_ol/* $wa2 */ = function(_om/* sgBt */, _/* EXTERNAL */){
  var _on/* sgBy */ = __get/* EXTERNAL */(_om/* sgBt */, E(_95/* LudoJS.$fFromAnyGameState16 */)),
  _oo/* sgBC */ = String/* EXTERNAL */(_on/* sgBy */),
  _op/* sgBI */ = strEq/* EXTERNAL */(_oo/* sgBC */, E(_ok/* LudoJS.lvl32 */));
  if(!E(_op/* sgBI */)){
    var _oq/* sgC7 */ = strEq/* EXTERNAL */(_oo/* sgBC */, E(_oj/* LudoJS.lvl31 */));
    if(!E(_oq/* sgC7 */)){
      var _or/* sgCu */ = strEq/* EXTERNAL */(_oo/* sgBC */, E(_oi/* LudoJS.lvl30 */));
      if(!E(_or/* sgCu */)){
        var _os/* sgD6 */ = strEq/* EXTERNAL */(_oo/* sgBC */, E(_oh/* LudoJS.lvl29 */));
        return (E(_os/* sgD6 */)==0) ? E(_og/* LudoJS.lvl28 */) : _of/* LudoJS.GameFinished */;
      }else{
        var _ot/* sgCB */ = __get/* EXTERNAL */(_om/* sgBt */, E(_9s/* LudoJS.$fToAnyGameState11 */)),
        _ou/* sgCH */ = __get/* EXTERNAL */(_om/* sgBt */, E(_9D/* LudoJS.$fToAnyGameState6 */));
        return new T2(2,new T(function(){
          var _ov/* sgCL */ = Number/* EXTERNAL */(_ot/* sgCB */);
          return jsTrunc/* EXTERNAL */(_ov/* sgCL */);
        }),new T(function(){
          var _ow/* sgCU */ = Number/* EXTERNAL */(_ou/* sgCH */);
          return jsTrunc/* EXTERNAL */(_ow/* sgCU */);
        }));
      }
    }else{
      var _ox/* sgCe */ = __get/* EXTERNAL */(_om/* sgBt */, E(_9s/* LudoJS.$fToAnyGameState11 */));
      return new T1(1,new T(function(){
        var _oy/* sgCi */ = Number/* EXTERNAL */(_ox/* sgCe */);
        return jsTrunc/* EXTERNAL */(_oy/* sgCi */);
      }));
    }
  }else{
    var _oz/* sgBP */ = __get/* EXTERNAL */(_om/* sgBt */, E(_9s/* LudoJS.$fToAnyGameState11 */));
    return new T1(0,new T(function(){
      var _oA/* sgBT */ = Number/* EXTERNAL */(_oz/* sgBP */),
      _oB/* sgBX */ = jsTrunc/* EXTERNAL */(_oA/* sgBT */),
      _oC/* sgC0 */ = E(_oB/* sgBX */);
      if(_oC/* sgC0 */==( -1)){
        return __Z/* EXTERNAL */;
      }else{
        return new T1(1,_oC/* sgC0 */);
      }
    }));
  }
},
_oD/* $wa1 */ = function(_oE/* shmi */, _/* EXTERNAL */){
  var _oF/* shmn */ = __get/* EXTERNAL */(_oE/* shmi */, E(_95/* LudoJS.$fFromAnyGameState16 */)),
  _oG/* shmq */ = B(_ol/* LudoJS.$wa2 */(_oF/* shmn */, _/* EXTERNAL */)),
  _oH/* shmw */ = __get/* EXTERNAL */(_oE/* shmi */, E(_94/* LudoJS.$fFromAnyGameState15 */)),
  _oI/* shmA */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_oH/* shmw */, _/* EXTERNAL */)),
  _oJ/* shmG */ = __get/* EXTERNAL */(_oE/* shmi */, E(_98/* LudoJS.$fFromAnyGameState8 */)),
  _oK/* shmM */ = __get/* EXTERNAL */(_oE/* shmi */, E(_97/* LudoJS.$fFromAnyGameState7 */)),
  _oL/* shmQ */ = B(_hZ/* LudoJS.$fFromAnyGameState6 */(_oK/* shmM */, _/* EXTERNAL */)),
  _oM/* shmW */ = __get/* EXTERNAL */(_oE/* shmi */, E(_96/* LudoJS.$fFromAnyGameState5 */)),
  _oN/* shn0 */ = __arr2lst/* EXTERNAL */(0, _oM/* shmW */),
  _oO/* shn4 */ = B(_dM/* LudoJS.$fFromAnyGameState4 */(_oN/* shn0 */, _/* EXTERNAL */));
  return new T5(0,_oG/* shmq */,_oI/* shmA */,new T(function(){
    var _oP/* shn8 */ = Number/* EXTERNAL */(_oJ/* shmG */);
    return jsTrunc/* EXTERNAL */(_oP/* shn8 */);
  }),_oL/* shmQ */,_oO/* shn4 */);
},
_oQ/* go2 */ = function(_oR/* shrk */){
  while(1){
    var _oS/* shrl */ = E(_oR/* shrk */);
    if(!_oS/* shrl */._){
      return true;
    }else{
      if(!E(E(_oS/* shrl */.a).b)._){
        _oR/* shrk */ = _oS/* shrl */.b;
        continue;
      }else{
        return false;
      }
    }
  }
},
_oT/* lvl7 */ = 1,
_oU/* $wa12 */ = function(_oV/* shrt */, _/* EXTERNAL */){
  var _oW/* shrO */ = new T(function(){
    var _oX/* shrv */ = E(_oV/* shrt */),
    _oY/* shrz */ = _oX/* shrv */.d,
    _oZ/* shrB */ = new T(function(){
      switch(E(_oX/* shrv */.b)){
        case 0:
          switch(B(_1E/* GHC.Classes.modInt# */(1, 4))+1|0){
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
              return E(_5B/* LudoJS.numToColor1 */);
          }
          break;
        case 1:
          switch(B(_1E/* GHC.Classes.modInt# */(2, 4))+1|0){
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
              return E(_5B/* LudoJS.numToColor1 */);
          }
          break;
        case 2:
          switch(B(_1E/* GHC.Classes.modInt# */(4, 4))+1|0){
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
              return E(_5B/* LudoJS.numToColor1 */);
          }
          break;
        default:
          switch(B(_1E/* GHC.Classes.modInt# */(3, 4))+1|0){
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
              return E(_5B/* LudoJS.numToColor1 */);
          }
      }
    });
    return new T5(0,_b8/* LudoJS.play11 */,_oZ/* shrB */,new T(function(){
      if(!B(_oQ/* LudoJS.go2 */(B(_bt/* LudoJS.$s!1 */(_oZ/* shrB */, _oY/* shrz */))))){
        return E(_oT/* LudoJS.lvl7 */);
      }else{
        return E(_b7/* LudoJS.play10 */);
      }
    }),_oY/* shrz */,_oX/* shrv */.e);
  });
  return new T2(0,_2s/* GHC.Tuple.() */,_oW/* shrO */);
},
_p0/* $wa13 */ = function(_p1/* shrQ */, _p2/* shrR */, _p3/* shrS */, _p4/* shrT */, _p5/* shrU */, _/* EXTERNAL */){
  var _p6/* shrY */ = new T5(0,_p1/* shrQ */,_p2/* shrR */,_p3/* shrS */,_p4/* shrT */,_p5/* shrU */),
  _p7/* shrZ */ = function(_p8/* shs0 */){
    var _p9/* shs1 */ = B(_oU/* LudoJS.$wa12 */(_p6/* shrY */, _/* EXTERNAL */)),
    _pa/* shs4 */ = E(_p9/* shs1 */),
    _pb/* shs7 */ = E(_pa/* shs4 */.b),
    _pc/* shsd */ = B(_p0/* LudoJS.$wa13 */(_pb/* shs7 */.a, _pb/* shs7 */.b, _pb/* shs7 */.c, _pb/* shs7 */.d, _pb/* shs7 */.e, _/* EXTERNAL */));
    return new T2(0,new T2(1,_pa/* shs4 */.a,new T(function(){
      return E(E(_pc/* shsd */).a);
    })),new T(function(){
      return E(E(_pc/* shsd */).b);
    }));
  };
  if(!E(B(_bB/* GHC.List.$wlenAcc */(B(_bt/* LudoJS.$s!1 */(_p2/* shrR */, _p4/* shrT */)), 0)))){
    return new F(function(){return _p7/* shrZ */(_/* EXTERNAL */);});
  }else{
    if(E(_p3/* shrS */)>=1){
      return new T2(0,_4/* GHC.Types.[] */,_p6/* shrY */);
    }else{
      return new F(function(){return _p7/* shrZ */(_/* EXTERNAL */);});
    }
  }
},
_pd/* $wa14 */ = function(_pe/* shsw */, _pf/* shsx */, _pg/* shsy */, _ph/* shsz */, _pi/* shsA */, _/* EXTERNAL */){
  var _pj/* shsE */ = new T5(0,_pe/* shsw */,_pf/* shsx */,_pg/* shsy */,_ph/* shsz */,_pi/* shsA */),
  _pk/* shsF */ = function(_pl/* shsG */){
    var _pm/* shsH */ = B(_oU/* LudoJS.$wa12 */(_pj/* shsE */, _/* EXTERNAL */)),
    _pn/* shsK */ = E(_pm/* shsH */),
    _po/* shsN */ = E(_pn/* shsK */.b),
    _pp/* shsT */ = B(_pd/* LudoJS.$wa14 */(_po/* shsN */.a, _po/* shsN */.b, _po/* shsN */.c, _po/* shsN */.d, _po/* shsN */.e, _/* EXTERNAL */));
    return new T2(0,new T2(1,_pn/* shsK */.a,new T(function(){
      return E(E(_pp/* shsT */).a);
    })),new T(function(){
      return E(E(_pp/* shsT */).b);
    }));
  };
  if(!E(B(_bB/* GHC.List.$wlenAcc */(B(_bt/* LudoJS.$s!1 */(_pf/* shsx */, _ph/* shsz */)), 0)))){
    return new F(function(){return _pk/* shsF */(_/* EXTERNAL */);});
  }else{
    if(E(_pg/* shsy */)>=1){
      return new T2(0,_4/* GHC.Types.[] */,_pj/* shsE */);
    }else{
      return new F(function(){return _pk/* shsF */(_/* EXTERNAL */);});
    }
  }
},
_pq/* neInt */ = function(_pr/* scEM */, _ps/* scEN */){
  return E(_pr/* scEM */)!=E(_ps/* scEN */);
},
_pt/* $fEqInt */ = new T2(0,_b9/* GHC.Classes.eqInt */,_pq/* GHC.Classes.neInt */),
_pu/* $soutByCell */ = function(_pv/* sgGo */, _pw/* sgGp */){
  var _px/* sgGq */ = E(_pv/* sgGo */);
  if(!_px/* sgGq */._){
    return __Z/* EXTERNAL */;
  }else{
    var _py/* sgGs */ = _px/* sgGq */.b,
    _pz/* sgGt */ = E(_px/* sgGq */.a),
    _pA/* sgGw */ = E(_pz/* sgGt */.b);
    return (_pA/* sgGw */._==0) ? new T2(1,_pz/* sgGt */,new T(function(){
      return B(_pu/* LudoJS.$soutByCell */(_py/* sgGs */, _pw/* sgGp */));
    })) : (_pw/* sgGp */!=E(_pA/* sgGw */.a)) ? new T2(1,_pz/* sgGt */,new T(function(){
      return B(_pu/* LudoJS.$soutByCell */(_py/* sgGs */, _pw/* sgGp */));
    })) : new T2(1,new T2(0,_pz/* sgGt */.a,_hO/* LudoJS.Out */),new T(function(){
      return B(_pu/* LudoJS.$soutByCell */(_py/* sgGs */, _pw/* sgGp */));
    }));
  }
},
_pB/* $sremoveFrom */ = function(_pC/* sgH0 */, _pD/* sgH1 */){
  var _pE/* sgH2 */ = E(_pC/* sgH0 */);
  if(!_pE/* sgH2 */._){
    return __Z/* EXTERNAL */;
  }else{
    var _pF/* sgH4 */ = _pE/* sgH2 */.b,
    _pG/* sgH5 */ = E(_pE/* sgH2 */.a);
    return (_pD/* sgH1 */!=E(_pG/* sgH5 */.a)) ? new T2(1,_pG/* sgH5 */,new T(function(){
      return B(_pB/* LudoJS.$sremoveFrom */(_pF/* sgH4 */, _pD/* sgH1 */));
    })) : E(_pF/* sgH4 */);
  }
},
_pH/* lvl1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("!!: negative index"));
}),
_pI/* prel_list_str */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Prelude."));
}),
_pJ/* lvl2 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_pI/* GHC.List.prel_list_str */, _pH/* GHC.List.lvl1 */));
}),
_pK/* negIndex */ = new T(function(){
  return B(err/* EXTERNAL */(_pJ/* GHC.List.lvl2 */));
}),
_pL/* lvl3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("!!: index too large"));
}),
_pM/* lvl4 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_pI/* GHC.List.prel_list_str */, _pL/* GHC.List.lvl3 */));
}),
_pN/* !!1 */ = new T(function(){
  return B(err/* EXTERNAL */(_pM/* GHC.List.lvl4 */));
}),
_pO/* poly_$wgo2 */ = function(_pP/* sbFw */, _pQ/* sbFx */){
  while(1){
    var _pR/* sbFy */ = E(_pP/* sbFw */);
    if(!_pR/* sbFy */._){
      return E(_pN/* GHC.List.!!1 */);
    }else{
      var _pS/* sbFB */ = E(_pQ/* sbFx */);
      if(!_pS/* sbFB */){
        return E(_pR/* sbFy */.a);
      }else{
        _pP/* sbFw */ = _pR/* sbFy */.b;
        _pQ/* sbFx */ = _pS/* sbFB */-1|0;
        continue;
      }
    }
  }
},
_pT/* $w!! */ = function(_pU/* sbFD */, _pV/* sbFE */){
  if(_pV/* sbFE */>=0){
    return new F(function(){return _pO/* GHC.List.poly_$wgo2 */(_pU/* sbFD */, _pV/* sbFE */);});
  }else{
    return E(_pK/* GHC.List.negIndex */);
  }
},
_pW/* $s!_$spoly_go1 */ = function(_pX/* sgwY */){
  while(1){
    var _pY/* sgwZ */ = E(_pX/* sgwY */);
    if(!_pY/* sgwZ */._){
      var _pZ/* sgx3 */ = _pY/* sgwZ */.d;
      switch(E(_pY/* sgwZ */.b)){
        case 0:
          _pX/* sgwY */ = _pY/* sgwZ */.e;
          continue;
        case 1:
          return E(_pY/* sgwZ */.c);
        case 2:
          _pX/* sgwY */ = _pZ/* sgx3 */;
          continue;
        default:
          _pX/* sgwY */ = _pZ/* sgx3 */;
          continue;
      }
    }else{
      return E(_bs/* LudoJS.lvl27 */);
    }
  }
},
_q0/* $s!_$spoly_go10 */ = function(_q1/* sgwQ */){
  while(1){
    var _q2/* sgwR */ = E(_q1/* sgwQ */);
    if(!_q2/* sgwR */._){
      if(E(_q2/* sgwR */.b)==3){
        return E(_q2/* sgwR */.c);
      }else{
        _q1/* sgwQ */ = _q2/* sgwR */.e;
        continue;
      }
    }else{
      return E(_bs/* LudoJS.lvl27 */);
    }
  }
},
_q3/* $s!_$spoly_go2 */ = function(_q4/* sgx6 */){
  while(1){
    var _q5/* sgx7 */ = E(_q4/* sgx6 */);
    if(!_q5/* sgx7 */._){
      var _q6/* sgxb */ = _q5/* sgx7 */.d;
      switch(E(_q5/* sgx7 */.b)){
        case 0:
          return E(_q5/* sgx7 */.c);
        case 1:
          _q4/* sgx6 */ = _q6/* sgxb */;
          continue;
        case 2:
          _q4/* sgx6 */ = _q6/* sgxb */;
          continue;
        default:
          _q4/* sgx6 */ = _q6/* sgxb */;
          continue;
      }
    }else{
      return E(_bs/* LudoJS.lvl27 */);
    }
  }
},
_q7/* $sinsert_$s$sgo1 */ = function(_q8/* sgz2 */, _q9/* sgz3 */){
  var _qa/* sgz4 */ = E(_q9/* sgz3 */);
  if(!_qa/* sgz4 */._){
    var _qb/* sgz7 */ = _qa/* sgz4 */.c,
    _qc/* sgz8 */ = _qa/* sgz4 */.d,
    _qd/* sgz9 */ = _qa/* sgz4 */.e;
    switch(E(_qa/* sgz4 */.b)){
      case 0:
        return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bn/* LudoJS.Blue */, _qb/* sgz7 */, _qc/* sgz8 */, B(_q7/* LudoJS.$sinsert_$s$sgo1 */(_q8/* sgz2 */, _qd/* sgz9 */)));});
        break;
      case 1:
        return new T5(0,_qa/* sgz4 */.a,E(_bo/* LudoJS.Green */),_q8/* sgz2 */,E(_qc/* sgz8 */),E(_qd/* sgz9 */));
      case 2:
        return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bp/* LudoJS.Red */, _qb/* sgz7 */, B(_q7/* LudoJS.$sinsert_$s$sgo1 */(_q8/* sgz2 */, _qc/* sgz8 */)), _qd/* sgz9 */);});
        break;
      default:
        return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bq/* LudoJS.Yellow */, _qb/* sgz7 */, B(_q7/* LudoJS.$sinsert_$s$sgo1 */(_q8/* sgz2 */, _qc/* sgz8 */)), _qd/* sgz9 */);});
    }
  }else{
    return new T5(0,1,E(_bo/* LudoJS.Green */),_q8/* sgz2 */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
  }
},
_qe/* $sinsert_$s$sgo10 */ = function(_qf/* sgyQ */, _qg/* sgyR */){
  var _qh/* sgyS */ = E(_qg/* sgyR */);
  if(!_qh/* sgyS */._){
    var _qi/* sgyV */ = _qh/* sgyS */.c,
    _qj/* sgyW */ = _qh/* sgyS */.d,
    _qk/* sgyX */ = _qh/* sgyS */.e;
    switch(E(_qh/* sgyS */.b)){
      case 0:
        return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bn/* LudoJS.Blue */, _qi/* sgyV */, _qj/* sgyW */, B(_qe/* LudoJS.$sinsert_$s$sgo10 */(_qf/* sgyQ */, _qk/* sgyX */)));});
        break;
      case 1:
        return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bo/* LudoJS.Green */, _qi/* sgyV */, _qj/* sgyW */, B(_qe/* LudoJS.$sinsert_$s$sgo10 */(_qf/* sgyQ */, _qk/* sgyX */)));});
        break;
      case 2:
        return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bp/* LudoJS.Red */, _qi/* sgyV */, _qj/* sgyW */, B(_qe/* LudoJS.$sinsert_$s$sgo10 */(_qf/* sgyQ */, _qk/* sgyX */)));});
        break;
      default:
        return new T5(0,_qh/* sgyS */.a,E(_bq/* LudoJS.Yellow */),_qf/* sgyQ */,E(_qj/* sgyW */),E(_qk/* sgyX */));
    }
  }else{
    return new T5(0,1,E(_bq/* LudoJS.Yellow */),_qf/* sgyQ */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
  }
},
_ql/* $sinsert_$s$sgo2 */ = function(_qm/* sgze */, _qn/* sgzf */){
  var _qo/* sgzg */ = E(_qn/* sgzf */);
  if(!_qo/* sgzg */._){
    var _qp/* sgzj */ = _qo/* sgzg */.c,
    _qq/* sgzk */ = _qo/* sgzg */.d,
    _qr/* sgzl */ = _qo/* sgzg */.e;
    switch(E(_qo/* sgzg */.b)){
      case 0:
        return new T5(0,_qo/* sgzg */.a,E(_bn/* LudoJS.Blue */),_qm/* sgze */,E(_qq/* sgzk */),E(_qr/* sgzl */));
      case 1:
        return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bo/* LudoJS.Green */, _qp/* sgzj */, B(_ql/* LudoJS.$sinsert_$s$sgo2 */(_qm/* sgze */, _qq/* sgzk */)), _qr/* sgzl */);});
        break;
      case 2:
        return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bp/* LudoJS.Red */, _qp/* sgzj */, B(_ql/* LudoJS.$sinsert_$s$sgo2 */(_qm/* sgze */, _qq/* sgzk */)), _qr/* sgzl */);});
        break;
      default:
        return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bq/* LudoJS.Yellow */, _qp/* sgzj */, B(_ql/* LudoJS.$sinsert_$s$sgo2 */(_qm/* sgze */, _qq/* sgzk */)), _qr/* sgzl */);});
    }
  }else{
    return new T5(0,1,E(_bn/* LudoJS.Blue */),_qm/* sgze */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
  }
},
_qs/* False */ = false,
_qt/* geInteger */ = function(_qu/* s1FG */, _qv/* s1FH */){
  var _qw/* s1FI */ = E(_qu/* s1FG */);
  if(!_qw/* s1FI */._){
    var _qx/* s1FJ */ = _qw/* s1FI */.a,
    _qy/* s1FK */ = E(_qv/* s1FH */);
    return (_qy/* s1FK */._==0) ? _qx/* s1FJ */>=_qy/* s1FK */.a : I_compareInt/* EXTERNAL */(_qy/* s1FK */.a, _qx/* s1FJ */)<=0;
  }else{
    var _qz/* s1FR */ = _qw/* s1FI */.a,
    _qA/* s1FS */ = E(_qv/* s1FH */);
    return (_qA/* s1FS */._==0) ? I_compareInt/* EXTERNAL */(_qz/* s1FR */, _qA/* s1FS */.a)>=0 : I_compare/* EXTERNAL */(_qz/* s1FR */, _qA/* s1FS */.a)>=0;
  }
},
_qB/* lvl11 */ = new T1(0,2),
_qC/* lvl12 */ = new T1(0,0),
_qD/* lvl13 */ = new T1(0,1),
_qE/* outByCell */ = function(_qF/* sgGG */, _qG/* sgGH */){
  var _qH/* sgGI */ = E(_qF/* sgGG */);
  if(!_qH/* sgGI */._){
    return __Z/* EXTERNAL */;
  }else{
    var _qI/* sgGK */ = _qH/* sgGI */.b,
    _qJ/* sgGL */ = E(_qH/* sgGI */.a),
    _qK/* sgGO */ = E(_qJ/* sgGL */.b);
    if(!_qK/* sgGO */._){
      return new T2(1,_qJ/* sgGL */,new T(function(){
        return B(_qE/* LudoJS.outByCell */(_qI/* sgGK */, _qG/* sgGH */));
      }));
    }else{
      var _qL/* sgGR */ = E(_qG/* sgGH */);
      return (_qL/* sgGR */!=E(_qK/* sgGO */.a)) ? new T2(1,_qJ/* sgGL */,new T(function(){
        return B(_pu/* LudoJS.$soutByCell */(_qI/* sgGK */, _qL/* sgGR */));
      })) : new T2(1,new T2(0,_qJ/* sgGL */.a,_hO/* LudoJS.Out */),new T(function(){
        return B(_pu/* LudoJS.$soutByCell */(_qI/* sgGK */, _qL/* sgGR */));
      }));
    }
  }
},
_qM/* plusInteger */ = function(_qN/* s1Qe */, _qO/* s1Qf */){
  while(1){
    var _qP/* s1Qg */ = E(_qN/* s1Qe */);
    if(!_qP/* s1Qg */._){
      var _qQ/* s1Qh */ = _qP/* s1Qg */.a,
      _qR/* s1Qi */ = E(_qO/* s1Qf */);
      if(!_qR/* s1Qi */._){
        var _qS/* s1Qj */ = _qR/* s1Qi */.a,
        _qT/* s1Qk */ = addC/* EXTERNAL */(_qQ/* s1Qh */, _qS/* s1Qj */);
        if(!E(_qT/* s1Qk */.b)){
          return new T1(0,_qT/* s1Qk */.a);
        }else{
          _qN/* s1Qe */ = new T1(1,I_fromInt/* EXTERNAL */(_qQ/* s1Qh */));
          _qO/* s1Qf */ = new T1(1,I_fromInt/* EXTERNAL */(_qS/* s1Qj */));
          continue;
        }
      }else{
        _qN/* s1Qe */ = new T1(1,I_fromInt/* EXTERNAL */(_qQ/* s1Qh */));
        _qO/* s1Qf */ = _qR/* s1Qi */;
        continue;
      }
    }else{
      var _qU/* s1Qz */ = E(_qO/* s1Qf */);
      if(!_qU/* s1Qz */._){
        _qN/* s1Qe */ = _qP/* s1Qg */;
        _qO/* s1Qf */ = new T1(1,I_fromInt/* EXTERNAL */(_qU/* s1Qz */.a));
        continue;
      }else{
        return new T1(1,I_add/* EXTERNAL */(_qP/* s1Qg */.a, _qU/* s1Qz */.a));
      }
    }
  }
},
_qV/* a43 */ = function(_qW/* sgJi */, _qX/* sgJj */, _qY/* sgJk */, _/* EXTERNAL */){
  var _qZ/* sgJm */ = function(_r0/* sgJn */){
    var _r1/* sgJo */ = E(_r0/* sgJn */);
    if(!_r1/* sgJo */._){
      return E(_qC/* LudoJS.lvl12 */);
    }else{
      var _r2/* sgJq */ = _r1/* sgJo */.b,
      _r3/* sgJr */ = E(_qX/* sgJj */);
      if(_r3/* sgJr */!=E(_r1/* sgJo */.a)){
        var _r4/* sgJx */ = function(_r5/* sgJy */){
          while(1){
            var _r6/* sgJz */ = E(_r5/* sgJy */);
            if(!_r6/* sgJz */._){
              return E(_qC/* LudoJS.lvl12 */);
            }else{
              var _r7/* sgJB */ = _r6/* sgJz */.b;
              if(_r3/* sgJr */!=E(_r6/* sgJz */.a)){
                _r5/* sgJy */ = _r7/* sgJB */;
                continue;
              }else{
                return new F(function(){return _qM/* GHC.Integer.Type.plusInteger */(B(_r4/* sgJx */(_r7/* sgJB */)), _qD/* LudoJS.lvl13 */);});
              }
            }
          }
        };
        return new F(function(){return _r4/* sgJx */(_r2/* sgJq */);});
      }else{
        var _r8/* sgJH */ = function(_r9/* sgJI */){
          while(1){
            var _ra/* sgJJ */ = E(_r9/* sgJI */);
            if(!_ra/* sgJJ */._){
              return E(_qC/* LudoJS.lvl12 */);
            }else{
              var _rb/* sgJL */ = _ra/* sgJJ */.b;
              if(_r3/* sgJr */!=E(_ra/* sgJJ */.a)){
                _r9/* sgJI */ = _rb/* sgJL */;
                continue;
              }else{
                return new F(function(){return _qM/* GHC.Integer.Type.plusInteger */(B(_r8/* sgJH */(_rb/* sgJL */)), _qD/* LudoJS.lvl13 */);});
              }
            }
          }
        };
        return new F(function(){return _qM/* GHC.Integer.Type.plusInteger */(B(_r8/* sgJH */(_r2/* sgJq */)), _qD/* LudoJS.lvl13 */);});
      }
    }
  },
  _rc/* sgJS */ = function(_rd/* sgJT */, _/* EXTERNAL */){
    var _re/* sgKj */ = new T(function(){
      var _rf/* sgK1 */ = function(_rg/*  sgK2 */){
        while(1){
          var _rh/*  sgK1 */ = B((function(_ri/* sgK2 */){
            var _rj/* sgK3 */ = E(_ri/* sgK2 */);
            if(!_rj/* sgK3 */._){
              return __Z/* EXTERNAL */;
            }else{
              var _rk/* sgK5 */ = _rj/* sgK3 */.b,
              _rl/* sgK9 */ = E(E(_rj/* sgK3 */.a).b);
              if(!_rl/* sgK9 */._){
                _rg/*  sgK2 */ = _rk/* sgK5 */;
                return __continue/* EXTERNAL */;
              }else{
                return new T2(1,new T(function(){
                  return B(_1L/* LudoJS.$wconvertCell */(_bn/* LudoJS.Blue */, E(_rl/* sgK9 */.a), _qW/* sgJi */));
                }),new T(function(){
                  return B(_rf/* sgK1 */(_rk/* sgK5 */));
                }));
              }
            }
          })(_rg/*  sgK2 */));
          if(_rh/*  sgK1 */!=__continue/* EXTERNAL */){
            return _rh/*  sgK1 */;
          }
        }
      };
      return B(_qt/* GHC.Integer.Type.geInteger */(B(_qZ/* sgJm */(B(_rf/* sgK1 */(B(_q3/* LudoJS.$s!_$spoly_go2 */(E(_rd/* sgJT */).d)))))), _qB/* LudoJS.lvl11 */));
    });
    return new T2(0,_re/* sgKj */,_rd/* sgJT */);
  },
  _rm/* sgKl */ = function(_/* EXTERNAL */, _rn/* sgKn */){
    var _ro/* sgKo */ = E(_rn/* sgKn */),
    _rp/* sgKq */ = _ro/* sgKo */.b;
    if(!E(_ro/* sgKo */.a)){
      var _rq/* sgKs */ = function(_/* EXTERNAL */, _rr/* sgKu */, _rs/* sgKv */){
        var _rt/* sgKw */ = function(_/* EXTERNAL */, _ru/* sgKy */, _rv/* sgKz */){
          var _rw/* sgKA */ = function(_/* EXTERNAL */, _rx/* sgKC */){
            var _ry/* sgKD */ = E(_qW/* sgJi */);
            if(_ry/* sgKD */==2){
              return new T2(0,_2s/* GHC.Tuple.() */,new T(function(){
                return E(E(_rx/* sgKC */).b);
              }));
            }else{
              var _rz/* sgLc */ = new T(function(){
                var _rA/* sgKH */ = E(E(_rx/* sgKC */).b),
                _rB/* sgKL */ = _rA/* sgKH */.d,
                _rC/* sgLb */ = new T(function(){
                  var _rD/* sgLa */ = new T(function(){
                    return B(_qE/* LudoJS.outByCell */(B(_bt/* LudoJS.$s!1 */(_bp/* LudoJS.Red */, _rB/* sgKL */)), new T(function(){
                      var _rE/* sgKO */ = E(_qX/* sgJj */);
                      switch(E(_ry/* sgKD */)){
                        case 0:
                          return B(_1E/* GHC.Classes.modInt# */(_rE/* sgKO */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52));
                          break;
                        case 1:
                          return B(_1E/* GHC.Classes.modInt# */(_rE/* sgKO */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52));
                          break;
                        default:
                          return B(_1E/* GHC.Classes.modInt# */(_rE/* sgKO */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52));
                      }
                    },1)));
                  });
                  return B(_fi/* LudoJS.$sinsert_$sgo10 */(_bp/* LudoJS.Red */, _rD/* sgLa */, _rB/* sgKL */));
                });
                return new T5(0,_rA/* sgKH */.a,_rA/* sgKH */.b,_rA/* sgKH */.c,_rC/* sgLb */,_rA/* sgKH */.e);
              });
              return new T2(0,_2s/* GHC.Tuple.() */,_rz/* sgLc */);
            }
          },
          _rF/* sgLj */ = E(_qW/* sgJi */);
          if(_rF/* sgLj */==3){
            return new F(function(){return _rw/* sgKA */(_/* EXTERNAL */, new T2(0,_2s/* GHC.Tuple.() */,_rv/* sgKz */));});
          }else{
            var _rG/* sgLP */ = new T(function(){
              var _rH/* sgLk */ = E(_rv/* sgKz */),
              _rI/* sgLo */ = _rH/* sgLk */.d,
              _rJ/* sgLO */ = new T(function(){
                var _rK/* sgLN */ = new T(function(){
                  return B(_qE/* LudoJS.outByCell */(B(_q0/* LudoJS.$s!_$spoly_go10 */(_rI/* sgLo */)), new T(function(){
                    var _rL/* sgLr */ = E(_qX/* sgJj */);
                    switch(E(_rF/* sgLj */)){
                      case 0:
                        return B(_1E/* GHC.Classes.modInt# */(_rL/* sgLr */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52));
                        break;
                      case 1:
                        return B(_1E/* GHC.Classes.modInt# */(_rL/* sgLr */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52));
                        break;
                      default:
                        return B(_1E/* GHC.Classes.modInt# */(_rL/* sgLr */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52));
                    }
                  },1)));
                });
                return B(_qe/* LudoJS.$sinsert_$s$sgo10 */(_rK/* sgLN */, _rI/* sgLo */));
              });
              return new T5(0,_rH/* sgLk */.a,_rH/* sgLk */.b,_rH/* sgLk */.c,_rJ/* sgLO */,_rH/* sgLk */.e);
            });
            return new F(function(){return _rw/* sgKA */(_/* EXTERNAL */, new T2(0,_2s/* GHC.Tuple.() */,_rG/* sgLP */));});
          }
        },
        _rM/* sgLS */ = E(_qW/* sgJi */);
        if(_rM/* sgLS */==1){
          return new F(function(){return _rt/* sgKw */(_/* EXTERNAL */, _2s/* GHC.Tuple.() */, _rs/* sgKv */);});
        }else{
          var _rN/* sgMo */ = new T(function(){
            var _rO/* sgLT */ = E(_rs/* sgKv */),
            _rP/* sgLX */ = _rO/* sgLT */.d,
            _rQ/* sgMn */ = new T(function(){
              var _rR/* sgMm */ = new T(function(){
                return B(_qE/* LudoJS.outByCell */(B(_pW/* LudoJS.$s!_$spoly_go1 */(_rP/* sgLX */)), new T(function(){
                  var _rS/* sgM0 */ = E(_qX/* sgJj */);
                  switch(E(_rM/* sgLS */)){
                    case 0:
                      return B(_1E/* GHC.Classes.modInt# */(_rS/* sgM0 */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52));
                      break;
                    case 2:
                      return B(_1E/* GHC.Classes.modInt# */(_rS/* sgM0 */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52));
                      break;
                    default:
                      return B(_1E/* GHC.Classes.modInt# */(_rS/* sgM0 */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52));
                  }
                },1)));
              });
              return B(_q7/* LudoJS.$sinsert_$s$sgo1 */(_rR/* sgMm */, _rP/* sgLX */));
            });
            return new T5(0,_rO/* sgLT */.a,_rO/* sgLT */.b,_rO/* sgLT */.c,_rQ/* sgMn */,_rO/* sgLT */.e);
          },1);
          return new F(function(){return _rt/* sgKw */(_/* EXTERNAL */, _2s/* GHC.Tuple.() */, _rN/* sgMo */);});
        }
      },
      _rT/* sgMp */ = E(_qW/* sgJi */);
      if(!_rT/* sgMp */){
        return new F(function(){return _rq/* sgKs */(_/* EXTERNAL */, _2s/* GHC.Tuple.() */, _rp/* sgKq */);});
      }else{
        var _rU/* sgMV */ = new T(function(){
          var _rV/* sgMq */ = E(_rp/* sgKq */),
          _rW/* sgMu */ = _rV/* sgMq */.d,
          _rX/* sgMU */ = new T(function(){
            var _rY/* sgMT */ = new T(function(){
              return B(_qE/* LudoJS.outByCell */(B(_q3/* LudoJS.$s!_$spoly_go2 */(_rW/* sgMu */)), new T(function(){
                var _rZ/* sgMx */ = E(_qX/* sgJj */);
                switch(E(_rT/* sgMp */)){
                  case 1:
                    return B(_1E/* GHC.Classes.modInt# */(_rZ/* sgMx */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52));
                    break;
                  case 2:
                    return B(_1E/* GHC.Classes.modInt# */(_rZ/* sgMx */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52));
                    break;
                  default:
                    return B(_1E/* GHC.Classes.modInt# */(_rZ/* sgMx */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52));
                }
              },1)));
            });
            return B(_ql/* LudoJS.$sinsert_$s$sgo2 */(_rY/* sgMT */, _rW/* sgMu */));
          });
          return new T5(0,_rV/* sgMq */.a,_rV/* sgMq */.b,_rV/* sgMq */.c,_rX/* sgMU */,_rV/* sgMq */.e);
        },1);
        return new F(function(){return _rq/* sgKs */(_/* EXTERNAL */, _2s/* GHC.Tuple.() */, _rU/* sgMV */);});
      }
    }else{
      var _s0/* sgN5 */ = new T(function(){
        var _s1/* sgMW */ = E(_rp/* sgKq */),
        _s2/* sgN0 */ = _s1/* sgMW */.d,
        _s3/* sgN4 */ = new T(function(){
          return B(_fi/* LudoJS.$sinsert_$sgo10 */(_qW/* sgJi */, new T(function(){
            return B(_qE/* LudoJS.outByCell */(B(_bt/* LudoJS.$s!1 */(_qW/* sgJi */, _s2/* sgN0 */)), _qX/* sgJj */));
          }), _s2/* sgN0 */));
        });
        return new T5(0,_s1/* sgMW */.a,_s1/* sgMW */.b,_s1/* sgMW */.c,_s3/* sgN4 */,_s1/* sgMW */.e);
      });
      return new T2(0,_2s/* GHC.Tuple.() */,_s0/* sgN5 */);
    }
  };
  switch(E(_qW/* sgJi */)){
    case 0:
      var _s4/* sgNa */ = function(_s5/*  sgOc */, _s6/*  sgOd */, _s7/*  sgOe */, _/* EXTERNAL */){
        while(1){
          var _s8/*  sgNa */ = B((function(_s9/* sgOc */, _sa/* sgOd */, _sb/* sgOe */, _/* EXTERNAL */){
            var _sc/* sgOg */ = E(_s9/* sgOc */);
            if(!_sc/* sgOg */._){
              return new T2(0,_sa/* sgOd */,_sb/* sgOe */);
            }else{
              var _sd/* sgOj */ = _sc/* sgOg */.b,
              _se/* sgOk */ = E(_sc/* sgOg */.a);
              if(!_se/* sgOk */){
                var _sf/*   sgOd */ = _sa/* sgOd */,
                _sg/*   sgOe */ = _sb/* sgOe */;
                _s5/*  sgOc */ = _sd/* sgOj */;
                _s6/*  sgOd */ = _sf/*   sgOd */;
                _s7/*  sgOe */ = _sg/*   sgOe */;
                return __continue/* EXTERNAL */;
              }else{
                var _sh/* sgOK */ = new T(function(){
                  if(!E(_sa/* sgOd */)){
                    var _si/* sgOs */ = function(_sj/*  sgOt */){
                      while(1){
                        var _sk/*  sgOs */ = B((function(_sl/* sgOt */){
                          var _sm/* sgOu */ = E(_sl/* sgOt */);
                          if(!_sm/* sgOu */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _sn/* sgOw */ = _sm/* sgOu */.b,
                            _so/* sgOA */ = E(E(_sm/* sgOu */.a).b);
                            if(!_so/* sgOA */._){
                              _sj/*  sgOt */ = _sn/* sgOw */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_1L/* LudoJS.$wconvertCell */(_se/* sgOk */, E(_so/* sgOA */.a), _bn/* LudoJS.Blue */));
                              }),new T(function(){
                                return B(_si/* sgOs */(_sn/* sgOw */));
                              }));
                            }
                          }
                        })(_sj/*  sgOt */));
                        if(_sk/*  sgOs */!=__continue/* EXTERNAL */){
                          return _sk/*  sgOs */;
                        }
                      }
                    };
                    return B(_qt/* GHC.Integer.Type.geInteger */(B(_qZ/* sgJm */(B(_si/* sgOs */(B(_bt/* LudoJS.$s!1 */(_se/* sgOk */, E(_sb/* sgOe */).d)))))), _qB/* LudoJS.lvl11 */));
                  }else{
                    return true;
                  }
                }),
                _sg/*   sgOe */ = _sb/* sgOe */;
                _s5/*  sgOc */ = _sd/* sgOj */;
                _s6/*  sgOd */ = _sh/* sgOK */;
                _s7/*  sgOe */ = _sg/*   sgOe */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_s5/*  sgOc */, _s6/*  sgOd */, _s7/*  sgOe */, _/* EXTERNAL */));
          if(_s8/*  sgNa */!=__continue/* EXTERNAL */){
            return _s8/*  sgNa */;
          }
        }
      },
      _sp/* sgN8 */ = function(_sq/*  sgNb */, _sr/*  sgNc */, _/* EXTERNAL */){
        while(1){
          var _ss/*  sgN8 */ = B((function(_st/* sgNb */, _su/* sgNc */, _/* EXTERNAL */){
            var _sv/* sgNe */ = E(_st/* sgNb */);
            if(!_sv/* sgNe */._){
              return new T2(0,_qs/* GHC.Types.False */,_su/* sgNc */);
            }else{
              var _sw/* sgNh */ = _sv/* sgNe */.b,
              _sx/* sgNi */ = E(_sv/* sgNe */.a);
              if(!_sx/* sgNi */){
                var _sy/*   sgNc */ = _su/* sgNc */;
                _sq/*  sgNb */ = _sw/* sgNh */;
                _sr/*  sgNc */ = _sy/*   sgNc */;
                return __continue/* EXTERNAL */;
              }else{
                var _sz/* sgNH */ = new T(function(){
                  var _sA/* sgNp */ = function(_sB/*  sgNq */){
                    while(1){
                      var _sC/*  sgNp */ = B((function(_sD/* sgNq */){
                        var _sE/* sgNr */ = E(_sD/* sgNq */);
                        if(!_sE/* sgNr */._){
                          return __Z/* EXTERNAL */;
                        }else{
                          var _sF/* sgNt */ = _sE/* sgNr */.b,
                          _sG/* sgNx */ = E(E(_sE/* sgNr */.a).b);
                          if(!_sG/* sgNx */._){
                            _sB/*  sgNq */ = _sF/* sgNt */;
                            return __continue/* EXTERNAL */;
                          }else{
                            return new T2(1,new T(function(){
                              return B(_1L/* LudoJS.$wconvertCell */(_sx/* sgNi */, E(_sG/* sgNx */.a), _bn/* LudoJS.Blue */));
                            }),new T(function(){
                              return B(_sA/* sgNp */(_sF/* sgNt */));
                            }));
                          }
                        }
                      })(_sB/*  sgNq */));
                      if(_sC/*  sgNp */!=__continue/* EXTERNAL */){
                        return _sC/*  sgNp */;
                      }
                    }
                  };
                  return B(_qt/* GHC.Integer.Type.geInteger */(B(_qZ/* sgJm */(B(_sA/* sgNp */(B(_bt/* LudoJS.$s!1 */(_sx/* sgNi */, E(_su/* sgNc */).d)))))), _qB/* LudoJS.lvl11 */));
                });
                return new F(function(){return _s4/* sgNa */(_sw/* sgNh */, _sz/* sgNH */, _su/* sgNc */, _/* EXTERNAL */);});
              }
            }
          })(_sq/*  sgNb */, _sr/*  sgNc */, _/* EXTERNAL */));
          if(_ss/*  sgN8 */!=__continue/* EXTERNAL */){
            return _ss/*  sgN8 */;
          }
        }
      },
      _sH/* sgN9 */ = function(_sI/* sgNI */, _sJ/* sgNJ */, _sK/* sgNK */, _/* EXTERNAL */){
        var _sL/* sgNM */ = E(_sI/* sgNI */);
        if(!_sL/* sgNM */){
          return new F(function(){return _sp/* sgN8 */(_sJ/* sgNJ */, _sK/* sgNK */, _/* EXTERNAL */);});
        }else{
          var _sM/* sgOb */ = new T(function(){
            var _sN/* sgNT */ = function(_sO/*  sgNU */){
              while(1){
                var _sP/*  sgNT */ = B((function(_sQ/* sgNU */){
                  var _sR/* sgNV */ = E(_sQ/* sgNU */);
                  if(!_sR/* sgNV */._){
                    return __Z/* EXTERNAL */;
                  }else{
                    var _sS/* sgNX */ = _sR/* sgNV */.b,
                    _sT/* sgO1 */ = E(E(_sR/* sgNV */.a).b);
                    if(!_sT/* sgO1 */._){
                      _sO/*  sgNU */ = _sS/* sgNX */;
                      return __continue/* EXTERNAL */;
                    }else{
                      return new T2(1,new T(function(){
                        return B(_1L/* LudoJS.$wconvertCell */(_sL/* sgNM */, E(_sT/* sgO1 */.a), _bn/* LudoJS.Blue */));
                      }),new T(function(){
                        return B(_sN/* sgNT */(_sS/* sgNX */));
                      }));
                    }
                  }
                })(_sO/*  sgNU */));
                if(_sP/*  sgNT */!=__continue/* EXTERNAL */){
                  return _sP/*  sgNT */;
                }
              }
            };
            return B(_qt/* GHC.Integer.Type.geInteger */(B(_qZ/* sgJm */(B(_sN/* sgNT */(B(_bt/* LudoJS.$s!1 */(_sL/* sgNM */, E(_sK/* sgNK */).d)))))), _qB/* LudoJS.lvl11 */));
          });
          return new F(function(){return _s4/* sgNa */(_sJ/* sgNJ */, _sM/* sgOb */, _sK/* sgNK */, _/* EXTERNAL */);});
        }
      },
      _sU/* sgOL */ = B(_sH/* sgN9 */(_bo/* LudoJS.Green */, _hY/* LudoJS.lvl9 */, _qY/* sgJk */, _/* EXTERNAL */));
      return new F(function(){return _rm/* sgKl */(_/* EXTERNAL */, _sU/* sgOL */);});
      break;
    case 1:
      var _sV/* sgOO */ = B(_rc/* sgJS */(_qY/* sgJk */, _/* EXTERNAL */)),
      _sW/* sgOS */ = function(_sX/*  sgOX */, _sY/*  sgOY */, _sZ/*  sgOZ */, _/* EXTERNAL */){
        while(1){
          var _t0/*  sgOS */ = B((function(_t1/* sgOX */, _t2/* sgOY */, _t3/* sgOZ */, _/* EXTERNAL */){
            var _t4/* sgP1 */ = E(_t1/* sgOX */);
            if(!_t4/* sgP1 */._){
              return new T2(0,_t2/* sgOY */,_t3/* sgOZ */);
            }else{
              var _t5/* sgP4 */ = _t4/* sgP1 */.b,
              _t6/* sgP5 */ = E(_t4/* sgP1 */.a);
              if(_t6/* sgP5 */==1){
                var _t7/*   sgOY */ = _t2/* sgOY */,
                _t8/*   sgOZ */ = _t3/* sgOZ */;
                _sX/*  sgOX */ = _t5/* sgP4 */;
                _sY/*  sgOY */ = _t7/*   sgOY */;
                _sZ/*  sgOZ */ = _t8/*   sgOZ */;
                return __continue/* EXTERNAL */;
              }else{
                var _t9/* sgPv */ = new T(function(){
                  if(!E(_t2/* sgOY */)){
                    var _ta/* sgPd */ = function(_tb/*  sgPe */){
                      while(1){
                        var _tc/*  sgPd */ = B((function(_td/* sgPe */){
                          var _te/* sgPf */ = E(_td/* sgPe */);
                          if(!_te/* sgPf */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _tf/* sgPh */ = _te/* sgPf */.b,
                            _tg/* sgPl */ = E(E(_te/* sgPf */.a).b);
                            if(!_tg/* sgPl */._){
                              _tb/*  sgPe */ = _tf/* sgPh */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_1L/* LudoJS.$wconvertCell */(_t6/* sgP5 */, E(_tg/* sgPl */.a), _bo/* LudoJS.Green */));
                              }),new T(function(){
                                return B(_ta/* sgPd */(_tf/* sgPh */));
                              }));
                            }
                          }
                        })(_tb/*  sgPe */));
                        if(_tc/*  sgPd */!=__continue/* EXTERNAL */){
                          return _tc/*  sgPd */;
                        }
                      }
                    };
                    return B(_qt/* GHC.Integer.Type.geInteger */(B(_qZ/* sgJm */(B(_ta/* sgPd */(B(_bt/* LudoJS.$s!1 */(_t6/* sgP5 */, E(_t3/* sgOZ */).d)))))), _qB/* LudoJS.lvl11 */));
                  }else{
                    return true;
                  }
                }),
                _t8/*   sgOZ */ = _t3/* sgOZ */;
                _sX/*  sgOX */ = _t5/* sgP4 */;
                _sY/*  sgOY */ = _t9/* sgPv */;
                _sZ/*  sgOZ */ = _t8/*   sgOZ */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_sX/*  sgOX */, _sY/*  sgOY */, _sZ/*  sgOZ */, _/* EXTERNAL */));
          if(_t0/*  sgOS */!=__continue/* EXTERNAL */){
            return _t0/*  sgOS */;
          }
        }
      },
      _th/* sgPE */ = B((function(_ti/* sgOT */, _tj/* sgOU */, _tk/* sgOV */, _/* EXTERNAL */){
        return new F(function(){return _sW/* sgOS */(_ti/* sgOT */, _tj/* sgOU */, _tk/* sgOV */, _/* EXTERNAL */);});
      })(_hY/* LudoJS.lvl9 */, new T(function(){
        return E(E(_sV/* sgOO */).a);
      }), new T(function(){
        return E(E(_sV/* sgOO */).b);
      }), _/* EXTERNAL */));
      return new F(function(){return _rm/* sgKl */(_/* EXTERNAL */, _th/* sgPE */);});
      break;
    case 2:
      var _tl/* sgPH */ = B(_rc/* sgJS */(_qY/* sgJk */, _/* EXTERNAL */)),
      _tm/* sgPL */ = function(_tn/*  sgQi */, _to/*  sgQj */, _tp/*  sgQk */, _/* EXTERNAL */){
        while(1){
          var _tq/*  sgPL */ = B((function(_tr/* sgQi */, _ts/* sgQj */, _tt/* sgQk */, _/* EXTERNAL */){
            var _tu/* sgQm */ = E(_tr/* sgQi */);
            if(!_tu/* sgQm */._){
              return new T2(0,_ts/* sgQj */,_tt/* sgQk */);
            }else{
              var _tv/* sgQp */ = _tu/* sgQm */.b,
              _tw/* sgQq */ = E(_tu/* sgQm */.a);
              if(_tw/* sgQq */==2){
                var _tx/*   sgQj */ = _ts/* sgQj */,
                _ty/*   sgQk */ = _tt/* sgQk */;
                _tn/*  sgQi */ = _tv/* sgQp */;
                _to/*  sgQj */ = _tx/*   sgQj */;
                _tp/*  sgQk */ = _ty/*   sgQk */;
                return __continue/* EXTERNAL */;
              }else{
                var _tz/* sgQQ */ = new T(function(){
                  if(!E(_ts/* sgQj */)){
                    var _tA/* sgQy */ = function(_tB/*  sgQz */){
                      while(1){
                        var _tC/*  sgQy */ = B((function(_tD/* sgQz */){
                          var _tE/* sgQA */ = E(_tD/* sgQz */);
                          if(!_tE/* sgQA */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _tF/* sgQC */ = _tE/* sgQA */.b,
                            _tG/* sgQG */ = E(E(_tE/* sgQA */.a).b);
                            if(!_tG/* sgQG */._){
                              _tB/*  sgQz */ = _tF/* sgQC */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_1L/* LudoJS.$wconvertCell */(_tw/* sgQq */, E(_tG/* sgQG */.a), _bp/* LudoJS.Red */));
                              }),new T(function(){
                                return B(_tA/* sgQy */(_tF/* sgQC */));
                              }));
                            }
                          }
                        })(_tB/*  sgQz */));
                        if(_tC/*  sgQy */!=__continue/* EXTERNAL */){
                          return _tC/*  sgQy */;
                        }
                      }
                    };
                    return B(_qt/* GHC.Integer.Type.geInteger */(B(_qZ/* sgJm */(B(_tA/* sgQy */(B(_bt/* LudoJS.$s!1 */(_tw/* sgQq */, E(_tt/* sgQk */).d)))))), _qB/* LudoJS.lvl11 */));
                  }else{
                    return true;
                  }
                }),
                _ty/*   sgQk */ = _tt/* sgQk */;
                _tn/*  sgQi */ = _tv/* sgQp */;
                _to/*  sgQj */ = _tz/* sgQQ */;
                _tp/*  sgQk */ = _ty/*   sgQk */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_tn/*  sgQi */, _to/*  sgQj */, _tp/*  sgQk */, _/* EXTERNAL */));
          if(_tq/*  sgPL */!=__continue/* EXTERNAL */){
            return _tq/*  sgPL */;
          }
        }
      },
      _tH/* sgPK */ = function(_tI/* sgPM */, _tJ/* sgPN */, _tK/* sgPO */, _tL/* sgPP */, _/* EXTERNAL */){
        var _tM/* sgPR */ = E(_tI/* sgPM */);
        if(_tM/* sgPR */==2){
          return new F(function(){return _tm/* sgPL */(_tJ/* sgPN */, _tK/* sgPO */, _tL/* sgPP */, _/* EXTERNAL */);});
        }else{
          var _tN/* sgQh */ = new T(function(){
            if(!E(_tK/* sgPO */)){
              var _tO/* sgPZ */ = function(_tP/*  sgQ0 */){
                while(1){
                  var _tQ/*  sgPZ */ = B((function(_tR/* sgQ0 */){
                    var _tS/* sgQ1 */ = E(_tR/* sgQ0 */);
                    if(!_tS/* sgQ1 */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _tT/* sgQ3 */ = _tS/* sgQ1 */.b,
                      _tU/* sgQ7 */ = E(E(_tS/* sgQ1 */.a).b);
                      if(!_tU/* sgQ7 */._){
                        _tP/*  sgQ0 */ = _tT/* sgQ3 */;
                        return __continue/* EXTERNAL */;
                      }else{
                        return new T2(1,new T(function(){
                          return B(_1L/* LudoJS.$wconvertCell */(_tM/* sgPR */, E(_tU/* sgQ7 */.a), _bp/* LudoJS.Red */));
                        }),new T(function(){
                          return B(_tO/* sgPZ */(_tT/* sgQ3 */));
                        }));
                      }
                    }
                  })(_tP/*  sgQ0 */));
                  if(_tQ/*  sgPZ */!=__continue/* EXTERNAL */){
                    return _tQ/*  sgPZ */;
                  }
                }
              };
              return B(_qt/* GHC.Integer.Type.geInteger */(B(_qZ/* sgJm */(B(_tO/* sgPZ */(B(_bt/* LudoJS.$s!1 */(_tM/* sgPR */, E(_tL/* sgPP */).d)))))), _qB/* LudoJS.lvl11 */));
            }else{
              return true;
            }
          });
          return new F(function(){return _tm/* sgPL */(_tJ/* sgPN */, _tN/* sgQh */, _tL/* sgPP */, _/* EXTERNAL */);});
        }
      },
      _tV/* sgQZ */ = B(_tH/* sgPK */(_bo/* LudoJS.Green */, _hY/* LudoJS.lvl9 */, new T(function(){
        return E(E(_tl/* sgPH */).a);
      }), new T(function(){
        return E(E(_tl/* sgPH */).b);
      }), _/* EXTERNAL */));
      return new F(function(){return _rm/* sgKl */(_/* EXTERNAL */, _tV/* sgQZ */);});
      break;
    default:
      var _tW/* sgR2 */ = B(_rc/* sgJS */(_qY/* sgJk */, _/* EXTERNAL */)),
      _tX/* sgR6 */ = function(_tY/*  sgRD */, _tZ/*  sgRE */, _u0/*  sgRF */, _/* EXTERNAL */){
        while(1){
          var _u1/*  sgR6 */ = B((function(_u2/* sgRD */, _u3/* sgRE */, _u4/* sgRF */, _/* EXTERNAL */){
            var _u5/* sgRH */ = E(_u2/* sgRD */);
            if(!_u5/* sgRH */._){
              return new T2(0,_u3/* sgRE */,_u4/* sgRF */);
            }else{
              var _u6/* sgRK */ = _u5/* sgRH */.b,
              _u7/* sgRL */ = E(_u5/* sgRH */.a);
              if(_u7/* sgRL */==3){
                var _u8/*   sgRE */ = _u3/* sgRE */,
                _u9/*   sgRF */ = _u4/* sgRF */;
                _tY/*  sgRD */ = _u6/* sgRK */;
                _tZ/*  sgRE */ = _u8/*   sgRE */;
                _u0/*  sgRF */ = _u9/*   sgRF */;
                return __continue/* EXTERNAL */;
              }else{
                var _ua/* sgSb */ = new T(function(){
                  if(!E(_u3/* sgRE */)){
                    var _ub/* sgRT */ = function(_uc/*  sgRU */){
                      while(1){
                        var _ud/*  sgRT */ = B((function(_ue/* sgRU */){
                          var _uf/* sgRV */ = E(_ue/* sgRU */);
                          if(!_uf/* sgRV */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _ug/* sgRX */ = _uf/* sgRV */.b,
                            _uh/* sgS1 */ = E(E(_uf/* sgRV */.a).b);
                            if(!_uh/* sgS1 */._){
                              _uc/*  sgRU */ = _ug/* sgRX */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_1L/* LudoJS.$wconvertCell */(_u7/* sgRL */, E(_uh/* sgS1 */.a), _bq/* LudoJS.Yellow */));
                              }),new T(function(){
                                return B(_ub/* sgRT */(_ug/* sgRX */));
                              }));
                            }
                          }
                        })(_uc/*  sgRU */));
                        if(_ud/*  sgRT */!=__continue/* EXTERNAL */){
                          return _ud/*  sgRT */;
                        }
                      }
                    };
                    return B(_qt/* GHC.Integer.Type.geInteger */(B(_qZ/* sgJm */(B(_ub/* sgRT */(B(_bt/* LudoJS.$s!1 */(_u7/* sgRL */, E(_u4/* sgRF */).d)))))), _qB/* LudoJS.lvl11 */));
                  }else{
                    return true;
                  }
                }),
                _u9/*   sgRF */ = _u4/* sgRF */;
                _tY/*  sgRD */ = _u6/* sgRK */;
                _tZ/*  sgRE */ = _ua/* sgSb */;
                _u0/*  sgRF */ = _u9/*   sgRF */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_tY/*  sgRD */, _tZ/*  sgRE */, _u0/*  sgRF */, _/* EXTERNAL */));
          if(_u1/*  sgR6 */!=__continue/* EXTERNAL */){
            return _u1/*  sgR6 */;
          }
        }
      },
      _ui/* sgR5 */ = function(_uj/* sgR7 */, _uk/* sgR8 */, _ul/* sgR9 */, _um/* sgRa */, _/* EXTERNAL */){
        var _un/* sgRc */ = E(_uj/* sgR7 */);
        if(_un/* sgRc */==3){
          return new F(function(){return _tX/* sgR6 */(_uk/* sgR8 */, _ul/* sgR9 */, _um/* sgRa */, _/* EXTERNAL */);});
        }else{
          var _uo/* sgRC */ = new T(function(){
            if(!E(_ul/* sgR9 */)){
              var _up/* sgRk */ = function(_uq/*  sgRl */){
                while(1){
                  var _ur/*  sgRk */ = B((function(_us/* sgRl */){
                    var _ut/* sgRm */ = E(_us/* sgRl */);
                    if(!_ut/* sgRm */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _uu/* sgRo */ = _ut/* sgRm */.b,
                      _uv/* sgRs */ = E(E(_ut/* sgRm */.a).b);
                      if(!_uv/* sgRs */._){
                        _uq/*  sgRl */ = _uu/* sgRo */;
                        return __continue/* EXTERNAL */;
                      }else{
                        return new T2(1,new T(function(){
                          return B(_1L/* LudoJS.$wconvertCell */(_un/* sgRc */, E(_uv/* sgRs */.a), _bq/* LudoJS.Yellow */));
                        }),new T(function(){
                          return B(_up/* sgRk */(_uu/* sgRo */));
                        }));
                      }
                    }
                  })(_uq/*  sgRl */));
                  if(_ur/*  sgRk */!=__continue/* EXTERNAL */){
                    return _ur/*  sgRk */;
                  }
                }
              };
              return B(_qt/* GHC.Integer.Type.geInteger */(B(_qZ/* sgJm */(B(_up/* sgRk */(B(_bt/* LudoJS.$s!1 */(_un/* sgRc */, E(_um/* sgRa */).d)))))), _qB/* LudoJS.lvl11 */));
            }else{
              return true;
            }
          });
          return new F(function(){return _tX/* sgR6 */(_uk/* sgR8 */, _uo/* sgRC */, _um/* sgRa */, _/* EXTERNAL */);});
        }
      },
      _uw/* sgSk */ = B(_ui/* sgR5 */(_bo/* LudoJS.Green */, _hY/* LudoJS.lvl9 */, new T(function(){
        return E(E(_tW/* sgR2 */).a);
      }), new T(function(){
        return E(E(_tW/* sgR2 */).b);
      }), _/* EXTERNAL */));
      return new F(function(){return _rm/* sgKl */(_/* EXTERNAL */, _uw/* sgSk */);});
  }
},
_ux/* == */ = function(_uy/* scBK */){
  return E(E(_uy/* scBK */).a);
},
_uz/* elem */ = function(_uA/* sbGb */, _uB/* sbGc */, _uC/* sbGd */){
  while(1){
    var _uD/* sbGe */ = E(_uC/* sbGd */);
    if(!_uD/* sbGe */._){
      return false;
    }else{
      if(!B(A3(_ux/* GHC.Classes.== */,_uA/* sbGb */, _uB/* sbGc */, _uD/* sbGe */.a))){
        _uC/* sbGd */ = _uD/* sbGe */.b;
        continue;
      }else{
        return true;
      }
    }
  }
},
_uE/* findIndex */ = function(_uF/* s1ZJc */, _uG/* s1ZJd */){
  var _uH/* s1ZJe */ = function(_uI/*  s1ZJf */, _uJ/*  s1ZJg */){
    while(1){
      var _uK/*  s1ZJe */ = B((function(_uL/* s1ZJf */, _uM/* s1ZJg */){
        var _uN/* s1ZJh */ = E(_uL/* s1ZJf */);
        if(!_uN/* s1ZJh */._){
          return __Z/* EXTERNAL */;
        }else{
          var _uO/* s1ZJj */ = _uN/* s1ZJh */.b;
          if(!B(A1(_uF/* s1ZJc */,_uN/* s1ZJh */.a))){
            var _uP/*   s1ZJg */ = _uM/* s1ZJg */+1|0;
            _uI/*  s1ZJf */ = _uO/* s1ZJj */;
            _uJ/*  s1ZJg */ = _uP/*   s1ZJg */;
            return __continue/* EXTERNAL */;
          }else{
            return new T2(1,_uM/* s1ZJg */,new T(function(){
              return B(_uH/* s1ZJe */(_uO/* s1ZJj */, _uM/* s1ZJg */+1|0));
            }));
          }
        }
      })(_uI/*  s1ZJf */, _uJ/*  s1ZJg */));
      if(_uK/*  s1ZJe */!=__continue/* EXTERNAL */){
        return _uK/*  s1ZJe */;
      }
    }
  },
  _uQ/* s1ZJp */ = B(_uH/* s1ZJe */(_uG/* s1ZJd */, 0));
  return (_uQ/* s1ZJp */._==0) ? __Z/* EXTERNAL */ : new T1(1,_uQ/* s1ZJp */.a);
},
_uR/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Maybe.fromJust: Nothing"));
}),
_uS/* fromJust1 */ = new T(function(){
  return B(err/* EXTERNAL */(_uR/* Data.Maybe.lvl */));
}),
_uT/* a19 */ = 47,
_uU/* a20 */ = new T2(1,_uT/* LudoJS.a19 */,_4/* GHC.Types.[] */),
_uV/* a21 */ = 39,
_uW/* a22 */ = new T2(1,_uV/* LudoJS.a21 */,_uU/* LudoJS.a20 */),
_uX/* a23 */ = 34,
_uY/* a24 */ = new T2(1,_uX/* LudoJS.a23 */,_uW/* LudoJS.a22 */),
_uZ/* a25 */ = 26,
_v0/* a26 */ = new T2(1,_uZ/* LudoJS.a25 */,_uY/* LudoJS.a24 */),
_v1/* a27 */ = 21,
_v2/* a28 */ = new T2(1,_v1/* LudoJS.a27 */,_v0/* LudoJS.a26 */),
_v3/* a29 */ = 13,
_v4/* a30 */ = new T2(1,_v3/* LudoJS.a29 */,_v2/* LudoJS.a28 */),
_v5/* a31 */ = 8,
_v6/* globeCells */ = new T2(1,_v5/* LudoJS.a31 */,_v4/* LudoJS.a30 */),
_v7/* lvl3 */ = 56,
_v8/* lvl4 */ = new T2(1,_v7/* LudoJS.lvl3 */,_4/* GHC.Types.[] */),
_v9/* $fShowStage8 */ = 11,
_va/* a5 */ = 50,
_vb/* a6 */ = new T2(1,_va/* LudoJS.a5 */,_4/* GHC.Types.[] */),
_vc/* a7 */ = 44,
_vd/* a8 */ = new T2(1,_vc/* LudoJS.a7 */,_vb/* LudoJS.a6 */),
_ve/* a9 */ = 37,
_vf/* a10 */ = new T2(1,_ve/* LudoJS.a9 */,_vd/* LudoJS.a8 */),
_vg/* a11 */ = 31,
_vh/* a12 */ = new T2(1,_vg/* LudoJS.a11 */,_vf/* LudoJS.a10 */),
_vi/* a13 */ = 24,
_vj/* a14 */ = new T2(1,_vi/* LudoJS.a13 */,_vh/* LudoJS.a12 */),
_vk/* a15 */ = 18,
_vl/* a16 */ = new T2(1,_vk/* LudoJS.a15 */,_vj/* LudoJS.a14 */),
_vm/* a17 */ = new T2(1,_v9/* LudoJS.$fShowStage8 */,_vl/* LudoJS.a16 */),
_vn/* a18 */ = 5,
_vo/* starCells */ = new T2(1,_vn/* LudoJS.a18 */,_vm/* LudoJS.a17 */),
_vp/* lvl5 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_vo/* LudoJS.starCells */, _v8/* LudoJS.lvl4 */));
}),
_vq/* lvl6 */ = new T(function(){
  return B(_bB/* GHC.List.$wlenAcc */(_vo/* LudoJS.starCells */, 0));
}),
_vr/* $wa6 */ = function(_vs/* sgSn */, _vt/* sgSo */, _vu/* sgSp */, _/* EXTERNAL */){
  var _vv/* sgSr */ = new T(function(){
    return E(E(_vu/* sgSp */).b);
  }),
  _vw/* sgSy */ = new T(function(){
    return E(E(_vu/* sgSp */).d);
  }),
  _vx/* sgSF */ = E(_vt/* sgSo */);
  if(_vx/* sgSF */==56){
    var _vy/* sgX1 */ = new T(function(){
      var _vz/* sgWG */ = E(_vu/* sgSp */),
      _vA/* sgX0 */ = new T(function(){
        var _vB/* sgWZ */ = new T(function(){
          var _vC/* sgWM */ = B(_bt/* LudoJS.$s!1 */(_vv/* sgSr */, _vw/* sgSy */));
          if(!_vC/* sgWM */._){
            return __Z/* EXTERNAL */;
          }else{
            var _vD/* sgWO */ = _vC/* sgWM */.b,
            _vE/* sgWP */ = E(_vC/* sgWM */.a),
            _vF/* sgWS */ = E(_vs/* sgSn */);
            if(_vF/* sgWS */!=E(_vE/* sgWP */.a)){
              return new T2(1,_vE/* sgWP */,new T(function(){
                return B(_pB/* LudoJS.$sremoveFrom */(_vD/* sgWO */, _vF/* sgWS */));
              }));
            }else{
              return E(_vD/* sgWO */);
            }
          }
        });
        return B(_fi/* LudoJS.$sinsert_$sgo10 */(_vv/* sgSr */, _vB/* sgWZ */, _vw/* sgSy */));
      });
      return new T5(0,_vz/* sgWG */.a,_vz/* sgWG */.b,_vz/* sgWG */.c,_vA/* sgX0 */,_vz/* sgWG */.e);
    });
    return new T2(0,_2s/* GHC.Tuple.() */,_vy/* sgX1 */);
  }else{
    if(!B(_uz/* GHC.List.elem */(_pt/* GHC.Classes.$fEqInt */, _vx/* sgSF */, _vo/* LudoJS.starCells */))){
      if(!B(_uz/* GHC.List.elem */(_pt/* GHC.Classes.$fEqInt */, _vx/* sgSF */, _v6/* LudoJS.globeCells */))){
        if(_vx/* sgSF */<51){
          var _vG/* sgT9 */ = new T(function(){
            var _vH/* sgSL */ = E(_vu/* sgSp */),
            _vI/* sgT8 */ = new T(function(){
              var _vJ/* sgT6 */ = new T(function(){
                var _vK/* sgST */ = B(_bt/* LudoJS.$s!1 */(_vv/* sgSr */, _vw/* sgSy */));
                if(!_vK/* sgST */._){
                  return __Z/* EXTERNAL */;
                }else{
                  var _vL/* sgSV */ = _vK/* sgST */.b,
                  _vM/* sgSW */ = E(_vK/* sgST */.a),
                  _vN/* sgSZ */ = E(_vs/* sgSn */);
                  if(_vN/* sgSZ */!=E(_vM/* sgSW */.a)){
                    return new T2(1,_vM/* sgSW */,new T(function(){
                      return B(_pB/* LudoJS.$sremoveFrom */(_vL/* sgSV */, _vN/* sgSZ */));
                    }));
                  }else{
                    return E(_vL/* sgSV */);
                  }
                }
              });
              return B(_fi/* LudoJS.$sinsert_$sgo10 */(_vv/* sgSr */, new T2(1,new T2(0,_vs/* sgSn */,new T1(1,_vx/* sgSF */)),_vJ/* sgT6 */), _vw/* sgSy */));
            });
            return new T5(0,_vH/* sgSL */.a,_vH/* sgSL */.b,_vH/* sgSL */.c,_vI/* sgT8 */,_vH/* sgSL */.e);
          });
          return new F(function(){return _qV/* LudoJS.a43 */(_vv/* sgSr */, _vx/* sgSF */, _vG/* sgT9 */, _/* EXTERNAL */);});
        }else{
          var _vO/* sgTy */ = new T(function(){
            var _vP/* sgTa */ = E(_vu/* sgSp */),
            _vQ/* sgTx */ = new T(function(){
              var _vR/* sgTv */ = new T(function(){
                var _vS/* sgTi */ = B(_bt/* LudoJS.$s!1 */(_vv/* sgSr */, _vw/* sgSy */));
                if(!_vS/* sgTi */._){
                  return __Z/* EXTERNAL */;
                }else{
                  var _vT/* sgTk */ = _vS/* sgTi */.b,
                  _vU/* sgTl */ = E(_vS/* sgTi */.a),
                  _vV/* sgTo */ = E(_vs/* sgSn */);
                  if(_vV/* sgTo */!=E(_vU/* sgTl */.a)){
                    return new T2(1,_vU/* sgTl */,new T(function(){
                      return B(_pB/* LudoJS.$sremoveFrom */(_vT/* sgTk */, _vV/* sgTo */));
                    }));
                  }else{
                    return E(_vT/* sgTk */);
                  }
                }
              });
              return B(_fi/* LudoJS.$sinsert_$sgo10 */(_vv/* sgSr */, new T2(1,new T2(0,_vs/* sgSn */,new T1(1,_vx/* sgSF */)),_vR/* sgTv */), _vw/* sgSy */));
            });
            return new T5(0,_vP/* sgTa */.a,_vP/* sgTa */.b,_vP/* sgTa */.c,_vQ/* sgTx */,_vP/* sgTa */.e);
          });
          return new T2(0,_2s/* GHC.Tuple.() */,_vO/* sgTy */);
        }
      }else{
        var _vW/* sgTA */ = E(_vu/* sgSp */),
        _vX/* sgTB */ = _vW/* sgTA */.a,
        _vY/* sgTC */ = _vW/* sgTA */.b,
        _vZ/* sgTD */ = _vW/* sgTA */.c,
        _w0/* sgTF */ = _vW/* sgTA */.e,
        _w1/* sgTG */ = function(_w2/* sgTH */, _w3/* sgTI */, _w4/* sgTJ */, _w5/* sgTK */, _w6/* sgTL */, _w7/* sgTM */, _/* EXTERNAL */){
          var _w8/* sgTO */ = new T(function(){
            return B(_1L/* LudoJS.$wconvertCell */(_w4/* sgTJ */, _vx/* sgSF */, _w2/* sgTH */));
          }),
          _w9/* sgTQ */ = function(_wa/*  sgTR */){
            while(1){
              var _wb/*  sgTQ */ = B((function(_wc/* sgTR */){
                var _wd/* sgTS */ = E(_wc/* sgTR */);
                if(!_wd/* sgTS */._){
                  return false;
                }else{
                  var _we/* sgTU */ = _wd/* sgTS */.b,
                  _wf/* sgTY */ = E(E(_wd/* sgTS */.a).b);
                  if(!_wf/* sgTY */._){
                    _wa/*  sgTR */ = _we/* sgTU */;
                    return __continue/* EXTERNAL */;
                  }else{
                    var _wg/* sgU0 */ = E(_w8/* sgTO */);
                    if(_wg/* sgU0 */!=E(_wf/* sgTY */.a)){
                      var _wh/* sgU6 */ = function(_wi/* sgU7 */){
                        while(1){
                          var _wj/* sgU8 */ = E(_wi/* sgU7 */);
                          if(!_wj/* sgU8 */._){
                            return false;
                          }else{
                            var _wk/* sgUa */ = _wj/* sgU8 */.b,
                            _wl/* sgUe */ = E(E(_wj/* sgU8 */.a).b);
                            if(!_wl/* sgUe */._){
                              _wi/* sgU7 */ = _wk/* sgUa */;
                              continue;
                            }else{
                              if(_wg/* sgU0 */!=E(_wl/* sgUe */.a)){
                                _wi/* sgU7 */ = _wk/* sgUa */;
                                continue;
                              }else{
                                return true;
                              }
                            }
                          }
                        }
                      };
                      return new F(function(){return _wh/* sgU6 */(_we/* sgTU */);});
                    }else{
                      return true;
                    }
                  }
                }
              })(_wa/*  sgTR */));
              if(_wb/*  sgTQ */!=__continue/* EXTERNAL */){
                return _wb/*  sgTQ */;
              }
            }
          };
          if(!B(_w9/* sgTQ */(B(_bt/* LudoJS.$s!1 */(_w2/* sgTH */, _w6/* sgTL */))))){
            return new T2(0,_2s/* GHC.Tuple.() */,new T5(0,_w3/* sgTI */,_w4/* sgTJ */,_w5/* sgTK */,_w6/* sgTL */,_w7/* sgTM */));
          }else{
            var _wm/* sgUq */ = new T(function(){
              return B(_fi/* LudoJS.$sinsert_$sgo10 */(_w4/* sgTJ */, new T(function(){
                return B(_pu/* LudoJS.$soutByCell */(B(_bt/* LudoJS.$s!1 */(_w4/* sgTJ */, _w6/* sgTL */)), _vx/* sgSF */));
              }), _w6/* sgTL */));
            });
            return new T2(0,_2s/* GHC.Tuple.() */,new T5(0,_w3/* sgTI */,_w4/* sgTJ */,_w5/* sgTK */,_wm/* sgUq */,_w7/* sgTM */));
          }
        },
        _wn/* sgUt */ = function(_wo/* sgUu */, _wp/* sgUv */, _wq/* sgUw */, _wr/* sgUx */, _ws/* sgUy */, _/* EXTERNAL */){
          var _wt/* sgUA */ = function(_wu/* sgUB */, _wv/* sgUC */, _ww/* sgUD */, _wx/* sgUE */, _wy/* sgUF */, _/* EXTERNAL */){
            var _wz/* sgUH */ = E(_vv/* sgSr */);
            if(_wz/* sgUH */==3){
              return new F(function(){return _w1/* sgTG */(_bp/* LudoJS.Red */, _wu/* sgUB */, _wv/* sgUC */, _ww/* sgUD */, _wx/* sgUE */, _wy/* sgUF */, _/* EXTERNAL */);});
            }else{
              var _wA/* sgUI */ = B(_w1/* sgTG */(_bq/* LudoJS.Yellow */, _wu/* sgUB */, _wv/* sgUC */, _ww/* sgUD */, _wx/* sgUE */, _wy/* sgUF */, _/* EXTERNAL */));
              if(E(_wz/* sgUH */)==2){
                return new T2(0,_2s/* GHC.Tuple.() */,new T(function(){
                  return E(E(_wA/* sgUI */).b);
                }));
              }else{
                var _wB/* sgUP */ = E(E(_wA/* sgUI */).b);
                return new F(function(){return _w1/* sgTG */(_bp/* LudoJS.Red */, _wB/* sgUP */.a, _wB/* sgUP */.b, _wB/* sgUP */.c, _wB/* sgUP */.d, _wB/* sgUP */.e, _/* EXTERNAL */);});
              }
            }
          };
          if(E(_vv/* sgSr */)==1){
            return new F(function(){return _wt/* sgUA */(_wo/* sgUu */, _wp/* sgUv */, _wq/* sgUw */, _wr/* sgUx */, _ws/* sgUy */, _/* EXTERNAL */);});
          }else{
            var _wC/* sgV1 */ = B(_w1/* sgTG */(_bo/* LudoJS.Green */, _wo/* sgUu */, _wp/* sgUv */, _wq/* sgUw */, _wr/* sgUx */, _ws/* sgUy */, _/* EXTERNAL */)),
            _wD/* sgV7 */ = E(E(_wC/* sgV1 */).b);
            return new F(function(){return _wt/* sgUA */(_wD/* sgV7 */.a, _wD/* sgV7 */.b, _wD/* sgV7 */.c, _wD/* sgV7 */.d, _wD/* sgV7 */.e, _/* EXTERNAL */);});
          }
        },
        _wE/* sgVd */ = new T(function(){
          var _wF/* sgVt */ = new T(function(){
            var _wG/* sgVg */ = B(_bt/* LudoJS.$s!1 */(_vv/* sgSr */, _vw/* sgSy */));
            if(!_wG/* sgVg */._){
              return __Z/* EXTERNAL */;
            }else{
              var _wH/* sgVi */ = _wG/* sgVg */.b,
              _wI/* sgVj */ = E(_wG/* sgVg */.a),
              _wJ/* sgVm */ = E(_vs/* sgSn */);
              if(_wJ/* sgVm */!=E(_wI/* sgVj */.a)){
                return new T2(1,_wI/* sgVj */,new T(function(){
                  return B(_pB/* LudoJS.$sremoveFrom */(_wH/* sgVi */, _wJ/* sgVm */));
                }));
              }else{
                return E(_wH/* sgVi */);
              }
            }
          });
          return B(_fi/* LudoJS.$sinsert_$sgo10 */(_vv/* sgSr */, new T2(1,new T2(0,_vs/* sgSn */,new T1(1,_vx/* sgSF */)),_wF/* sgVt */), _vw/* sgSy */));
        });
        if(!E(_vv/* sgSr */)){
          return new F(function(){return _wn/* sgUt */(_vX/* sgTB */, _vY/* sgTC */, _vZ/* sgTD */, _wE/* sgVd */, _w0/* sgTF */, _/* EXTERNAL */);});
        }else{
          var _wK/* sgVw */ = B(_w1/* sgTG */(_bn/* LudoJS.Blue */, _vX/* sgTB */, _vY/* sgTC */, _vZ/* sgTD */, _wE/* sgVd */, _w0/* sgTF */, _/* EXTERNAL */)),
          _wL/* sgVC */ = E(E(_wK/* sgVw */).b);
          return new F(function(){return _wn/* sgUt */(_wL/* sgVC */.a, _wL/* sgVC */.b, _wL/* sgVC */.c, _wL/* sgVC */.d, _wL/* sgVC */.e, _/* EXTERNAL */);});
        }
      }
    }else{
      var _wM/* sgVI */ = new T(function(){
        var _wN/* sgVK */ = B(_uE/* Data.OldList.findIndex */(function(_wO/* B1 */){
          return new F(function(){return _b9/* GHC.Classes.eqInt */(_vx/* sgSF */, _wO/* B1 */);});
        }, _vo/* LudoJS.starCells */));
        if(!_wN/* sgVK */._){
          return E(_uS/* Data.Maybe.fromJust1 */);
        }else{
          return E(_wN/* sgVK */.a);
        }
      }),
      _wP/* sgVM */ = new T(function(){
        return B(_pT/* GHC.List.$w!! */(_vp/* LudoJS.lvl5 */, E(_wM/* sgVI */)+1|0));
      }),
      _wQ/* sgWy */ = new T(function(){
        var _wR/* sgVQ */ = E(_vu/* sgSp */),
        _wS/* sgWx */ = new T(function(){
          var _wT/* sgWw */ = new T(function(){
            if((E(_wM/* sgVI */)+1|0)!=E(_vq/* LudoJS.lvl6 */)){
              var _wU/* sgWi */ = new T(function(){
                var _wV/* sgW5 */ = B(_bt/* LudoJS.$s!1 */(_vv/* sgSr */, _vw/* sgSy */));
                if(!_wV/* sgW5 */._){
                  return __Z/* EXTERNAL */;
                }else{
                  var _wW/* sgW7 */ = _wV/* sgW5 */.b,
                  _wX/* sgW8 */ = E(_wV/* sgW5 */.a),
                  _wY/* sgWb */ = E(_vs/* sgSn */);
                  if(_wY/* sgWb */!=E(_wX/* sgW8 */.a)){
                    return new T2(1,_wX/* sgW8 */,new T(function(){
                      return B(_pB/* LudoJS.$sremoveFrom */(_wW/* sgW7 */, _wY/* sgWb */));
                    }));
                  }else{
                    return E(_wW/* sgW7 */);
                  }
                }
              });
              return new T2(1,new T2(0,_vs/* sgSn */,new T1(1,_wP/* sgVM */)),_wU/* sgWi */);
            }else{
              var _wZ/* sgWj */ = B(_bt/* LudoJS.$s!1 */(_vv/* sgSr */, _vw/* sgSy */));
              if(!_wZ/* sgWj */._){
                return __Z/* EXTERNAL */;
              }else{
                var _x0/* sgWl */ = _wZ/* sgWj */.b,
                _x1/* sgWm */ = E(_wZ/* sgWj */.a),
                _x2/* sgWp */ = E(_vs/* sgSn */);
                if(_x2/* sgWp */!=E(_x1/* sgWm */.a)){
                  return new T2(1,_x1/* sgWm */,new T(function(){
                    return B(_pB/* LudoJS.$sremoveFrom */(_x0/* sgWl */, _x2/* sgWp */));
                  }));
                }else{
                  return E(_x0/* sgWl */);
                }
              }
            }
          });
          return B(_fi/* LudoJS.$sinsert_$sgo10 */(_vv/* sgSr */, _wT/* sgWw */, _vw/* sgSy */));
        });
        return new T5(0,_wR/* sgVQ */.a,_wR/* sgVQ */.b,_wR/* sgVQ */.c,_wS/* sgWx */,_wR/* sgVQ */.e);
      }),
      _x3/* sgWz */ = B(_qV/* LudoJS.a43 */(_vv/* sgSr */, _vx/* sgSF */, _wQ/* sgWy */, _/* EXTERNAL */));
      return new F(function(){return _qV/* LudoJS.a43 */(_vv/* sgSr */, _wP/* sgVM */, new T(function(){
        return E(E(_x3/* sgWz */).b);
      }), _/* EXTERNAL */);});
    }
  }
},
_x4/* f2 */ = new T(function(){
  return eval/* EXTERNAL */("(() => gameState)");
}),
_x5/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */("piece does not exist"));
}),
_x6/* lvl1 */ = new T(function(){
  return B(err/* EXTERNAL */(_x5/* LudoJS.lvl */));
}),
_x7/* lvl10 */ = new T2(1,_bo/* LudoJS.Green */,_hY/* LudoJS.lvl9 */),
_x8/* lvl2 */ = "((x, y, player) => posToField(x, y, player))",
_x9/* lvl42 */ = function(_xa/* shuP */){
  var _xb/* shuQ */ = B(_5f/* System.Random.$w$crandomR12 */(_5c/* System.Random.Internal.$fRandomGenStdGen */, 1, 6, _xa/* shuP */));
  return new T2(0,E(_xb/* shuQ */.b),_xb/* shuQ */.a);
},
_xc/* lvl43 */ = function(_xd/* shvq */){
  var _xe/* shvr */ = B(_5f/* System.Random.$w$crandomR12 */(_5c/* System.Random.Internal.$fRandomGenStdGen */, 1, 4, _xd/* shvq */));
  return new T2(0,E(_xe/* shvr */.b),_xe/* shvr */.a);
},
_xf/* play9 */ = new T2(1,_bq/* LudoJS.Yellow */,_4/* GHC.Types.[] */),
_xg/* play8 */ = new T2(1,_bp/* LudoJS.Red */,_xf/* LudoJS.play9 */),
_xh/* play7 */ = new T2(1,_bo/* LudoJS.Green */,_xg/* LudoJS.play8 */),
_xi/* lvl16 */ = new T2(0,_hP/* LudoJS.lvl15 */,_hO/* LudoJS.Out */),
_xj/* lvl17 */ = new T2(1,_xi/* LudoJS.lvl16 */,_4/* GHC.Types.[] */),
_xk/* lvl18 */ = new T2(0,_b7/* LudoJS.play10 */,_hO/* LudoJS.Out */),
_xl/* lvl19 */ = new T2(1,_xk/* LudoJS.lvl18 */,_xj/* LudoJS.lvl17 */),
_xm/* lvl21 */ = new T2(0,_hS/* LudoJS.lvl20 */,_hO/* LudoJS.Out */),
_xn/* lvl22 */ = new T2(1,_xm/* LudoJS.lvl21 */,_xl/* LudoJS.lvl19 */),
_xo/* lvl23 */ = new T2(0,_oT/* LudoJS.lvl7 */,_hO/* LudoJS.Out */),
_xp/* lvl24 */ = new T2(1,_xo/* LudoJS.lvl23 */,_xn/* LudoJS.lvl22 */),
_xq/* go */ = function(_xr/* sgDM */){
  var _xs/* sgDN */ = E(_xr/* sgDM */);
  return (_xs/* sgDN */._==0) ? __Z/* EXTERNAL */ : new T2(1,new T2(0,_xs/* sgDN */.a,_xp/* LudoJS.lvl24 */),new T(function(){
    return B(_xq/* LudoJS.go */(_xs/* sgDN */.b));
  }));
},
_xt/* play_$sgo */ = function(_xu/* sgDI */, _xv/* sgDJ */){
  return new T2(1,new T2(0,_xu/* sgDI */,_xp/* LudoJS.lvl24 */),new T(function(){
    return B(_xq/* LudoJS.go */(_xv/* sgDJ */));
  }));
},
_xw/* play6 */ = new T(function(){
  return B(_xt/* LudoJS.play_$sgo */(_bn/* LudoJS.Blue */, _xh/* LudoJS.play7 */));
}),
_xx/* play5 */ = new T(function(){
  return B(_hy/* LudoJS.$sfromList */(_xw/* LudoJS.play6 */));
}),
_xy/* $fFractionalFixed1 */ = new T1(0,0),
_xz/* True */ = true,
_xA/* lvl */ = new T1(0,0),
_xB/* orInteger */ = function(_xC/* s1KS */, _xD/* s1KT */){
  while(1){
    var _xE/* s1KU */ = E(_xC/* s1KS */);
    if(!_xE/* s1KU */._){
      var _xF/* s1KV */ = _xE/* s1KU */.a,
      _xG/* s1KW */ = E(_xD/* s1KT */);
      if(!_xG/* s1KW */._){
        return new T1(0,(_xF/* s1KV */>>>0|_xG/* s1KW */.a>>>0)>>>0&4294967295);
      }else{
        _xC/* s1KS */ = new T1(1,I_fromInt/* EXTERNAL */(_xF/* s1KV */));
        _xD/* s1KT */ = _xG/* s1KW */;
        continue;
      }
    }else{
      var _xH/* s1L7 */ = E(_xD/* s1KT */);
      if(!_xH/* s1L7 */._){
        _xC/* s1KS */ = _xE/* s1KU */;
        _xD/* s1KT */ = new T1(1,I_fromInt/* EXTERNAL */(_xH/* s1L7 */.a));
        continue;
      }else{
        return new T1(1,I_or/* EXTERNAL */(_xE/* s1KU */.a, _xH/* s1L7 */.a));
      }
    }
  }
},
_xI/* shiftLInteger */ = function(_xJ/* s1Jk */, _xK/* s1Jl */){
  while(1){
    var _xL/* s1Jm */ = E(_xJ/* s1Jk */);
    if(!_xL/* s1Jm */._){
      _xJ/* s1Jk */ = new T1(1,I_fromInt/* EXTERNAL */(_xL/* s1Jm */.a));
      continue;
    }else{
      return new T1(1,I_shiftLeft/* EXTERNAL */(_xL/* s1Jm */.a, _xK/* s1Jl */));
    }
  }
},
_xM/* mkInteger_f */ = function(_xN/* s1S6 */){
  var _xO/* s1S7 */ = E(_xN/* s1S6 */);
  if(!_xO/* s1S7 */._){
    return E(_xA/* GHC.Integer.Type.lvl */);
  }else{
    return new F(function(){return _xB/* GHC.Integer.Type.orInteger */(new T1(0,E(_xO/* s1S7 */.a)), B(_xI/* GHC.Integer.Type.shiftLInteger */(B(_xM/* GHC.Integer.Type.mkInteger_f */(_xO/* s1S7 */.b)), 31)));});
  }
},
_xP/* log2I1 */ = new T1(0,1),
_xQ/* lvl2 */ = new T1(0,2147483647),
_xR/* lvl3 */ = new T(function(){
  return B(_qM/* GHC.Integer.Type.plusInteger */(_xQ/* GHC.Integer.Type.lvl2 */, _xP/* GHC.Integer.Type.log2I1 */));
}),
_xS/* negateInteger */ = function(_xT/* s1QH */){
  var _xU/* s1QI */ = E(_xT/* s1QH */);
  if(!_xU/* s1QI */._){
    var _xV/* s1QK */ = E(_xU/* s1QI */.a);
    return (_xV/* s1QK */==( -2147483648)) ? E(_xR/* GHC.Integer.Type.lvl3 */) : new T1(0, -_xV/* s1QK */);
  }else{
    return new T1(1,I_negate/* EXTERNAL */(_xU/* s1QI */.a));
  }
},
_xW/* mkInteger */ = function(_xX/* s1Sf */, _xY/* s1Sg */){
  if(!E(_xX/* s1Sf */)){
    return new F(function(){return _xS/* GHC.Integer.Type.negateInteger */(B(_xM/* GHC.Integer.Type.mkInteger_f */(_xY/* s1Sg */)));});
  }else{
    return new F(function(){return _xM/* GHC.Integer.Type.mkInteger_f */(_xY/* s1Sg */);});
  }
},
_xZ/* s6TCi */ = 1420103680,
_y0/* s6TCj */ = 465,
_y1/* s6TCk */ = new T2(1,_y0/* s6TCj */,_4/* GHC.Types.[] */),
_y2/* s6TCl */ = new T2(1,_xZ/* s6TCi */,_y1/* s6TCk */),
_y3/* $fHasResolutionE5 */ = new T(function(){
  return B(_xW/* GHC.Integer.Type.mkInteger */(_xz/* GHC.Types.True */, _y2/* s6TCl */));
}),
_y4/* $wa1 */ = function(_y5/* s3vU */, _/* EXTERNAL */){
  var _y6/* s3vZ */ = __get/* EXTERNAL */(_y5/* s3vU */, 0),
  _y7/* s3w5 */ = __get/* EXTERNAL */(_y5/* s3vU */, 1),
  _y8/* s3w9 */ = Number/* EXTERNAL */(_y6/* s3vZ */),
  _y9/* s3wd */ = jsTrunc/* EXTERNAL */(_y8/* s3w9 */),
  _ya/* s3wh */ = Number/* EXTERNAL */(_y7/* s3w5 */),
  _yb/* s3wl */ = jsTrunc/* EXTERNAL */(_ya/* s3wh */);
  return new T2(0,_y9/* s3wd */,_yb/* s3wl */);
},
_yc/* divInt# */ = function(_yd/* scDT */, _ye/* scDU */){
  if(_yd/* scDT */<=0){
    if(_yd/* scDT */>=0){
      return new F(function(){return quot/* EXTERNAL */(_yd/* scDT */, _ye/* scDU */);});
    }else{
      if(_ye/* scDU */<=0){
        return new F(function(){return quot/* EXTERNAL */(_yd/* scDT */, _ye/* scDU */);});
      }else{
        return quot/* EXTERNAL */(_yd/* scDT */+1|0, _ye/* scDU */)-1|0;
      }
    }
  }else{
    if(_ye/* scDU */>=0){
      if(_yd/* scDT */>=0){
        return new F(function(){return quot/* EXTERNAL */(_yd/* scDT */, _ye/* scDU */);});
      }else{
        if(_ye/* scDU */<=0){
          return new F(function(){return quot/* EXTERNAL */(_yd/* scDT */, _ye/* scDU */);});
        }else{
          return quot/* EXTERNAL */(_yd/* scDT */+1|0, _ye/* scDU */)-1|0;
        }
      }
    }else{
      return quot/* EXTERNAL */(_yd/* scDT */-1|0, _ye/* scDU */)-1|0;
    }
  }
},
_yf/* divInteger */ = function(_yg/* s1Nz */, _yh/* s1NA */){
  while(1){
    var _yi/* s1NB */ = E(_yg/* s1Nz */);
    if(!_yi/* s1NB */._){
      var _yj/* s1ND */ = E(_yi/* s1NB */.a);
      if(_yj/* s1ND */==( -2147483648)){
        _yg/* s1Nz */ = new T1(1,I_fromInt/* EXTERNAL */( -2147483648));
        continue;
      }else{
        var _yk/* s1NE */ = E(_yh/* s1NA */);
        if(!_yk/* s1NE */._){
          return new T1(0,B(_yc/* GHC.Classes.divInt# */(_yj/* s1ND */, _yk/* s1NE */.a)));
        }else{
          _yg/* s1Nz */ = new T1(1,I_fromInt/* EXTERNAL */(_yj/* s1ND */));
          _yh/* s1NA */ = _yk/* s1NE */;
          continue;
        }
      }
    }else{
      var _yl/* s1NO */ = _yi/* s1NB */.a,
      _ym/* s1NP */ = E(_yh/* s1NA */);
      return (_ym/* s1NP */._==0) ? new T1(1,I_div/* EXTERNAL */(_yl/* s1NO */, I_fromInt/* EXTERNAL */(_ym/* s1NP */.a))) : new T1(1,I_div/* EXTERNAL */(_yl/* s1NO */, _ym/* s1NP */.a));
    }
  }
},
_yn/* eqInteger */ = function(_yo/* s1Fo */, _yp/* s1Fp */){
  var _yq/* s1Fq */ = E(_yo/* s1Fo */);
  if(!_yq/* s1Fq */._){
    var _yr/* s1Fr */ = _yq/* s1Fq */.a,
    _ys/* s1Fs */ = E(_yp/* s1Fp */);
    return (_ys/* s1Fs */._==0) ? _yr/* s1Fr */==_ys/* s1Fs */.a : (I_compareInt/* EXTERNAL */(_ys/* s1Fs */.a, _yr/* s1Fr */)==0) ? true : false;
  }else{
    var _yt/* s1Fy */ = _yq/* s1Fq */.a,
    _yu/* s1Fz */ = E(_yp/* s1Fp */);
    return (_yu/* s1Fz */._==0) ? (I_compareInt/* EXTERNAL */(_yt/* s1Fy */, _yu/* s1Fz */.a)==0) ? true : false : (I_compare/* EXTERNAL */(_yt/* s1Fy */, _yu/* s1Fz */.a)==0) ? true : false;
  }
},
_yv/* getCTimeval_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function(){var ms = new Date().getTime();                   return [(ms/1000)|0, ((ms % 1000)*1000)|0];})");
}),
_yw/* slti */ = 660865024,
_yx/* sltj */ = 465661287,
_yy/* sltk */ = new T2(1,_yx/* sltj */,_4/* GHC.Types.[] */),
_yz/* sltl */ = new T2(1,_yw/* slti */,_yy/* sltk */),
_yA/* getPOSIXTime2 */ = new T(function(){
  return B(_xW/* GHC.Integer.Type.mkInteger */(_xz/* GHC.Types.True */, _yz/* sltl */));
}),
_yB/* smallInteger */ = function(_yC/* B1 */){
  return new T1(0,_yC/* B1 */);
},
_yD/* timesInteger */ = function(_yE/* s1PN */, _yF/* s1PO */){
  while(1){
    var _yG/* s1PP */ = E(_yE/* s1PN */);
    if(!_yG/* s1PP */._){
      var _yH/* s1PQ */ = _yG/* s1PP */.a,
      _yI/* s1PR */ = E(_yF/* s1PO */);
      if(!_yI/* s1PR */._){
        var _yJ/* s1PS */ = _yI/* s1PR */.a;
        if(!(imul/* EXTERNAL */(_yH/* s1PQ */, _yJ/* s1PS */)|0)){
          return new T1(0,imul/* EXTERNAL */(_yH/* s1PQ */, _yJ/* s1PS */)|0);
        }else{
          _yE/* s1PN */ = new T1(1,I_fromInt/* EXTERNAL */(_yH/* s1PQ */));
          _yF/* s1PO */ = new T1(1,I_fromInt/* EXTERNAL */(_yJ/* s1PS */));
          continue;
        }
      }else{
        _yE/* s1PN */ = new T1(1,I_fromInt/* EXTERNAL */(_yH/* s1PQ */));
        _yF/* s1PO */ = _yI/* s1PR */;
        continue;
      }
    }else{
      var _yK/* s1Q6 */ = E(_yF/* s1PO */);
      if(!_yK/* s1Q6 */._){
        _yE/* s1PN */ = _yG/* s1PP */;
        _yF/* s1PO */ = new T1(1,I_fromInt/* EXTERNAL */(_yK/* s1Q6 */.a));
        continue;
      }else{
        return new T1(1,I_mul/* EXTERNAL */(_yG/* s1PP */.a, _yK/* s1Q6 */.a));
      }
    }
  }
},
_yL/* getPOSIXTime1 */ = function(_/* EXTERNAL */){
  var _yM/* sltq */ = __app0/* EXTERNAL */(E(_yv/* Data.Time.Clock.CTimeval.getCTimeval_f1 */)),
  _yN/* sltt */ = B(_y4/* Data.Time.Clock.CTimeval.$wa1 */(_yM/* sltq */, _/* EXTERNAL */));
  return new T(function(){
    var _yO/* sltw */ = E(_yN/* sltt */);
    if(!B(_yn/* GHC.Integer.Type.eqInteger */(_yA/* Data.Time.Clock.POSIX.getPOSIXTime2 */, _xy/* Data.Fixed.$fFractionalFixed1 */))){
      return B(_qM/* GHC.Integer.Type.plusInteger */(B(_yD/* GHC.Integer.Type.timesInteger */(B(_yB/* GHC.Integer.Type.smallInteger */(E(_yO/* sltw */.a))), _y3/* Data.Fixed.$fHasResolutionE5 */)), B(_yf/* GHC.Integer.Type.divInteger */(B(_yD/* GHC.Integer.Type.timesInteger */(B(_yD/* GHC.Integer.Type.timesInteger */(B(_yB/* GHC.Integer.Type.smallInteger */(E(_yO/* sltw */.b))), _y3/* Data.Fixed.$fHasResolutionE5 */)), _y3/* Data.Fixed.$fHasResolutionE5 */)), _yA/* Data.Time.Clock.POSIX.getPOSIXTime2 */))));
    }else{
      return E(_3K/* GHC.Real.divZeroError */);
    }
  });
},
_yP/* $fBitsWord4 */ = 0,
_yQ/* $fBoundedWord32_$cmaxBound */ = 4294967295,
_yR/* $fBoundedWord32 */ = new T2(0,_yP/* GHC.Word.$fBitsWord4 */,_yQ/* GHC.Word.$fBoundedWord32_$cmaxBound */),
_yS/* $fEnumRatio1 */ = new T1(0,1),
_yT/* $p1Integral */ = function(_yU/* sv9T */){
  return E(E(_yU/* sv9T */).a);
},
_yV/* $p1Real */ = function(_yW/* svbu */){
  return E(E(_yW/* svbu */).a);
},
_yX/* fromInteger */ = function(_yY/* s6Go */){
  return E(E(_yY/* s6Go */).g);
},
_yZ/* gtInteger */ = function(_z0/* s1G1 */, _z1/* s1G2 */){
  var _z2/* s1G3 */ = E(_z0/* s1G1 */);
  if(!_z2/* s1G3 */._){
    var _z3/* s1G4 */ = _z2/* s1G3 */.a,
    _z4/* s1G5 */ = E(_z1/* s1G2 */);
    return (_z4/* s1G5 */._==0) ? _z3/* s1G4 */>_z4/* s1G5 */.a : I_compareInt/* EXTERNAL */(_z4/* s1G5 */.a, _z3/* s1G4 */)<0;
  }else{
    var _z5/* s1Gc */ = _z2/* s1G3 */.a,
    _z6/* s1Gd */ = E(_z1/* s1G2 */);
    return (_z6/* s1Gd */._==0) ? I_compareInt/* EXTERNAL */(_z5/* s1Gc */, _z6/* s1Gd */.a)>0 : I_compare/* EXTERNAL */(_z5/* s1Gc */, _z6/* s1Gd */.a)>0;
  }
},
_z7/* maxBound */ = function(_z8/* smih */){
  return E(E(_z8/* smih */).b);
},
_z9/* toInteger */ = function(_za/* svbj */){
  return E(E(_za/* svbj */).i);
},
_zb/* integralEnumFrom */ = function(_zc/* svrj */, _zd/* svrk */, _ze/* svrl */){
  var _zf/* svro */ = new T(function(){
    return B(_yX/* GHC.Num.fromInteger */(new T(function(){
      return B(_yV/* GHC.Real.$p1Real */(new T(function(){
        return B(_yT/* GHC.Real.$p1Integral */(_zc/* svrj */));
      })));
    })));
  }),
  _zg/* svrq */ = new T(function(){
    return B(_z7/* GHC.Enum.maxBound */(_zd/* svrk */));
  }),
  _zh/* svrr */ = function(_zi/* svrs */){
    return (!B(_yZ/* GHC.Integer.Type.gtInteger */(_zi/* svrs */, B(A2(_z9/* GHC.Real.toInteger */,_zc/* svrj */, _zg/* svrq */))))) ? new T2(1,new T(function(){
      return B(A1(_zf/* svro */,_zi/* svrs */));
    }),new T(function(){
      return B(_zh/* svrr */(B(_qM/* GHC.Integer.Type.plusInteger */(_zi/* svrs */, _yS/* GHC.Real.$fEnumRatio1 */))));
    })) : __Z/* EXTERNAL */;
  };
  return new F(function(){return _zh/* svrr */(B(A2(_z9/* GHC.Real.toInteger */,_zc/* svrj */, _ze/* svrl */)));});
},
_zj/* $fEnumWord32_$cenumFrom */ = function(_zk/* B1 */){
  return new F(function(){return _zb/* GHC.Real.integralEnumFrom */(_zl/* GHC.Word.$fIntegralWord32 */, _yR/* GHC.Word.$fBoundedWord32 */, _zk/* B1 */);});
},
_zm/* $fEnumInteger1 */ = new T1(0,0),
_zn/* ltInteger */ = function(_zo/* s1GH */, _zp/* s1GI */){
  var _zq/* s1GJ */ = E(_zo/* s1GH */);
  if(!_zq/* s1GJ */._){
    var _zr/* s1GK */ = _zq/* s1GJ */.a,
    _zs/* s1GL */ = E(_zp/* s1GI */);
    return (_zs/* s1GL */._==0) ? _zr/* s1GK */<_zs/* s1GL */.a : I_compareInt/* EXTERNAL */(_zs/* s1GL */.a, _zr/* s1GK */)>0;
  }else{
    var _zt/* s1GS */ = _zq/* s1GJ */.a,
    _zu/* s1GT */ = E(_zp/* s1GI */);
    return (_zu/* s1GT */._==0) ? I_compareInt/* EXTERNAL */(_zt/* s1GS */, _zu/* s1GT */.a)<0 : I_compare/* EXTERNAL */(_zt/* s1GS */, _zu/* s1GT */.a)<0;
  }
},
_zv/* up_fb */ = function(_zw/* smjD */, _zx/* smjE */, _zy/* smjF */, _zz/* smjG */, _zA/* smjH */){
  var _zB/* smjI */ = function(_zC/* smjJ */){
    if(!B(_yZ/* GHC.Integer.Type.gtInteger */(_zC/* smjJ */, _zA/* smjH */))){
      return new F(function(){return A2(_zw/* smjD */,_zC/* smjJ */, new T(function(){
        return B(_zB/* smjI */(B(_qM/* GHC.Integer.Type.plusInteger */(_zC/* smjJ */, _zz/* smjG */))));
      }));});
    }else{
      return E(_zx/* smjE */);
    }
  };
  return new F(function(){return _zB/* smjI */(_zy/* smjF */);});
},
_zD/* enumDeltaToIntegerFB */ = function(_zE/* smFL */, _zF/* smFM */, _zG/* smFN */, _zH/* smFO */, _zI/* smFP */){
  if(!B(_qt/* GHC.Integer.Type.geInteger */(_zH/* smFO */, _zm/* GHC.Enum.$fEnumInteger1 */))){
    var _zJ/* smFR */ = function(_zK/* smFS */){
      if(!B(_zn/* GHC.Integer.Type.ltInteger */(_zK/* smFS */, _zI/* smFP */))){
        return new F(function(){return A2(_zE/* smFL */,_zK/* smFS */, new T(function(){
          return B(_zJ/* smFR */(B(_qM/* GHC.Integer.Type.plusInteger */(_zK/* smFS */, _zH/* smFO */))));
        }));});
      }else{
        return E(_zF/* smFM */);
      }
    };
    return new F(function(){return _zJ/* smFR */(_zG/* smFN */);});
  }else{
    return new F(function(){return _zv/* GHC.Enum.up_fb */(_zE/* smFL */, _zF/* smFM */, _zG/* smFN */, _zH/* smFO */, _zI/* smFP */);});
  }
},
_zL/* minBound */ = function(_zM/* smid */){
  return E(E(_zM/* smid */).a);
},
_zN/* minusInteger */ = function(_zO/* s1P0 */, _zP/* s1P1 */){
  while(1){
    var _zQ/* s1P2 */ = E(_zO/* s1P0 */);
    if(!_zQ/* s1P2 */._){
      var _zR/* s1P3 */ = _zQ/* s1P2 */.a,
      _zS/* s1P4 */ = E(_zP/* s1P1 */);
      if(!_zS/* s1P4 */._){
        var _zT/* s1P5 */ = _zS/* s1P4 */.a,
        _zU/* s1P6 */ = subC/* EXTERNAL */(_zR/* s1P3 */, _zT/* s1P5 */);
        if(!E(_zU/* s1P6 */.b)){
          return new T1(0,_zU/* s1P6 */.a);
        }else{
          _zO/* s1P0 */ = new T1(1,I_fromInt/* EXTERNAL */(_zR/* s1P3 */));
          _zP/* s1P1 */ = new T1(1,I_fromInt/* EXTERNAL */(_zT/* s1P5 */));
          continue;
        }
      }else{
        _zO/* s1P0 */ = new T1(1,I_fromInt/* EXTERNAL */(_zR/* s1P3 */));
        _zP/* s1P1 */ = _zS/* s1P4 */;
        continue;
      }
    }else{
      var _zV/* s1Pl */ = E(_zP/* s1P1 */);
      if(!_zV/* s1Pl */._){
        _zO/* s1P0 */ = _zQ/* s1P2 */;
        _zP/* s1P1 */ = new T1(1,I_fromInt/* EXTERNAL */(_zV/* s1Pl */.a));
        continue;
      }else{
        return new T1(1,I_sub/* EXTERNAL */(_zQ/* s1P2 */.a, _zV/* s1Pl */.a));
      }
    }
  }
},
_zW/* integralEnumFromThen */ = function(_zX/* svry */, _zY/* svrz */, _zZ/* svrA */, _A0/* svrB */){
  var _A1/* svrC */ = B(A2(_z9/* GHC.Real.toInteger */,_zX/* svry */, _A0/* svrB */)),
  _A2/* svrD */ = B(A2(_z9/* GHC.Real.toInteger */,_zX/* svry */, _zZ/* svrA */));
  if(!B(_qt/* GHC.Integer.Type.geInteger */(_A1/* svrC */, _A2/* svrD */))){
    var _A3/* svrH */ = new T(function(){
      return B(_yX/* GHC.Num.fromInteger */(new T(function(){
        return B(_yV/* GHC.Real.$p1Real */(new T(function(){
          return B(_yT/* GHC.Real.$p1Integral */(_zX/* svry */));
        })));
      })));
    }),
    _A4/* svrL */ = function(_A5/* svrI */, _A6/* svrJ */){
      return new T2(1,new T(function(){
        return B(A1(_A3/* svrH */,_A5/* svrI */));
      }),_A6/* svrJ */);
    };
    return new F(function(){return _zD/* GHC.Enum.enumDeltaToIntegerFB */(_A4/* svrL */, _4/* GHC.Types.[] */, _A2/* svrD */, B(_zN/* GHC.Integer.Type.minusInteger */(_A1/* svrC */, _A2/* svrD */)), B(A2(_z9/* GHC.Real.toInteger */,_zX/* svry */, new T(function(){
      return B(_zL/* GHC.Enum.minBound */(_zY/* svrz */));
    }))));});
  }else{
    var _A7/* svrR */ = new T(function(){
      return B(_yX/* GHC.Num.fromInteger */(new T(function(){
        return B(_yV/* GHC.Real.$p1Real */(new T(function(){
          return B(_yT/* GHC.Real.$p1Integral */(_zX/* svry */));
        })));
      })));
    }),
    _A8/* svrV */ = function(_A9/* svrS */, _Aa/* svrT */){
      return new T2(1,new T(function(){
        return B(A1(_A7/* svrR */,_A9/* svrS */));
      }),_Aa/* svrT */);
    };
    return new F(function(){return _zD/* GHC.Enum.enumDeltaToIntegerFB */(_A8/* svrV */, _4/* GHC.Types.[] */, _A2/* svrD */, B(_zN/* GHC.Integer.Type.minusInteger */(_A1/* svrC */, _A2/* svrD */)), B(A2(_z9/* GHC.Real.toInteger */,_zX/* svry */, new T(function(){
      return B(_z7/* GHC.Enum.maxBound */(_zY/* svrz */));
    }))));});
  }
},
_Ab/* $fEnumWord32_$cenumFromThen */ = function(_Ac/* B2 */, _zk/* B1 */){
  return new F(function(){return _zW/* GHC.Real.integralEnumFromThen */(_zl/* GHC.Word.$fIntegralWord32 */, _yR/* GHC.Word.$fBoundedWord32 */, _Ac/* B2 */, _zk/* B1 */);});
},
_Ad/* integralEnumFromThenTo */ = function(_Ae/* svsd */, _Af/* svse */, _Ag/* svsf */, _Ah/* svsg */){
  var _Ai/* svsh */ = B(A2(_z9/* GHC.Real.toInteger */,_Ae/* svsd */, _Af/* svse */)),
  _Aj/* svsk */ = new T(function(){
    return B(_yX/* GHC.Num.fromInteger */(new T(function(){
      return B(_yV/* GHC.Real.$p1Real */(new T(function(){
        return B(_yT/* GHC.Real.$p1Integral */(_Ae/* svsd */));
      })));
    })));
  }),
  _Ak/* svso */ = function(_Al/* svsl */, _Am/* svsm */){
    return new T2(1,new T(function(){
      return B(A1(_Aj/* svsk */,_Al/* svsl */));
    }),_Am/* svsm */);
  };
  return new F(function(){return _zD/* GHC.Enum.enumDeltaToIntegerFB */(_Ak/* svso */, _4/* GHC.Types.[] */, _Ai/* svsh */, B(_zN/* GHC.Integer.Type.minusInteger */(B(A2(_z9/* GHC.Real.toInteger */,_Ae/* svsd */, _Ag/* svsf */)), _Ai/* svsh */)), B(A2(_z9/* GHC.Real.toInteger */,_Ae/* svsd */, _Ah/* svsg */)));});
},
_An/* $fEnumWord32_$cenumFromThenTo */ = function(_Ao/* B3 */, _Ac/* B2 */, _zk/* B1 */){
  return new F(function(){return _Ad/* GHC.Real.integralEnumFromThenTo */(_zl/* GHC.Word.$fIntegralWord32 */, _Ao/* B3 */, _Ac/* B2 */, _zk/* B1 */);});
},
_Ap/* integralEnumFromTo */ = function(_Aq/* svrZ */, _Ar/* svs0 */, _As/* svs1 */){
  var _At/* svs4 */ = new T(function(){
    return B(_yX/* GHC.Num.fromInteger */(new T(function(){
      return B(_yV/* GHC.Real.$p1Real */(new T(function(){
        return B(_yT/* GHC.Real.$p1Integral */(_Aq/* svrZ */));
      })));
    })));
  }),
  _Au/* svs6 */ = function(_Av/* svs7 */){
    return (!B(_yZ/* GHC.Integer.Type.gtInteger */(_Av/* svs7 */, B(A2(_z9/* GHC.Real.toInteger */,_Aq/* svrZ */, _As/* svs1 */))))) ? new T2(1,new T(function(){
      return B(A1(_At/* svs4 */,_Av/* svs7 */));
    }),new T(function(){
      return B(_Au/* svs6 */(B(_qM/* GHC.Integer.Type.plusInteger */(_Av/* svs7 */, _yS/* GHC.Real.$fEnumRatio1 */))));
    })) : __Z/* EXTERNAL */;
  };
  return new F(function(){return _Au/* svs6 */(B(A2(_z9/* GHC.Real.toInteger */,_Aq/* svrZ */, _Ar/* svs0 */)));});
},
_Aw/* $fEnumWord32_$cenumFromTo */ = function(_Ac/* B2 */, _zk/* B1 */){
  return new F(function(){return _Ap/* GHC.Real.integralEnumFromTo */(_zl/* GHC.Word.$fIntegralWord32 */, _Ac/* B2 */, _zk/* B1 */);});
},
_Ax/* wordToInteger */ = function(_Ay/* s1J7 */){
  return new T1(1,I_fromInt/* EXTERNAL */(_Ay/* s1J7 */));
},
_Az/* $fIntegralWord32_$ctoInteger */ = function(_AA/* s1RpU */){
  var _AB/* s1RpV */ = E(_AA/* s1RpU */),
  _AC/* s1RpX */ = _AB/* s1RpV */&4294967295;
  if(_AC/* s1RpX */<0){
    return new F(function(){return _Ax/* GHC.Integer.Type.wordToInteger */(_AB/* s1RpV */);});
  }else{
    return new F(function(){return _yB/* GHC.Integer.Type.smallInteger */(_AC/* s1RpX */);});
  }
},
_AD/* integerToJSString */ = function(_AE/* s1Ii */){
  while(1){
    var _AF/* s1Ij */ = E(_AE/* s1Ii */);
    if(!_AF/* s1Ij */._){
      _AE/* s1Ii */ = new T1(1,I_fromInt/* EXTERNAL */(_AF/* s1Ij */.a));
      continue;
    }else{
      return new F(function(){return I_toString/* EXTERNAL */(_AF/* s1Ij */.a);});
    }
  }
},
_AG/* integerToString */ = function(_AH/* sf6p */, _AI/* sf6q */){
  return new F(function(){return _q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(B(_AD/* GHC.Integer.Type.integerToJSString */(_AH/* sf6p */))), _AI/* sf6q */);});
},
_AJ/* shows9 */ = new T1(0,0),
_AK/* $w$cshowsPrec1 */ = function(_AL/* sf7E */, _AM/* sf7F */, _AN/* sf7G */){
  if(_AL/* sf7E */<=6){
    return new F(function(){return _AG/* GHC.Show.integerToString */(_AM/* sf7F */, _AN/* sf7G */);});
  }else{
    if(!B(_zn/* GHC.Integer.Type.ltInteger */(_AM/* sf7F */, _AJ/* GHC.Show.shows9 */))){
      return new F(function(){return _AG/* GHC.Show.integerToString */(_AM/* sf7F */, _AN/* sf7G */);});
    }else{
      return new T2(1,_5K/* GHC.Show.shows8 */,new T(function(){
        return B(_q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(B(_AD/* GHC.Integer.Type.integerToJSString */(_AM/* sf7F */))), new T2(1,_5J/* GHC.Show.shows7 */,_AN/* sf7G */)));
      }));
    }
  }
},
_AO/* $fShowWord32_$cshow */ = function(_AP/* s1RrO */){
  return new F(function(){return _AK/* GHC.Show.$w$cshowsPrec1 */(0, B(_Az/* GHC.Word.$fIntegralWord32_$ctoInteger */(_AP/* s1RrO */)), _4/* GHC.Types.[] */);});
},
_AQ/* $fShowWord2 */ = function(_AR/* s1RrD */, _AS/* s1RrE */){
  var _AT/* s1RrF */ = E(_AR/* s1RrD */),
  _AU/* s1RrH */ = _AT/* s1RrF */&4294967295;
  if(_AU/* s1RrH */<0){
    return new F(function(){return _AK/* GHC.Show.$w$cshowsPrec1 */(0, B(_Ax/* GHC.Integer.Type.wordToInteger */(_AT/* s1RrF */)), _AS/* s1RrE */);});
  }else{
    return new F(function(){return _AK/* GHC.Show.$w$cshowsPrec1 */(0, B(_yB/* GHC.Integer.Type.smallInteger */(_AU/* s1RrH */)), _AS/* s1RrE */);});
  }
},
_AV/* $fShowWord32_$cshowList */ = function(_AW/* s1RrM */, _AX/* s1RrN */){
  return new F(function(){return _A/* GHC.Show.showList__ */(_AQ/* GHC.Word.$fShowWord2 */, _AW/* s1RrM */, _AX/* s1RrN */);});
},
_AY/* $fShowWord32_$cshowsPrec */ = function(_AZ/* s1Rrr */, _B0/* s1Rrs */){
  var _B1/* s1Rrt */ = new T(function(){
    var _B2/* s1Rru */ = E(_B0/* s1Rrs */),
    _B3/* s1Rrw */ = _B2/* s1Rru */&4294967295;
    if(_B3/* s1Rrw */<0){
      return B(_Ax/* GHC.Integer.Type.wordToInteger */(_B2/* s1Rru */));
    }else{
      return B(_yB/* GHC.Integer.Type.smallInteger */(_B3/* s1Rrw */));
    }
  });
  return function(_B4/* s1Rrz */){
    return new F(function(){return _AK/* GHC.Show.$w$cshowsPrec1 */(E(_AZ/* s1Rrr */), _B1/* s1Rrt */, _B4/* s1Rrz */);});
  };
},
_B5/* $fShowWord32 */ = new T3(0,_AY/* GHC.Word.$fShowWord32_$cshowsPrec */,_AO/* GHC.Word.$fShowWord32_$cshow */,_AV/* GHC.Word.$fShowWord32_$cshowList */),
_B6/* lvl */ = new T2(1,_5J/* GHC.Show.shows7 */,_4/* GHC.Types.[] */),
_B7/* $fShow(,)1 */ = function(_B8/* sfbb */, _B9/* sfbc */, _Ba/* sfbd */){
  return new F(function(){return A1(_B8/* sfbb */,new T2(1,_x/* GHC.Show.showList__1 */,new T(function(){
    return B(A1(_B9/* sfbc */,_Ba/* sfbd */));
  })));});
},
_Bb/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */(": empty list"));
}),
_Bc/* errorEmptyList */ = function(_Bd/* sbDG */){
  return new F(function(){return err/* EXTERNAL */(B(_q/* GHC.Base.++ */(_pI/* GHC.List.prel_list_str */, new T(function(){
    return B(_q/* GHC.Base.++ */(_Bd/* sbDG */, _Bb/* GHC.List.lvl */));
  },1))));});
},
_Be/* lvl7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("foldr1"));
}),
_Bf/* lvl8 */ = new T(function(){
  return B(_Bc/* GHC.List.errorEmptyList */(_Be/* GHC.List.lvl7 */));
}),
_Bg/* foldr1 */ = function(_Bh/* sbKQ */, _Bi/* sbKR */){
  var _Bj/* sbKS */ = E(_Bi/* sbKR */);
  if(!_Bj/* sbKS */._){
    return E(_Bf/* GHC.List.lvl8 */);
  }else{
    var _Bk/* sbKT */ = _Bj/* sbKS */.a,
    _Bl/* sbKV */ = E(_Bj/* sbKS */.b);
    if(!_Bl/* sbKV */._){
      return E(_Bk/* sbKT */);
    }else{
      return new F(function(){return A2(_Bh/* sbKQ */,_Bk/* sbKT */, new T(function(){
        return B(_Bg/* GHC.List.foldr1 */(_Bh/* sbKQ */, _Bl/* sbKV */));
      }));});
    }
  }
},
_Bm/* lvl14 */ = function(_Bn/* smzT */){
  return new F(function(){return _5L/* GHC.Show.$wshowSignedInt */(0,  -2147483648, _Bn/* smzT */);});
},
_Bo/* lvl15 */ = function(_Bp/* smzU */){
  return new F(function(){return _5L/* GHC.Show.$wshowSignedInt */(0, 2147483647, _Bp/* smzU */);});
},
_Bq/* lvl16 */ = new T2(1,_Bo/* GHC.Enum.lvl15 */,_4/* GHC.Types.[] */),
_Br/* lvl17 */ = new T2(1,_Bm/* GHC.Enum.lvl14 */,_Bq/* GHC.Enum.lvl16 */),
_Bs/* lvl18 */ = new T(function(){
  return B(_Bg/* GHC.List.foldr1 */(_B7/* GHC.Show.$fShow(,)1 */, _Br/* GHC.Enum.lvl17 */));
}),
_Bt/* lvl19 */ = new T(function(){
  return B(A1(_Bs/* GHC.Enum.lvl18 */,_B6/* GHC.Enum.lvl */));
}),
_Bu/* lvl20 */ = new T2(1,_5K/* GHC.Show.shows8 */,_Bt/* GHC.Enum.lvl19 */),
_Bv/* lvl21 */ = new T(function(){
  return B(unAppCStr/* EXTERNAL */(") is outside of Int\'s bounds ", _Bu/* GHC.Enum.lvl20 */));
}),
_Bw/* show */ = function(_Bx/* sf6a */){
  return E(E(_Bx/* sf6a */).b);
},
_By/* lvl22 */ = function(_Bz/* smzV */, _BA/* smzW */, _BB/* smzX */){
  var _BC/* smA1 */ = new T(function(){
    var _BD/* smA0 */ = new T(function(){
      return B(unAppCStr/* EXTERNAL */("}: value (", new T(function(){
        return B(_q/* GHC.Base.++ */(B(A2(_Bw/* GHC.Show.show */,_BB/* smzX */, _BA/* smzW */)), _Bv/* GHC.Enum.lvl21 */));
      })));
    },1);
    return B(_q/* GHC.Base.++ */(_Bz/* smzV */, _BD/* smA0 */));
  });
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("Enum.fromEnum{", _BC/* smA1 */)));});
},
_BE/* fromEnumError */ = function(_BF/* smA3 */, _BG/* smA4 */, _BH/* smA5 */){
  return new F(function(){return _By/* GHC.Enum.lvl22 */(_BG/* smA4 */, _BH/* smA5 */, _BF/* smA3 */);});
},
_BI/* lvl4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Word32"));
}),
_BJ/* lvl10 */ = function(_BK/* s1RrQ */){
  return new F(function(){return _BE/* GHC.Enum.fromEnumError */(_B5/* GHC.Word.$fShowWord32 */, _BI/* GHC.Word.lvl4 */, _BK/* s1RrQ */);});
},
_BL/* $fEnumWord32_$cfromEnum */ = function(_BM/* s1RMa */){
  var _BN/* s1RMb */ = E(_BM/* s1RMa */);
  if(_BN/* s1RMb */>2147483647){
    return new F(function(){return _BJ/* GHC.Word.lvl10 */(_BN/* s1RMb */);});
  }else{
    return _BN/* s1RMb */&4294967295;
  }
},
_BO/* lvl1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("}: tried to take `pred\' of minBound"));
}),
_BP/* lvl2 */ = function(_BQ/* smnl */){
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("Enum.pred{", new T(function(){
    return B(_q/* GHC.Base.++ */(_BQ/* smnl */, _BO/* GHC.Enum.lvl1 */));
  }))));});
},
_BR/* predError */ = function(_BS/* B1 */){
  return new F(function(){return _BP/* GHC.Enum.lvl2 */(_BS/* B1 */);});
},
_BT/* $fEnumWord7 */ = new T(function(){
  return B(_BR/* GHC.Enum.predError */(_BI/* GHC.Word.lvl4 */));
}),
_BU/* $fEnumWord32_$cpred */ = function(_BV/* s1RNu */){
  var _BW/* s1RNx */ = E(_BV/* s1RNu */);
  return (_BW/* s1RNx */==0) ? E(_BT/* GHC.Word.$fEnumWord7 */) : _BW/* s1RNx */-1>>>0;
},
_BX/* lvl3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("}: tried to take `succ\' of maxBound"));
}),
_BY/* lvl4 */ = function(_BZ/* smno */){
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("Enum.succ{", new T(function(){
    return B(_q/* GHC.Base.++ */(_BZ/* smno */, _BX/* GHC.Enum.lvl3 */));
  }))));});
},
_C0/* succError */ = function(_BS/* B1 */){
  return new F(function(){return _BY/* GHC.Enum.lvl4 */(_BS/* B1 */);});
},
_C1/* $fEnumWord9 */ = new T(function(){
  return B(_C0/* GHC.Enum.succError */(_BI/* GHC.Word.lvl4 */));
}),
_C2/* $fEnumWord32_$csucc */ = function(_C3/* s1RNp */){
  var _C4/* s1RNs */ = E(_C3/* s1RNp */);
  return (_C4/* s1RNs */==4294967295) ? E(_C1/* GHC.Word.$fEnumWord9 */) : _C4/* s1RNs */+1>>>0;
},
_C5/* lvl12 */ = new T2(0,_yP/* GHC.Word.$fBitsWord4 */,_yQ/* GHC.Word.$fBoundedWord32_$cmaxBound */),
_C6/* shows14 */ = 0,
_C7/* showsPrec */ = function(_C8/* sf65 */){
  return E(E(_C8/* sf65 */).a);
},
_C9/* lvl5 */ = function(_Ca/* smnr */, _Cb/* smns */, _Cc/* smnt */, _Cd/* smnu */){
  var _Ce/* smnK */ = new T(function(){
    var _Cf/* smnJ */ = new T(function(){
      var _Cg/* smnI */ = new T(function(){
        var _Ch/* smnH */ = new T(function(){
          var _Ci/* smnG */ = new T(function(){
            var _Cj/* smny */ = E(_Cc/* smnt */),
            _Ck/* smnF */ = new T(function(){
              return B(A3(_Bg/* GHC.List.foldr1 */,_B7/* GHC.Show.$fShow(,)1 */, new T2(1,new T(function(){
                return B(A3(_C7/* GHC.Show.showsPrec */,_Cd/* smnu */, _C6/* GHC.Show.shows14 */, _Cj/* smny */.a));
              }),new T2(1,new T(function(){
                return B(A3(_C7/* GHC.Show.showsPrec */,_Cd/* smnu */, _C6/* GHC.Show.shows14 */, _Cj/* smny */.b));
              }),_4/* GHC.Types.[] */)), _B6/* GHC.Enum.lvl */));
            });
            return new T2(1,_5K/* GHC.Show.shows8 */,_Ck/* smnF */);
          });
          return B(unAppCStr/* EXTERNAL */(") is outside of bounds ", _Ci/* smnG */));
        },1);
        return B(_q/* GHC.Base.++ */(B(_5L/* GHC.Show.$wshowSignedInt */(0, E(_Cb/* smns */), _4/* GHC.Types.[] */)), _Ch/* smnH */));
      });
      return B(unAppCStr/* EXTERNAL */("}: tag (", _Cg/* smnI */));
    },1);
    return B(_q/* GHC.Base.++ */(_Ca/* smnr */, _Cf/* smnJ */));
  });
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("Enum.toEnum{", _Ce/* smnK */)));});
},
_Cl/* toEnumError */ = function(_Cm/* smnM */, _Cn/* smnN */, _Co/* smnO */, _Cp/* smnP */){
  return new F(function(){return _C9/* GHC.Enum.lvl5 */(_Cn/* smnN */, _Co/* smnO */, _Cp/* smnP */, _Cm/* smnM */);});
},
_Cq/* $fEnumWord6 */ = function(_Cr/* s1Rs9 */){
  return new F(function(){return _Cl/* GHC.Enum.toEnumError */(_B5/* GHC.Word.$fShowWord32 */, _BI/* GHC.Word.lvl4 */, _Cr/* s1Rs9 */, _C5/* GHC.Word.lvl12 */);});
},
_Cs/* $fEnumWord32_$ctoEnum */ = function(_Ct/* s1Rse */){
  var _Cu/* s1Rsf */ = E(_Ct/* s1Rse */);
  if(_Cu/* s1Rsf */<0){
    return new F(function(){return _Cq/* GHC.Word.$fEnumWord6 */(_Cu/* s1Rsf */);});
  }else{
    return _Cu/* s1Rsf */>>>0;
  }
},
_Cv/* $fEnumWord32 */ = new T(function(){
  return {_:0,a:_C2/* GHC.Word.$fEnumWord32_$csucc */,b:_BU/* GHC.Word.$fEnumWord32_$cpred */,c:_Cs/* GHC.Word.$fEnumWord32_$ctoEnum */,d:_BL/* GHC.Word.$fEnumWord32_$cfromEnum */,e:_zj/* GHC.Word.$fEnumWord32_$cenumFrom */,f:_Ab/* GHC.Word.$fEnumWord32_$cenumFromThen */,g:_Aw/* GHC.Word.$fEnumWord32_$cenumFromTo */,h:_An/* GHC.Word.$fEnumWord32_$cenumFromThenTo */};
}),
_Cw/* $fIntegralWord32_$cdivMod */ = function(_Cx/* s1RNe */, _Cy/* s1RNf */){
  var _Cz/* s1RNg */ = E(_Cx/* s1RNe */),
  _CA/* s1RNk */ = E(_Cy/* s1RNf */);
  return (_CA/* s1RNk */==0) ? E(_3K/* GHC.Real.divZeroError */) : new T2(0,new T(function(){
    return quot/* EXTERNAL */(_Cz/* s1RNg */, _CA/* s1RNk */);
  }),new T(function(){
    return _Cz/* s1RNg */%_CA/* s1RNk */;
  }));
},
_CB/* $fIntegralWord32_$cquot */ = function(_CC/* s1RMM */, _CD/* s1RMN */){
  var _CE/* s1RMS */ = E(_CD/* s1RMN */);
  if(!_CE/* s1RMS */){
    return E(_3K/* GHC.Real.divZeroError */);
  }else{
    return new F(function(){return quot/* EXTERNAL */(E(_CC/* s1RMM */), _CE/* s1RMS */);});
  }
},
_CF/* $fIntegralWord32_$cquotRem */ = function(_CG/* s1RN2 */, _CH/* s1RN3 */){
  var _CI/* s1RN8 */ = E(_CH/* s1RN3 */);
  if(!_CI/* s1RN8 */){
    return E(_3K/* GHC.Real.divZeroError */);
  }else{
    var _CJ/* s1RN9 */ = quotRemI/* EXTERNAL */(E(_CG/* s1RN2 */), _CI/* s1RN8 */);
    return new T2(0,_CJ/* s1RN9 */.a,_CJ/* s1RN9 */.b);
  }
},
_CK/* $fIntegralWord32_$crem */ = function(_CL/* s1RMU */, _CM/* s1RMV */){
  var _CN/* s1RN0 */ = E(_CM/* s1RMV */);
  return (_CN/* s1RN0 */==0) ? E(_3K/* GHC.Real.divZeroError */) : E(_CL/* s1RMU */)%_CN/* s1RN0 */;
},
_CO/* integer2Word# */ = function(_CP/* s2C */){
  return I_toInt/* EXTERNAL */(_CP/* s2C */)>>>0;
},
_CQ/* integerToWord */ = function(_CR/* s1Rr */){
  var _CS/* s1Rs */ = E(_CR/* s1Rr */);
  if(!_CS/* s1Rs */._){
    return _CS/* s1Rs */.a>>>0;
  }else{
    return new F(function(){return _CO/* GHC.Integer.GMP.Prim.integer2Word# */(_CS/* s1Rs */.a);});
  }
},
_CT/* $cfromInteger2 */ = function(_CU/* s1Rpq */){
  return new F(function(){return _CQ/* GHC.Integer.Type.integerToWord */(_CU/* s1Rpq */);});
},
_CV/* $fNumWord32_$c* */ = function(_CW/* s1Rpz */, _CX/* s1RpA */){
  return imul/* EXTERNAL */(E(_CW/* s1Rpz */), E(_CX/* s1RpA */))>>>0;
},
_CY/* $fNumWord32_$c+ */ = function(_CZ/* s1RpN */, _D0/* s1RpO */){
  return E(_CZ/* s1RpN */)+E(_D0/* s1RpO */)>>>0;
},
_D1/* $fNumWord32_$c- */ = function(_D2/* s1RpG */, _D3/* s1RpH */){
  return E(_D2/* s1RpG */)-E(_D3/* s1RpH */)>>>0;
},
_D4/* $fNumWord32_$cabs */ = function(_D5/* s1Rps */){
  return E(_D5/* s1Rps */);
},
_D6/* $fNumWord32_$cnegate */ = function(_D7/* s1Rpt */){
  return  -(E(_D7/* s1Rpt */)&4294967295)>>>0;
},
_D8/* $fNumWord2 */ = 1,
_D9/* $fNumWord32_$csignum */ = function(_Da/* s1RNz */){
  return (E(_Da/* s1RNz */)==0) ? E(_yP/* GHC.Word.$fBitsWord4 */) : E(_D8/* GHC.Word.$fNumWord2 */);
},
_Db/* $fNumWord32 */ = {_:0,a:_CY/* GHC.Word.$fNumWord32_$c+ */,b:_D1/* GHC.Word.$fNumWord32_$c- */,c:_CV/* GHC.Word.$fNumWord32_$c* */,d:_D6/* GHC.Word.$fNumWord32_$cnegate */,e:_D4/* GHC.Word.$fNumWord32_$cabs */,f:_D9/* GHC.Word.$fNumWord32_$csignum */,g:_CT/* GHC.Word.$cfromInteger2 */},
_Dc/* $fBitsWord32_$c/= */ = function(_Dd/* s1RME */, _De/* s1RMF */){
  return (E(_Dd/* s1RME */)!=E(_De/* s1RMF */)) ? true : false;
},
_Df/* $fEqWord32_$c== */ = function(_Dg/* s1RMx */, _Dh/* s1RMy */){
  return E(_Dg/* s1RMx */)==E(_Dh/* s1RMy */);
},
_Di/* $fEqWord32 */ = new T2(0,_Df/* GHC.Word.$fEqWord32_$c== */,_Dc/* GHC.Word.$fBitsWord32_$c/= */),
_Dj/* $fOrdWord32_$c< */ = function(_Dk/* s1RMg */, _Dl/* s1RMh */){
  return E(_Dk/* s1RMg */)<E(_Dl/* s1RMh */);
},
_Dm/* $fOrdWord32_$c<= */ = function(_Dn/* s1RLP */, _Do/* s1RLQ */){
  return E(_Dn/* s1RLP */)<=E(_Do/* s1RLQ */);
},
_Dp/* $fOrdWord32_$c> */ = function(_Dq/* s1RLI */, _Dr/* s1RLJ */){
  return E(_Dq/* s1RLI */)>E(_Dr/* s1RLJ */);
},
_Ds/* $fOrdWord32_$c>= */ = function(_Dt/* s1RLB */, _Du/* s1RLC */){
  return E(_Dt/* s1RLB */)>=E(_Du/* s1RLC */);
},
_Dv/* $fOrdWord32_$ccompare */ = function(_Dw/* s1RMn */, _Dx/* s1RMo */){
  var _Dy/* s1RMp */ = E(_Dw/* s1RMn */),
  _Dz/* s1RMr */ = E(_Dx/* s1RMo */);
  return (_Dy/* s1RMp */>=_Dz/* s1RMr */) ? (_Dy/* s1RMp */!=_Dz/* s1RMr */) ? 2 : 1 : 0;
},
_DA/* $fOrdWord32_$cmax */ = function(_DB/* s1ROa */, _DC/* s1ROb */){
  var _DD/* s1ROc */ = E(_DB/* s1ROa */),
  _DE/* s1ROe */ = E(_DC/* s1ROb */);
  return (_DD/* s1ROc */>_DE/* s1ROe */) ? E(_DD/* s1ROc */) : E(_DE/* s1ROe */);
},
_DF/* $fOrdWord32_$cmin */ = function(_DG/* s1RO2 */, _DH/* s1RO3 */){
  var _DI/* s1RO4 */ = E(_DG/* s1RO2 */),
  _DJ/* s1RO6 */ = E(_DH/* s1RO3 */);
  return (_DI/* s1RO4 */>_DJ/* s1RO6 */) ? E(_DJ/* s1RO6 */) : E(_DI/* s1RO4 */);
},
_DK/* $fOrdWord32 */ = {_:0,a:_Di/* GHC.Word.$fEqWord32 */,b:_Dv/* GHC.Word.$fOrdWord32_$ccompare */,c:_Dj/* GHC.Word.$fOrdWord32_$c< */,d:_Dm/* GHC.Word.$fOrdWord32_$c<= */,e:_Dp/* GHC.Word.$fOrdWord32_$c> */,f:_Ds/* GHC.Word.$fOrdWord32_$c>= */,g:_DA/* GHC.Word.$fOrdWord32_$cmax */,h:_DF/* GHC.Word.$fOrdWord32_$cmin */},
_DL/* $fRealWord1 */ = new T1(0,1),
_DM/* even1 */ = new T1(0,0),
_DN/* remInteger */ = function(_DO/* s1NY */, _DP/* s1NZ */){
  while(1){
    var _DQ/* s1O0 */ = E(_DO/* s1NY */);
    if(!_DQ/* s1O0 */._){
      var _DR/* s1O2 */ = E(_DQ/* s1O0 */.a);
      if(_DR/* s1O2 */==( -2147483648)){
        _DO/* s1NY */ = new T1(1,I_fromInt/* EXTERNAL */( -2147483648));
        continue;
      }else{
        var _DS/* s1O3 */ = E(_DP/* s1NZ */);
        if(!_DS/* s1O3 */._){
          return new T1(0,_DR/* s1O2 */%_DS/* s1O3 */.a);
        }else{
          _DO/* s1NY */ = new T1(1,I_fromInt/* EXTERNAL */(_DR/* s1O2 */));
          _DP/* s1NZ */ = _DS/* s1O3 */;
          continue;
        }
      }
    }else{
      var _DT/* s1Od */ = _DQ/* s1O0 */.a,
      _DU/* s1Oe */ = E(_DP/* s1NZ */);
      return (_DU/* s1Oe */._==0) ? new T1(1,I_rem/* EXTERNAL */(_DT/* s1Od */, I_fromInt/* EXTERNAL */(_DU/* s1Oe */.a))) : new T1(1,I_rem/* EXTERNAL */(_DT/* s1Od */, _DU/* s1Oe */.a));
    }
  }
},
_DV/* $fIntegralInteger_$crem */ = function(_DW/* svkU */, _DX/* svkV */){
  if(!B(_yn/* GHC.Integer.Type.eqInteger */(_DX/* svkV */, _DM/* GHC.Real.even1 */))){
    return new F(function(){return _DN/* GHC.Integer.Type.remInteger */(_DW/* svkU */, _DX/* svkV */);});
  }else{
    return E(_3K/* GHC.Real.divZeroError */);
  }
},
_DY/* $fEnumRatio_gcd' */ = function(_DZ/* svl0 */, _E0/* svl1 */){
  while(1){
    if(!B(_yn/* GHC.Integer.Type.eqInteger */(_E0/* svl1 */, _DM/* GHC.Real.even1 */))){
      var _E1/*  svl0 */ = _E0/* svl1 */,
      _E2/*  svl1 */ = B(_DV/* GHC.Real.$fIntegralInteger_$crem */(_DZ/* svl0 */, _E0/* svl1 */));
      _DZ/* svl0 */ = _E1/*  svl0 */;
      _E0/* svl1 */ = _E2/*  svl1 */;
      continue;
    }else{
      return E(_DZ/* svl0 */);
    }
  }
},
_E3/* absInteger */ = function(_E4/* s1QP */){
  var _E5/* s1QQ */ = E(_E4/* s1QP */);
  if(!_E5/* s1QQ */._){
    var _E6/* s1QS */ = E(_E5/* s1QQ */.a);
    return (_E6/* s1QS */==( -2147483648)) ? E(_xR/* GHC.Integer.Type.lvl3 */) : (_E6/* s1QS */<0) ? new T1(0, -_E6/* s1QS */) : E(_E5/* s1QQ */);
  }else{
    var _E7/* s1QW */ = _E5/* s1QQ */.a;
    return (I_compareInt/* EXTERNAL */(_E7/* s1QW */, 0)>=0) ? E(_E5/* s1QQ */) : new T1(1,I_negate/* EXTERNAL */(_E7/* s1QW */));
  }
},
_E8/* quotInteger */ = function(_E9/* s1On */, _Ea/* s1Oo */){
  while(1){
    var _Eb/* s1Op */ = E(_E9/* s1On */);
    if(!_Eb/* s1Op */._){
      var _Ec/* s1Or */ = E(_Eb/* s1Op */.a);
      if(_Ec/* s1Or */==( -2147483648)){
        _E9/* s1On */ = new T1(1,I_fromInt/* EXTERNAL */( -2147483648));
        continue;
      }else{
        var _Ed/* s1Os */ = E(_Ea/* s1Oo */);
        if(!_Ed/* s1Os */._){
          return new T1(0,quot/* EXTERNAL */(_Ec/* s1Or */, _Ed/* s1Os */.a));
        }else{
          _E9/* s1On */ = new T1(1,I_fromInt/* EXTERNAL */(_Ec/* s1Or */));
          _Ea/* s1Oo */ = _Ed/* s1Os */;
          continue;
        }
      }
    }else{
      var _Ee/* s1OC */ = _Eb/* s1Op */.a,
      _Ef/* s1OD */ = E(_Ea/* s1Oo */);
      return (_Ef/* s1OD */._==0) ? new T1(0,I_toInt/* EXTERNAL */(I_quot/* EXTERNAL */(_Ee/* s1OC */, I_fromInt/* EXTERNAL */(_Ef/* s1OD */.a)))) : new T1(1,I_quot/* EXTERNAL */(_Ee/* s1OC */, _Ef/* s1OD */.a));
    }
  }
},
_Eg/* RatioZeroDenominator */ = 5,
_Eh/* ratioZeroDenomException */ = new T(function(){
  return B(_3H/* GHC.Exception.$fExceptionArithException_$ctoException */(_Eg/* GHC.Exception.RatioZeroDenominator */));
}),
_Ei/* ratioZeroDenominatorError */ = new T(function(){
  return die/* EXTERNAL */(_Eh/* GHC.Exception.ratioZeroDenomException */);
}),
_Ej/* $w$sreduce */ = function(_Ek/* svlj */, _El/* svlk */){
  if(!B(_yn/* GHC.Integer.Type.eqInteger */(_El/* svlk */, _DM/* GHC.Real.even1 */))){
    var _Em/* svlm */ = B(_DY/* GHC.Real.$fEnumRatio_gcd' */(B(_E3/* GHC.Integer.Type.absInteger */(_Ek/* svlj */)), B(_E3/* GHC.Integer.Type.absInteger */(_El/* svlk */))));
    return (!B(_yn/* GHC.Integer.Type.eqInteger */(_Em/* svlm */, _DM/* GHC.Real.even1 */))) ? new T2(0,B(_E8/* GHC.Integer.Type.quotInteger */(_Ek/* svlj */, _Em/* svlm */)),B(_E8/* GHC.Integer.Type.quotInteger */(_El/* svlk */, _Em/* svlm */))) : E(_3K/* GHC.Real.divZeroError */);
  }else{
    return E(_Ei/* GHC.Real.ratioZeroDenominatorError */);
  }
},
_En/* $w$ctoRational */ = function(_Eo/* s1RrR */){
  var _Ep/* s1RrS */ = _Eo/* s1RrR */&4294967295;
  if(_Ep/* s1RrS */<0){
    return new F(function(){return _Ej/* GHC.Real.$w$sreduce */(B(_yD/* GHC.Integer.Type.timesInteger */(B(_Ax/* GHC.Integer.Type.wordToInteger */(_Eo/* s1RrR */)), _DL/* GHC.Word.$fRealWord1 */)), _DL/* GHC.Word.$fRealWord1 */);});
  }else{
    return new F(function(){return _Ej/* GHC.Real.$w$sreduce */(B(_yD/* GHC.Integer.Type.timesInteger */(B(_yB/* GHC.Integer.Type.smallInteger */(_Ep/* s1RrS */)), _DL/* GHC.Word.$fRealWord1 */)), _DL/* GHC.Word.$fRealWord1 */);});
  }
},
_Eq/* $fRealWord32_$ctoRational */ = function(_Er/* s1RrZ */){
  var _Es/* s1Rs2 */ = B(_En/* GHC.Word.$w$ctoRational */(E(_Er/* s1RrZ */)));
  return new T2(0,E(_Es/* s1Rs2 */.a),E(_Es/* s1Rs2 */.b));
},
_Et/* $fRealWord32 */ = new T3(0,_Db/* GHC.Word.$fNumWord32 */,_DK/* GHC.Word.$fOrdWord32 */,_Eq/* GHC.Word.$fRealWord32_$ctoRational */),
_zl/* $fIntegralWord32 */ = new T(function(){
  return {_:0,a:_Et/* GHC.Word.$fRealWord32 */,b:_Cv/* GHC.Word.$fEnumWord32 */,c:_CB/* GHC.Word.$fIntegralWord32_$cquot */,d:_CK/* GHC.Word.$fIntegralWord32_$crem */,e:_CB/* GHC.Word.$fIntegralWord32_$cquot */,f:_CK/* GHC.Word.$fIntegralWord32_$crem */,g:_CF/* GHC.Word.$fIntegralWord32_$cquotRem */,h:_Cw/* GHC.Word.$fIntegralWord32_$cdivMod */,i:_Az/* GHC.Word.$fIntegralWord32_$ctoInteger */};
}),
_Eu/* lvl1 */ = new T1(0, -1),
_Ev/* signumInteger */ = function(_Ew/* s1OO */){
  var _Ex/* s1OP */ = E(_Ew/* s1OO */);
  if(!_Ex/* s1OP */._){
    var _Ey/* s1OQ */ = _Ex/* s1OP */.a;
    return (_Ey/* s1OQ */>=0) ? (E(_Ey/* s1OQ */)==0) ? E(_xA/* GHC.Integer.Type.lvl */) : E(_xP/* GHC.Integer.Type.log2I1 */) : E(_Eu/* GHC.Integer.Type.lvl1 */);
  }else{
    var _Ez/* s1OW */ = I_compareInt/* EXTERNAL */(_Ex/* s1OP */.a, 0);
    return (_Ez/* s1OW */<=0) ? (E(_Ez/* s1OW */)==0) ? E(_xA/* GHC.Integer.Type.lvl */) : E(_Eu/* GHC.Integer.Type.lvl1 */) : E(_xP/* GHC.Integer.Type.log2I1 */);
  }
},
_EA/* $w$s$c/ */ = function(_EB/* svlJ */, _EC/* svlK */, _ED/* svlL */, _EE/* svlM */){
  var _EF/* svlN */ = B(_yD/* GHC.Integer.Type.timesInteger */(_EC/* svlK */, _ED/* svlL */));
  return new F(function(){return _Ej/* GHC.Real.$w$sreduce */(B(_yD/* GHC.Integer.Type.timesInteger */(B(_yD/* GHC.Integer.Type.timesInteger */(_EB/* svlJ */, _EE/* svlM */)), B(_Ev/* GHC.Integer.Type.signumInteger */(_EF/* svlN */)))), B(_E3/* GHC.Integer.Type.absInteger */(_EF/* svlN */)));});
},
_EG/* quotRemInteger */ = function(_EH/* s1Ma */, _EI/* s1Mb */){
  while(1){
    var _EJ/* s1Mc */ = E(_EH/* s1Ma */);
    if(!_EJ/* s1Mc */._){
      var _EK/* s1Me */ = E(_EJ/* s1Mc */.a);
      if(_EK/* s1Me */==( -2147483648)){
        _EH/* s1Ma */ = new T1(1,I_fromInt/* EXTERNAL */( -2147483648));
        continue;
      }else{
        var _EL/* s1Mf */ = E(_EI/* s1Mb */);
        if(!_EL/* s1Mf */._){
          var _EM/* s1Mg */ = _EL/* s1Mf */.a;
          return new T2(0,new T1(0,quot/* EXTERNAL */(_EK/* s1Me */, _EM/* s1Mg */)),new T1(0,_EK/* s1Me */%_EM/* s1Mg */));
        }else{
          _EH/* s1Ma */ = new T1(1,I_fromInt/* EXTERNAL */(_EK/* s1Me */));
          _EI/* s1Mb */ = _EL/* s1Mf */;
          continue;
        }
      }
    }else{
      var _EN/* s1Mt */ = E(_EI/* s1Mb */);
      if(!_EN/* s1Mt */._){
        _EH/* s1Ma */ = _EJ/* s1Mc */;
        _EI/* s1Mb */ = new T1(1,I_fromInt/* EXTERNAL */(_EN/* s1Mt */.a));
        continue;
      }else{
        var _EO/* s1MA */ = I_quotRem/* EXTERNAL */(_EJ/* s1Mc */.a, _EN/* s1Mt */.a);
        return new T2(0,new T1(1,_EO/* s1MA */.a),new T1(1,_EO/* s1MA */.b));
      }
    }
  }
},
_EP/* $w$s$cproperFraction */ = function(_EQ/* svzS */, _ER/* svzT */, _ES/* svzU */){
  var _ET/* svzV */ = new T(function(){
    if(!B(_yn/* GHC.Integer.Type.eqInteger */(_ES/* svzU */, _DM/* GHC.Real.even1 */))){
      var _EU/* svzX */ = B(_EG/* GHC.Integer.Type.quotRemInteger */(_ER/* svzT */, _ES/* svzU */));
      return new T2(0,_EU/* svzX */.a,_EU/* svzX */.b);
    }else{
      return E(_3K/* GHC.Real.divZeroError */);
    }
  }),
  _EV/* svA6 */ = new T(function(){
    return B(A2(_yX/* GHC.Num.fromInteger */,B(_yV/* GHC.Real.$p1Real */(B(_yT/* GHC.Real.$p1Integral */(_EQ/* svzS */)))), new T(function(){
      return E(E(_ET/* svzV */).a);
    })));
  });
  return new T2(0,_EV/* svA6 */,new T(function(){
    return new T2(0,E(E(_ET/* svzV */).b),E(_ES/* svzU */));
  }));
},
_EW/* $fRealFracNominalDiffTime_$ctruncate */ = function(_EX/* sjkL */, _EY/* sjkM */){
  var _EZ/* sjkN */ = B(_EA/* GHC.Real.$w$s$c/ */(_EY/* sjkM */, _yS/* GHC.Real.$fEnumRatio1 */, _y3/* Data.Fixed.$fHasResolutionE5 */, _yS/* GHC.Real.$fEnumRatio1 */));
  return E(B(_EP/* GHC.Real.$w$s$cproperFraction */(_EX/* sjkL */, _EZ/* sjkN */.a, _EZ/* sjkN */.b)).a);
},
_F0/* $w$cshiftL */ = function(_F1/* s1Rz0 */, _F2/* s1Rz1 */){
  if(_F2/* s1Rz1 */<64){
    var _F3/* s1Rz5 */ = hs_uncheckedShiftL64/* EXTERNAL */(_F1/* s1Rz0 */, _F2/* s1Rz1 */);
    return E(_F3/* s1Rz5 */);
  }else{
    var _F4/* s1Rz9 */ = hs_wordToWord64/* EXTERNAL */(0);
    return E(_F4/* s1Rz9 */);
  }
},
_F5/* lvl5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Negative exponent"));
}),
_F6/* ^1 */ = new T(function(){
  return B(err/* EXTERNAL */(_F5/* GHC.Real.lvl5 */));
}),
_F7/* even2 */ = new T1(0,2),
_F8/* even3 */ = new T(function(){
  return B(_yn/* GHC.Integer.Type.eqInteger */(_F7/* GHC.Real.even2 */, _DM/* GHC.Real.even1 */));
}),
_F9/* g */ = function(_Fa/* svB9 */, _Fb/* svBa */, _Fc/* svBb */){
  while(1){
    if(!E(_F8/* GHC.Real.even3 */)){
      if(!B(_yn/* GHC.Integer.Type.eqInteger */(B(_DN/* GHC.Integer.Type.remInteger */(_Fb/* svBa */, _F7/* GHC.Real.even2 */)), _DM/* GHC.Real.even1 */))){
        if(!B(_yn/* GHC.Integer.Type.eqInteger */(_Fb/* svBa */, _yS/* GHC.Real.$fEnumRatio1 */))){
          var _Fd/*  svB9 */ = B(_yD/* GHC.Integer.Type.timesInteger */(_Fa/* svB9 */, _Fa/* svB9 */)),
          _Fe/*  svBa */ = B(_E8/* GHC.Integer.Type.quotInteger */(B(_zN/* GHC.Integer.Type.minusInteger */(_Fb/* svBa */, _yS/* GHC.Real.$fEnumRatio1 */)), _F7/* GHC.Real.even2 */)),
          _Ff/*  svBb */ = B(_yD/* GHC.Integer.Type.timesInteger */(_Fa/* svB9 */, _Fc/* svBb */));
          _Fa/* svB9 */ = _Fd/*  svB9 */;
          _Fb/* svBa */ = _Fe/*  svBa */;
          _Fc/* svBb */ = _Ff/*  svBb */;
          continue;
        }else{
          return new F(function(){return _yD/* GHC.Integer.Type.timesInteger */(_Fa/* svB9 */, _Fc/* svBb */);});
        }
      }else{
        var _Fd/*  svB9 */ = B(_yD/* GHC.Integer.Type.timesInteger */(_Fa/* svB9 */, _Fa/* svB9 */)),
        _Fe/*  svBa */ = B(_E8/* GHC.Integer.Type.quotInteger */(_Fb/* svBa */, _F7/* GHC.Real.even2 */));
        _Fa/* svB9 */ = _Fd/*  svB9 */;
        _Fb/* svBa */ = _Fe/*  svBa */;
        continue;
      }
    }else{
      return E(_3K/* GHC.Real.divZeroError */);
    }
  }
},
_Fg/* ^_f */ = function(_Fh/* svBn */, _Fi/* svBo */){
  while(1){
    if(!E(_F8/* GHC.Real.even3 */)){
      if(!B(_yn/* GHC.Integer.Type.eqInteger */(B(_DN/* GHC.Integer.Type.remInteger */(_Fi/* svBo */, _F7/* GHC.Real.even2 */)), _DM/* GHC.Real.even1 */))){
        if(!B(_yn/* GHC.Integer.Type.eqInteger */(_Fi/* svBo */, _yS/* GHC.Real.$fEnumRatio1 */))){
          return new F(function(){return _F9/* GHC.Real.g */(B(_yD/* GHC.Integer.Type.timesInteger */(_Fh/* svBn */, _Fh/* svBn */)), B(_E8/* GHC.Integer.Type.quotInteger */(B(_zN/* GHC.Integer.Type.minusInteger */(_Fi/* svBo */, _yS/* GHC.Real.$fEnumRatio1 */)), _F7/* GHC.Real.even2 */)), _Fh/* svBn */);});
        }else{
          return E(_Fh/* svBn */);
        }
      }else{
        var _Fj/*  svBn */ = B(_yD/* GHC.Integer.Type.timesInteger */(_Fh/* svBn */, _Fh/* svBn */)),
        _Fk/*  svBo */ = B(_E8/* GHC.Integer.Type.quotInteger */(_Fi/* svBo */, _F7/* GHC.Real.even2 */));
        _Fh/* svBn */ = _Fj/*  svBn */;
        _Fi/* svBo */ = _Fk/*  svBo */;
        continue;
      }
    }else{
      return E(_3K/* GHC.Real.divZeroError */);
    }
  }
},
_Fl/* ^_$s^ */ = function(_Fm/* svBz */, _Fn/* svBA */){
  if(!B(_zn/* GHC.Integer.Type.ltInteger */(_Fn/* svBA */, _DM/* GHC.Real.even1 */))){
    if(!B(_yn/* GHC.Integer.Type.eqInteger */(_Fn/* svBA */, _DM/* GHC.Real.even1 */))){
      return new F(function(){return _Fg/* GHC.Real.^_f */(_Fm/* svBz */, _Fn/* svBA */);});
    }else{
      return E(_yS/* GHC.Real.$fEnumRatio1 */);
    }
  }else{
    return E(_F6/* GHC.Real.^1 */);
  }
},
_Fo/* cpuTimePrecision1 */ = new T1(0,64),
_Fp/* cpuTimePrecision2 */ = new T1(0,2),
_Fq/* cpuTimePrecision */ = new T(function(){
  return B(_Fl/* GHC.Real.^_$s^ */(_Fp/* System.CPUTime.cpuTimePrecision2 */, _Fo/* System.CPUTime.cpuTimePrecision1 */));
}),
_Fr/* getCPUTime2 */ = new T1(0,0),
_Fs/* initSMGen3 */ = new T(function(){
  return B(_yf/* GHC.Integer.Type.divInteger */(_Fr/* System.CPUTime.getCPUTime2 */, _Fq/* System.CPUTime.cpuTimePrecision */));
}),
_Ft/* initSMGen5 */ = new T1(0,0),
_Fu/* initSMGen4 */ = new T(function(){
  return B(_yn/* GHC.Integer.Type.eqInteger */(_Fq/* System.CPUTime.cpuTimePrecision */, _Ft/* System.Random.SplitMix.initSMGen5 */));
}),
_Fv/* initSMGen2 */ = function(_Fw/* saXA */, _/* EXTERNAL */){
  return new T(function(){
    if(!E(_Fu/* System.Random.SplitMix.initSMGen4 */)){
      var _Fx/* saXF */ = hs_wordToWord64/* EXTERNAL */(B(_CQ/* GHC.Integer.Type.integerToWord */(_Fs/* System.Random.SplitMix.initSMGen3 */))),
      _Fy/* saXM */ = hs_wordToWord64/* EXTERNAL */(B(_EW/* Data.Time.Clock.UTC.$fRealFracNominalDiffTime_$ctruncate */(_zl/* GHC.Word.$fIntegralWord32 */, _Fw/* saXA */)));
      return hs_or64/* EXTERNAL */(B(_F0/* GHC.Word.$w$cshiftL */(_Fx/* saXF */, 32)), _Fy/* saXM */);
    }else{
      return E(_3K/* GHC.Real.divZeroError */);
    }
  });
},
_Fz/* mkSMGen */ = function(_FA/* saZo */){
  var _FB/* saZp */ = E(_FA/* saZo */);
  return new T2(0,B(_2l/* System.Random.SplitMix.$wmix64 */(_FB/* saZp */)),B(_4T/* System.Random.SplitMix.$wmixGamma */(B(_22/* GHC.Word.$w$c+ */(_FB/* saZp */, new Long/* EXTERNAL */(2135587861, 2654435769, true))))));
},
_FC/* lvl */ = function(_/* EXTERNAL */){
  var _FD/* szS3 */ = B(_yL/* Data.Time.Clock.POSIX.getPOSIXTime1 */(0)),
  _FE/* szS6 */ = B(_Fv/* System.Random.SplitMix.initSMGen2 */(_FD/* szS3 */, 0));
  return new F(function(){return nMV/* EXTERNAL */(new T(function(){
    return B(_Fz/* System.Random.SplitMix.mkSMGen */(_FE/* szS6 */));
  }));});
},
_FF/* theStdGen */ = new T(function(){
  return B(_1S/* GHC.IO.unsafeDupablePerformIO */(_FC/* System.Random.lvl */));
}),
_FG/* $wlvl */ = function(_FH/* shvx */, _FI/* shvy */, _FJ/* shvz */, _FK/* shvA */, _/* EXTERNAL */){
  var _FL/* shvC */ = E(_FJ/* shvz */);
  if(!_FL/* shvC */._){
    return _2s/* GHC.Tuple.() */;
  }else{
    if(!E(_FL/* shvC */.a)){
      var _FM/* shvF */ = E(_x4/* LudoJS.f2 */),
      _FN/* shvI */ = __app0/* EXTERNAL */(_FM/* shvF */),
      _FO/* shvL */ = B(_oD/* LudoJS.$wa1 */(_FN/* shvI */, _/* EXTERNAL */)),
      _FP/* shw2 */ = function(_FQ/* shw3 */){
        var _FR/* shwb */ = eval/* EXTERNAL */(E(_x8/* LudoJS.lvl2 */)),
        _FS/* shwj */ = __app3/* EXTERNAL */(E(_FR/* shwb */), E(_FH/* shvx */), E(_FI/* shvy */), toJSStr/* EXTERNAL */(_FQ/* shw3 */)),
        _FT/* shwp */ = __eq/* EXTERNAL */(_FS/* shwj */, E(_1W/* Haste.Prim.Any.jsNull */));
        if(!E(_FT/* shwp */)){
          var _FU/* shwu */ = __isUndef/* EXTERNAL */(_FS/* shwj */);
          if(!E(_FU/* shwu */)){
            var _FV/* shwy */ = new T(function(){
              var _FW/* shwA */ = Number/* EXTERNAL */(_FS/* shwj */);
              return jsTrunc/* EXTERNAL */(_FW/* shwA */);
            }),
            _FX/* shwI */ = __app0/* EXTERNAL */(_FM/* shvF */),
            _FY/* shwL */ = B(_oD/* LudoJS.$wa1 */(_FX/* shwI */, _/* EXTERNAL */)),
            _FZ/* shwO */ = E(_FY/* shwL */),
            _G0/* shwQ */ = _FZ/* shwO */.b,
            _G1/* shwR */ = _FZ/* shwO */.c,
            _G2/* shwS */ = _FZ/* shwO */.d,
            _G3/* shwT */ = _FZ/* shwO */.e,
            _G4/* shwU */ = E(_FZ/* shwO */.a);
            switch(_G4/* shwU */._){
              case 0:
                if(E(_FV/* shwy */)==( -5)){
                  var _G5/* shx1 */ = mMV/* EXTERNAL */(E(_FF/* System.Random.theStdGen */), _x9/* LudoJS.lvl42 */),
                  _G6/* shx4 */ = E(_G5/* shx1 */),
                  _G7/* shx6 */ = B(_cn/* LudoJS.a42 */(_G6/* shx4 */, _FZ/* shwO */, _/* EXTERNAL */)),
                  _G8/* shx9 */ = E(_G7/* shx6 */),
                  _G9/* shxb */ = _G8/* shx9 */.b;
                  if(!E(_G8/* shx9 */.a)._){
                    var _Ga/* shxd */ = E(_G9/* shxb */),
                    _Gb/* shxp */ = B(_p0/* LudoJS.$wa13 */(new T1(0,new T1(1,_G6/* shx4 */)), _Ga/* shxd */.b, new T(function(){
                      return E(_Ga/* shxd */.c)-1|0;
                    }), _Ga/* shxd */.d, _Ga/* shxd */.e, _/* EXTERNAL */)),
                    _Gc/* shxv */ = E(E(_Gb/* shxp */).b);
                    return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _Gc/* shxv */.a, _Gc/* shxv */.b, _Gc/* shxv */.c, _Gc/* shxv */.d, _Gc/* shxv */.e);});
                  }else{
                    var _Gd/* shxD */ = E(_G9/* shxb */);
                    return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, new T1(1,_G6/* shx4 */), _Gd/* shxD */.b, new T(function(){
                      if(E(_G6/* shx4 */)==6){
                        return E(_oT/* LudoJS.lvl7 */);
                      }else{
                        return E(_bm/* LudoJS.$fShowStage2 */);
                      }
                    }), _Gd/* shxD */.d, _Gd/* shxD */.e);});
                  }
                }else{
                  return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G4/* shwU */, _G0/* shwQ */, _G1/* shwR */, _G2/* shwS */, _G3/* shwT */);});
                }
                break;
              case 1:
                var _Ge/* shxM */ = _G4/* shwU */.a,
                _Gf/* shxN */ = E(_FV/* shwy */),
                _Gg/* shxP */ = function(_/* EXTERNAL */, _Gh/* shxR */, _Gi/* shxS */, _Gj/* shxT */, _Gk/* shxU */, _Gl/* shxV */, _Gm/* shxW */){
                  var _Gn/* shxX */ = E(_Gh/* shxR */);
                  if(!_Gn/* shxX */._){
                    return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _Gi/* shxS */, _Gj/* shxT */, _Gk/* shxU */, _Gl/* shxV */, _Gm/* shxW */);});
                  }else{
                    var _Go/* shxZ */ = B(_bt/* LudoJS.$s!1 */(_Gj/* shxT */, _Gl/* shxV */));
                    if(!_Go/* shxZ */._){
                      return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _Gi/* shxS */, _Gj/* shxT */, _Gk/* shxU */, _Gl/* shxV */, _Gm/* shxW */);});
                    }else{
                      var _Gp/* shy7 */ = E(_Gn/* shxX */.a);
                      if(E(E(_Go/* shxZ */.a).a)!=_Gp/* shy7 */){
                        var _Gq/* shyb */ = function(_Gr/* shyc */){
                          while(1){
                            var _Gs/* shyd */ = E(_Gr/* shyc */);
                            if(!_Gs/* shyd */._){
                              return false;
                            }else{
                              if(E(E(_Gs/* shyd */.a).a)!=_Gp/* shy7 */){
                                _Gr/* shyc */ = _Gs/* shyd */.b;
                                continue;
                              }else{
                                return true;
                              }
                            }
                          }
                        };
                        if(!B(_Gq/* shyb */(_Go/* shxZ */.b))){
                          return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _Gi/* shxS */, _Gj/* shxT */, _Gk/* shxU */, _Gl/* shxV */, _Gm/* shxW */);});
                        }else{
                          return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, new T2(2,_Ge/* shxM */,_Gp/* shy7 */), _Gj/* shxT */, _Gk/* shxU */, _Gl/* shxV */, _Gm/* shxW */);});
                        }
                      }else{
                        return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, new T2(2,_Ge/* shxM */,_Gp/* shy7 */), _Gj/* shxT */, _Gk/* shxU */, _Gl/* shxV */, _Gm/* shxW */);});
                      }
                    }
                  }
                };
                if(_Gf/* shxN */<( -4)){
                  if(_Gf/* shxN */<0){
                    return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G4/* shwU */, _G0/* shwQ */, _G1/* shwR */, _G2/* shwS */, _G3/* shwT */);});
                  }else{
                    if(_Gf/* shxN */>55){
                      return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G4/* shwU */, _G0/* shwQ */, _G1/* shwR */, _G2/* shwS */, _G3/* shwT */);});
                    }else{
                      var _Gt/* shyw */ = function(_Gu/* shyx */){
                        while(1){
                          var _Gv/* shyy */ = E(_Gu/* shyx */);
                          if(!_Gv/* shyy */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _Gw/* shyA */ = _Gv/* shyy */.b,
                            _Gx/* shyB */ = E(_Gv/* shyy */.a),
                            _Gy/* shyE */ = E(_Gx/* shyB */.b);
                            if(!_Gy/* shyE */._){
                              _Gu/* shyx */ = _Gw/* shyA */;
                              continue;
                            }else{
                              if(_Gf/* shxN */!=E(_Gy/* shyE */.a)){
                                _Gu/* shyx */ = _Gw/* shyA */;
                                continue;
                              }else{
                                return new T1(1,_Gx/* shyB */.a);
                              }
                            }
                          }
                        }
                      };
                      return new F(function(){return _Gg/* shxP */(_/* EXTERNAL */, B(_Gt/* shyw */(B(_bt/* LudoJS.$s!1 */(_G0/* shwQ */, _G2/* shwS */)))), _G4/* shwU */, _G0/* shwQ */, _G1/* shwR */, _G2/* shwS */, _G3/* shwT */);});
                    }
                  }
                }else{
                  if(_Gf/* shxN */>( -1)){
                    if(_Gf/* shxN */<0){
                      return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G4/* shwU */, _G0/* shwQ */, _G1/* shwR */, _G2/* shwS */, _G3/* shwT */);});
                    }else{
                      if(_Gf/* shxN */>55){
                        return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G4/* shwU */, _G0/* shwQ */, _G1/* shwR */, _G2/* shwS */, _G3/* shwT */);});
                      }else{
                        var _Gz/* shyS */ = function(_GA/* shyT */){
                          while(1){
                            var _GB/* shyU */ = E(_GA/* shyT */);
                            if(!_GB/* shyU */._){
                              return __Z/* EXTERNAL */;
                            }else{
                              var _GC/* shyW */ = _GB/* shyU */.b,
                              _GD/* shyX */ = E(_GB/* shyU */.a),
                              _GE/* shz0 */ = E(_GD/* shyX */.b);
                              if(!_GE/* shz0 */._){
                                _GA/* shyT */ = _GC/* shyW */;
                                continue;
                              }else{
                                if(_Gf/* shxN */!=E(_GE/* shz0 */.a)){
                                  _GA/* shyT */ = _GC/* shyW */;
                                  continue;
                                }else{
                                  return new T1(1,_GD/* shyX */.a);
                                }
                              }
                            }
                          }
                        };
                        return new F(function(){return _Gg/* shxP */(_/* EXTERNAL */, B(_Gz/* shyS */(B(_bt/* LudoJS.$s!1 */(_G0/* shwQ */, _G2/* shwS */)))), _G4/* shwU */, _G0/* shwQ */, _G1/* shwR */, _G2/* shwS */, _G3/* shwT */);});
                      }
                    }
                  }else{
                    var _GF/* shz8 */ = _Gf/* shxN */+5|0,
                    _GG/* shzb */ = function(_GH/* shzc */){
                      while(1){
                        var _GI/* shzd */ = E(_GH/* shzc */);
                        if(!_GI/* shzd */._){
                          return __Z/* EXTERNAL */;
                        }else{
                          var _GJ/* shzf */ = _GI/* shzd */.b,
                          _GK/* shzg */ = E(_GI/* shzd */.a);
                          if(E(_GK/* shzg */.a)!=_GF/* shz8 */){
                            _GH/* shzc */ = _GJ/* shzf */;
                            continue;
                          }else{
                            if(!E(_GK/* shzg */.b)._){
                              return E(new T1(1,_GF/* shz8 */));
                            }else{
                              _GH/* shzc */ = _GJ/* shzf */;
                              continue;
                            }
                          }
                        }
                      }
                    };
                    return new F(function(){return _Gg/* shxP */(_/* EXTERNAL */, B(_GG/* shzb */(B(_bt/* LudoJS.$s!1 */(_G0/* shwQ */, _G2/* shwS */)))), _G4/* shwU */, _G0/* shwQ */, _G1/* shwR */, _G2/* shwS */, _G3/* shwT */);});
                  }
                }
                break;
              case 2:
                var _GL/* shzr */ = _G4/* shwU */.a,
                _GM/* shzs */ = _G4/* shwU */.b,
                _GN/* shzt */ = E(_FV/* shwy */);
                if(_GN/* shzt */>56){
                  return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G4/* shwU */, _G0/* shwQ */, _G1/* shwR */, _G2/* shwS */, _G3/* shwT */);});
                }else{
                  if(_GN/* shzt */<0){
                    return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G4/* shwU */, _G0/* shwQ */, _G1/* shwR */, _G2/* shwS */, _G3/* shwT */);});
                  }else{
                    var _GO/* shzz */ = B(_cn/* LudoJS.a42 */(_GL/* shzr */, _FZ/* shwO */, _/* EXTERNAL */)),
                    _GP/* shzC */ = E(_GO/* shzz */),
                    _GQ/* shzE */ = _GP/* shzC */.b,
                    _GR/* shzF */ = new T(function(){
                      var _GS/* shzG */ = new T2(1,_GM/* shzs */,_GN/* shzt */),
                      _GT/* shzH */ = B(_bt/* LudoJS.$s!1 */(_G0/* shwQ */, _G2/* shwS */));
                      if(!_GT/* shzH */._){
                        return E(_x6/* LudoJS.lvl1 */);
                      }else{
                        var _GU/* shzK */ = E(_GT/* shzH */.a),
                        _GV/* shzN */ = E(_GU/* shzK */.a),
                        _GW/* shzP */ = E(_GM/* shzs */);
                        if(_GV/* shzN */!=_GW/* shzP */){
                          var _GX/* shzT */ = function(_GY/* shzU */){
                            while(1){
                              var _GZ/* shzV */ = E(_GY/* shzU */);
                              if(!_GZ/* shzV */._){
                                return E(_x6/* LudoJS.lvl1 */);
                              }else{
                                var _H0/* shzY */ = E(_GZ/* shzV */.a),
                                _H1/* shA1 */ = E(_H0/* shzY */.a);
                                if(_H1/* shA1 */!=_GW/* shzP */){
                                  _GY/* shzU */ = _GZ/* shzV */.b;
                                  continue;
                                }else{
                                  return (E(_H0/* shzY */.b)._==0) ? new T1(0,_H1/* shA1 */) : E(_GS/* shzG */);
                                }
                              }
                            }
                          };
                          return B(_GX/* shzT */(_GT/* shzH */.b));
                        }else{
                          if(!E(_GU/* shzK */.b)._){
                            return new T1(0,_GV/* shzN */);
                          }else{
                            return E(_GS/* shzG */);
                          }
                        }
                      }
                    });
                    if(!B(_uz/* GHC.List.elem */(_bl/* LudoJS.$fEqOption */, _GR/* shzF */, _GP/* shzC */.a))){
                      var _H2/* shAa */ = E(_GQ/* shzE */);
                      return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, new T1(1,_GL/* shzr */), _H2/* shAa */.b, _H2/* shAa */.c, _H2/* shAa */.d, _H2/* shAa */.e);});
                    }else{
                      var _H3/* shAh */ = function(_/* EXTERNAL */, _H4/* shAj */, _H5/* shAk */, _H6/* shAl */, _H7/* shAm */, _H8/* shAn */){
                        if(!B(_bB/* GHC.List.$wlenAcc */(B(_bt/* LudoJS.$s!1 */(_H5/* shAk */, _H7/* shAm */)), 0))){
                          var _H9/* shAQ */ = B(_q/* GHC.Base.++ */(_H8/* shAn */, new T2(1,_H5/* shAk */,_4/* GHC.Types.[] */)));
                          if(B(_bB/* GHC.List.$wlenAcc */(_H9/* shAQ */, 0))==3){
                            var _Ha/* shB3 */ = B(_c3/* LudoJS.$sa1 */(_bn/* LudoJS.Blue */, _x7/* LudoJS.lvl10 */, _H9/* shAQ */, _H4/* shAj */, _H5/* shAk */, _H6/* shAl */, _H7/* shAm */, _H9/* shAQ */, _/* EXTERNAL */)),
                            _Hb/* shBa */ = B(_pd/* LudoJS.$wa14 */(_of/* LudoJS.GameFinished */, _H5/* shAk */, _H6/* shAl */, _H7/* shAm */, new T(function(){
                              return E(E(_Ha/* shB3 */).a);
                            }), _/* EXTERNAL */));
                            return new T2(0,_2s/* GHC.Tuple.() */,new T(function(){
                              return E(E(_Hb/* shBa */).b);
                            }));
                          }else{
                            var _Hc/* shAV */ = B(_pd/* LudoJS.$wa14 */(new T1(0,new T1(1,_GL/* shzr */)), _H5/* shAk */, _H6/* shAl */, _H7/* shAm */, _H9/* shAQ */, _/* EXTERNAL */));
                            return new T2(0,_2s/* GHC.Tuple.() */,new T(function(){
                              return E(E(_Hc/* shAV */).b);
                            }));
                          }
                        }else{
                          if(B(_bB/* GHC.List.$wlenAcc */(_H8/* shAn */, 0))==3){
                            var _Hd/* shAB */ = B(_c3/* LudoJS.$sa1 */(_bn/* LudoJS.Blue */, _x7/* LudoJS.lvl10 */, _H8/* shAn */, _H4/* shAj */, _H5/* shAk */, _H6/* shAl */, _H7/* shAm */, _H8/* shAn */, _/* EXTERNAL */)),
                            _He/* shAI */ = B(_pd/* LudoJS.$wa14 */(_of/* LudoJS.GameFinished */, _H5/* shAk */, _H6/* shAl */, _H7/* shAm */, new T(function(){
                              return E(E(_Hd/* shAB */).a);
                            }), _/* EXTERNAL */));
                            return new T2(0,_2s/* GHC.Tuple.() */,new T(function(){
                              return E(E(_He/* shAI */).b);
                            }));
                          }else{
                            var _Hf/* shAt */ = B(_pd/* LudoJS.$wa14 */(new T1(0,new T1(1,_GL/* shzr */)), _H5/* shAk */, _H6/* shAl */, _H7/* shAm */, _H8/* shAn */, _/* EXTERNAL */));
                            return new T2(0,_2s/* GHC.Tuple.() */,new T(function(){
                              return E(E(_Hf/* shAt */).b);
                            }));
                          }
                        }
                      },
                      _Hg/* shBi */ = E(_GR/* shzF */);
                      if(!_Hg/* shBi */._){
                        var _Hh/* shBk */ = B(_vr/* LudoJS.$wa6 */(_Hg/* shBi */.a, 0, _GQ/* shzE */, _/* EXTERNAL */)),
                        _Hi/* shBq */ = E(E(_Hh/* shBk */).b),
                        _Hj/* shBw */ = B(_H3/* shAh */(_/* EXTERNAL */, _Hi/* shBq */.a, _Hi/* shBq */.b, _Hi/* shBq */.c, _Hi/* shBq */.d, _Hi/* shBq */.e)),
                        _Hk/* shBC */ = E(E(_Hj/* shBw */).b);
                        return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _Hk/* shBC */.a, _Hk/* shBC */.b, _Hk/* shBC */.c, _Hk/* shBC */.d, _Hk/* shBC */.e);});
                      }else{
                        var _Hl/* shBM */ = B(_vr/* LudoJS.$wa6 */(_Hg/* shBi */.a, E(_Hg/* shBi */.b), _GQ/* shzE */, _/* EXTERNAL */)),
                        _Hm/* shBS */ = E(E(_Hl/* shBM */).b),
                        _Hn/* shBY */ = B(_H3/* shAh */(_/* EXTERNAL */, _Hm/* shBS */.a, _Hm/* shBS */.b, _Hm/* shBS */.c, _Hm/* shBS */.d, _Hm/* shBS */.e)),
                        _Ho/* shC4 */ = E(E(_Hn/* shBY */).b);
                        return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _Ho/* shC4 */.a, _Ho/* shC4 */.b, _Ho/* shC4 */.c, _Ho/* shC4 */.d, _Ho/* shC4 */.e);});
                      }
                    }
                  }
                }
                break;
              default:
                var _Hp/* shCc */ = mMV/* EXTERNAL */(E(_FF/* System.Random.theStdGen */), _xc/* LudoJS.lvl43 */);
                return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _b8/* LudoJS.play11 */, new T(function(){
                  switch(E(_Hp/* shCc */)){
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
                      return E(_5B/* LudoJS.numToColor1 */);
                  }
                }), _b7/* LudoJS.play10 */, _xx/* LudoJS.play5 */, _4/* GHC.Types.[] */);});
            }
          }else{
            return _2s/* GHC.Tuple.() */;
          }
        }else{
          return _2s/* GHC.Tuple.() */;
        }
      };
      switch(E(E(_FO/* shvL */).b)){
        case 0:
          return new F(function(){return _FP/* shw2 */(E(_1u/* LudoJS.$fFromAnyGameState14 */));});
          break;
        case 1:
          return new F(function(){return _FP/* shw2 */(E(_1t/* LudoJS.$fFromAnyGameState13 */));});
          break;
        case 2:
          return new F(function(){return _FP/* shw2 */(E(_1s/* LudoJS.$fFromAnyGameState12 */));});
          break;
        default:
          return new F(function(){return _FP/* shw2 */(E(_1r/* LudoJS.$fFromAnyGameState11 */));});
      }
    }else{
      return _2s/* GHC.Tuple.() */;
    }
  }
},
_Hq/* play2 */ = function(_Hr/* shCo */, _/* EXTERNAL */){
  var _Hs/* shCq */ = E(_Hr/* shCo */),
  _Ht/* shCu */ = E(_Hs/* shCq */.a);
  return new F(function(){return _FG/* LudoJS.$wlvl */(_Ht/* shCu */.a, _Ht/* shCu */.b, _Hs/* shCq */.b, _Hs/* shCq */.c, _/* EXTERNAL */);});
},
_Hu/* play4 */ = new T(function(){
  return B(_ae/* GHC.Base.map */(_cl/* LudoJS.$fToAnyOption_$ctoAny */, _4/* GHC.Types.[] */));
}),
_Hv/* play1 */ = function(_Hw/* shCx */, _Hx/* shCy */, _/* EXTERNAL */){
  var _Hy/* shCB */ = B(_ai/* LudoJS.$w$ctoAny */(new T5(0,_b8/* LudoJS.play11 */,_Hw/* shCx */,_b7/* LudoJS.play10 */,_xx/* LudoJS.play5 */,_4/* GHC.Types.[] */))),
  _Hz/* shCF */ = __app1/* EXTERNAL */(E(_dl/* LudoJS.play_f1 */), _Hy/* shCB */),
  _HA/* shCL */ = __lst2arr/* EXTERNAL */(E(_Hu/* LudoJS.play4 */)),
  _HB/* shCR */ = eval/* EXTERNAL */(E(_di/* LudoJS.play3 */)),
  _HC/* shD1 */ = __app3/* EXTERNAL */(E(_HB/* shCR */), _Hy/* shCB */, _HA/* shCL */,  -1),
  _HD/* shD4 */ = B(A(_aF/* Haste.Events.Core.onEvent */,[_93/* Haste.Events.Core.$fMonadEventIO */, _8j/* Haste.Events.Core.$fEventSourceElem1 */, _8i/* Haste.Events.MouseEvents.$fEventMouseEvent */, _Hx/* shCy */, _al/* Haste.Events.MouseEvents.Click */, _Hq/* LudoJS.play2 */, _/* EXTERNAL */]));
  return _2s/* GHC.Tuple.() */;
},
_HE/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */(" found!"));
}),
_HF/* withElem1 */ = function(_HG/* svSB */){
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("No element with ID ", new T(function(){
    return B(_q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(E(_HG/* svSB */)), _HE/* Haste.DOM.JSString.lvl */));
  }))));});
},
_HH/* main1 */ = function(_/* EXTERNAL */){
  var _HI/* ssQ7 */ = function(_HJ/* ssPH */){
    var _HK/* ssPI */ = new T(function(){
      var _HL/* ssPJ */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_HJ/* ssPH */, _/* EXTERNAL */));
      return E(_HL/* ssPJ */);
    }),
    _HM/* ssQ6 */ = function(_HN/* ssPM */){
      var _HO/* ssPN */ = new T(function(){
        var _HP/* ssPR */ = Number/* EXTERNAL */(E(_HN/* ssPM */));
        return jsTrunc/* EXTERNAL */(_HP/* ssPR */);
      });
      return function(_HQ/* ssPY */){
        var _HR/* ssQ1 */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_HQ/* ssPY */, _/* EXTERNAL */));
        return new F(function(){return _1L/* LudoJS.$wconvertCell */(_HK/* ssPI */, E(_HO/* ssPN */), _HR/* ssQ1 */);});
      };
    };
    return E(_HM/* ssQ6 */);
  },
  _HS/* ssQb */ = __createJSFunc/* EXTERNAL */(3, E(_HI/* ssQ7 */)),
  _HT/* ssQj */ = __app2/* EXTERNAL */(E(_1Q/* Main.f2 */), "convertCell", _HS/* ssQb */),
  _HU/* ssQo */ = mMV/* EXTERNAL */(E(_FF/* System.Random.theStdGen */), _5x/* Main.lvl4 */),
  _HV/* ssQv */ = "canvas",
  _HW/* ssQB */ = __app1/* EXTERNAL */(E(_1P/* Haste.DOM.JSString.elemById_f1 */), _HV/* ssQv */),
  _HX/* ssQH */ = __eq/* EXTERNAL */(_HW/* ssQB */, E(_1W/* Haste.Prim.Any.jsNull */));
  if(!E(_HX/* ssQH */)){
    var _HY/* ssQN */ = __isUndef/* EXTERNAL */(_HW/* ssQB */);
    if(!E(_HY/* ssQN */)){
      return new F(function(){return _Hv/* LudoJS.play1 */(new T(function(){
        switch(E(_HU/* ssQo */)){
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
            return E(_5B/* LudoJS.numToColor1 */);
        }
      }), _HW/* ssQB */, _/* EXTERNAL */);});
    }else{
      return new F(function(){return _HF/* Haste.DOM.JSString.withElem1 */(_HV/* ssQv */);});
    }
  }else{
    return new F(function(){return _HF/* Haste.DOM.JSString.withElem1 */(_HV/* ssQv */);});
  }
},
_HZ/* main */ = function(_/* EXTERNAL */){
  return new F(function(){return _HH/* Main.main1 */(_/* EXTERNAL */);});
};

var hasteMain = function() {B(A(_HZ, [0]));};onHasteStart(); hasteMain();