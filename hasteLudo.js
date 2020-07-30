"use strict";
var __haste_prog_id = '8d6608666221eb407e2bc72b126776af3d9e20cc2c134549f95915aa27d55855';
var __haste_script_elem = typeof document == 'object' ? document.currentScript : null;
var gameState;

var c, ctx;
var tileWidth = 50;
var tileHeight = 50;

const canvasScale = 50;
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

function onHasteStart() {
    c = document.getElementById("canvas");
    c.width = numTilesX * canvasScale;
    c.height = numTilesY * canvasScale;
    ctx = c.getContext("2d");

    ctx.imageSmoothingEnabled = false;

    tileWidth = c.width / numTilesX;
    tileHeight = c.height / numTilesY;
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

function drawSolid(posX, posY, r, g, b, border = 1) {
    ctx.fillStyle = cssColor(r, g, b);
    ctx.fillRect(posX + border, posY + border, tileWidth - border * 2, tileHeight - border * 2);
}

function drawGlobe(posX, posY) {
    ctx.beginPath();
    ctx.strokeStyle = "#808080"
    ctx.lineWidth = 3;
    ctx.arc(posX + tileWidth / 2, posY + tileHeight / 2, tileWidth / 2.5, 0, Math.PI*2);
    ctx.stroke();
}

function drawStar(posX, posY) {
    ctx.beginPath();
    ctx.strokeStyle = "#808080"
    ctx.lineWidth = 3;
    ctx.moveTo(posX, posY);
    ctx.lineTo(posX + tileWidth, posY + tileHeight);
    ctx.stroke();
}

function drawPlayer(posX, posY, player = "Green") {
    ctx.beginPath();
    ctx.strokeStyle = cssColor(...playerColorDark[player]);
    ctx.lineWidth = 6;

    ctx.moveTo(posX + tileWidth / 2, posY + tileHeight * 1/4);
    ctx.lineTo(posX + tileWidth * 1/4, posY + tileHeight * 3/4);
    ctx.moveTo(posX + tileWidth / 2, posY + tileHeight * 1/4);
    ctx.lineTo(posX + tileWidth * 3/4, posY + tileHeight * 3/4);
    ctx.stroke();
}

function drawDice(posX, posY, num, color = [255, 255, 255]) {
    ctx.fillStyle = cssColor(...color);
    ctx.font = Math.floor(tileWidth * 0.8) + "px Georgia";
    ctx.fillText(num.toString(), posX + tileWidth * 1/4, posY + tileHeight * 3/5, tileWidth);
}

function drawHighlight(posX, posY, color = [200, 200, 200]) {
    ctx.beginPath();
    ctx.strokeStyle = cssColor(...color);
    ctx.lineWidth = 6;
    
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

var playerPos = 0;

function drawStaticBoard() {
    ctx.fillStyle = "#404040";
    ctx.fillRect(0, 0, c.width, c.height);

    for (let x = 0; x < numTilesX; x++) {
        for (let y = 0; y < numTilesY; y++) {
            drawTiles([[x, y]], drawSolid, 100, 100, 100)
        }
    }

    drawTiles(boardPositions, drawSolid, 255, 255, 255);

    drawTiles([...homePositions.Green, ...outPositions.Green, startPosition.Green], drawSolid, ...playerColor.Green);
    drawTiles([...homePositions.Yellow, ...outPositions.Yellow, startPosition.Yellow], drawSolid, ...playerColor.Yellow);
    drawTiles([...homePositions.Red, ...outPositions.Red, startPosition.Red], drawSolid, ...playerColor.Red);
    drawTiles([...homePositions.Blue, ...outPositions.Blue, startPosition.Blue], drawSolid, ...playerColor.Blue);

    drawTiles(globeCells.map((v, i) => boardPositions[v]), drawGlobe);
    drawTiles(starCells.map((v, i) => boardPositions[v]), drawStar);
}

function drawBoard(gameState, options, roll) {
    console.log(gameState);
    console.log(options);
    console.log(roll);

    drawStaticBoard();

    // Draw dice
    drawTiles(dicePositions, drawDice, roll, playerColorDark[gameState.turn]);

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
            drawTiles(dicePositions, drawHighlight);
        break;

        case "SelectPiece":
            for (let option of options) {
                if (option.option == "Play") {
                    drawTiles([outPositions[player][option.piece - 1]], drawHighlight);
                }
                else if (option.option == "Move") {
                    let pos = gameState.pieces[player][option.piece].field;
                    drawTiles([playerPosToTilePos(player, pos)], drawHighlight);
                }
            }

        break;

        case "SelectField":
            for (let option of options) {
                if (option.piece == gameState.stage.pieceIndex) {
                    if (option.option == "Play") {
                        drawTiles([startPosition[player]], drawHighlight);
                    }
                    else if (option.option == "Move") {
                        drawTiles([playerPosToTilePos(player, option.field)], drawHighlight);
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
_1A/* $fFromAnyGameState9 */ = function(_1B/* sgTf */, _/* EXTERNAL */){
  return new T(function(){
    var _1C/* sgTk */ = String/* EXTERNAL */(E(_1B/* sgTf */)),
    _1D/* sgTp */ = fromJSStr/* EXTERNAL */(_1C/* sgTk */);
    if(!B(_1v/* GHC.Base.eqString */(_1D/* sgTp */, _1u/* LudoJS.$fFromAnyGameState14 */))){
      if(!B(_1v/* GHC.Base.eqString */(_1D/* sgTp */, _1t/* LudoJS.$fFromAnyGameState13 */))){
        if(!B(_1v/* GHC.Base.eqString */(_1D/* sgTp */, _1s/* LudoJS.$fFromAnyGameState12 */))){
          if(!B(_1v/* GHC.Base.eqString */(_1D/* sgTp */, _1r/* LudoJS.$fFromAnyGameState11 */))){
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
_1L/* $wconvertCell */ = function(_1M/* sgUU */, _1N/* sgUV */, _1O/* sgUW */){
  switch(E(_1O/* sgUW */)){
    case 0:
      switch(E(_1M/* sgUU */)){
        case 0:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
      break;
    case 1:
      switch(E(_1M/* sgUU */)){
        case 0:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
      break;
    case 2:
      switch(E(_1M/* sgUU */)){
        case 0:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
      }
      break;
    default:
      switch(E(_1M/* sgUU */)){
        case 0:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52);});
          break;
        case 1:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52);});
          break;
        case 2:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52);});
          break;
        default:
          return new F(function(){return _1E/* GHC.Classes.modInt# */(_1N/* sgUV */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52);});
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
_99/* $w$ctoAny1 */ = function(_9a/* shAS */){
  switch(E(_9a/* shAS */)){
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
_9b/* $fToAnyGameState_$ctoAny1 */ = function(_9c/* shBe */){
  return new F(function(){return _99/* LudoJS.$w$ctoAny1 */(_9c/* shBe */);});
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
_9I/* $fToAnyGameState_$ctoAny2 */ = function(_9J/* shbd */){
  var _9K/* shbe */ = E(_9J/* shbd */);
  switch(_9K/* shbe */._){
    case 0:
      var _9L/* shbg */ = E(_9K/* shbe */.a);
      if(!_9L/* shbg */._){
        return E(_9C/* LudoJS.$fToAnyGameState14 */);
      }else{
        return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,_9u/* LudoJS.$fToAnyGameState12 */,new T2(1,new T2(0,_9s/* LudoJS.$fToAnyGameState11 */,_9L/* shbg */.a),_4/* GHC.Types.[] */)));});
      }
      break;
    case 1:
      return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,_9H/* LudoJS.$fToAnyGameState9 */,new T2(1,new T2(0,_9s/* LudoJS.$fToAnyGameState11 */,_9K/* shbe */.a),_4/* GHC.Types.[] */)));});
      break;
    case 2:
      return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,_9F/* LudoJS.$fToAnyGameState7 */,new T2(1,new T2(0,_9s/* LudoJS.$fToAnyGameState11 */,_9K/* shbe */.a),new T2(1,new T2(0,_9D/* LudoJS.$fToAnyGameState6 */,_9K/* shbe */.b),_4/* GHC.Types.[] */))));});
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
_9V/* $fToAnyPiece_$ctoAny */ = function(_9W/* shaZ */){
  var _9X/* shb0 */ = E(_9W/* shaZ */);
  if(!_9X/* shb0 */._){
    return E(_9U/* LudoJS.$fToAnyPiece3 */);
  }else{
    return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,_9P/* LudoJS.$fToAnyPiece1 */,new T2(1,new T2(0,_9M/* LudoJS.$fToAnyOption1 */,_9X/* shb0 */.a),_4/* GHC.Types.[] */)));});
  }
},
_9Y/* go1 */ = function(_9Z/* shBn */){
  var _a0/* shBo */ = E(_9Z/* shBn */);
  if(!_a0/* shBo */._){
    return __Z/* EXTERNAL */;
  }else{
    var _a1/* shBr */ = E(_a0/* shBo */.a);
    return new T2(1,new T2(0,new T(function(){
      return toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, E(_a1/* shBr */.a), _4/* GHC.Types.[] */)));
    }),new T(function(){
      return B(_9V/* LudoJS.$fToAnyPiece_$ctoAny */(_a1/* shBr */.b));
    })),new T(function(){
      return B(_9Y/* LudoJS.go1 */(_a0/* shBo */.b));
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
_a6/* $fToAnyGameState_go10 */ = function(_a7/*  shBD */, _a8/*  shBE */){
  while(1){
    var _a9/*  $fToAnyGameState_go10 */ = B((function(_aa/* shBD */, _ab/* shBE */){
      var _ac/* shBF */ = E(_ab/* shBE */);
      if(!_ac/* shBF */._){
        var _ad/* shC1 */ = new T(function(){
          return B(_9h/* Haste.Prim.Any.$wtoObject */(new T(function(){
            return B(_9Y/* LudoJS.go1 */(_ac/* shBF */.c));
          },1)));
        });
        _a7/*  shBD */ = new T2(1,new T2(0,new T(function(){
          switch(E(_ac/* shBF */.b)){
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
        }),_ad/* shC1 */),new T(function(){
          return B(_a6/* LudoJS.$fToAnyGameState_go10 */(_aa/* shBD */, _ac/* shBF */.e));
        }));
        _a8/*  shBE */ = _ac/* shBF */.d;
        return __continue/* EXTERNAL */;
      }else{
        return E(_aa/* shBD */);
      }
    })(_a7/*  shBD */, _a8/*  shBE */));
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
_ai/* $w$ctoAny */ = function(_aj/* shCf */){
  var _ak/* shCZ */ = new T(function(){
    return B(_9h/* Haste.Prim.Any.$wtoObject */(new T(function(){
      return B(_a6/* LudoJS.$fToAnyGameState_go10 */(_4/* GHC.Types.[] */, E(_aj/* shCf */).d));
    },1)));
  });
  return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,new T2(0,_95/* LudoJS.$fFromAnyGameState16 */,new T(function(){
    return B(_9I/* LudoJS.$fToAnyGameState_$ctoAny2 */(E(_aj/* shCf */).a));
  })),new T2(1,new T2(0,_94/* LudoJS.$fFromAnyGameState15 */,new T(function(){
    switch(E(E(_aj/* shCf */).b)){
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
    return E(E(_aj/* shCf */).c);
  })),new T2(1,new T2(0,_97/* LudoJS.$fFromAnyGameState7 */,_ak/* shCZ */),new T2(1,new T2(0,_96/* LudoJS.$fFromAnyGameState5 */,new T(function(){
    return __lst2arr/* EXTERNAL */(B(_ae/* GHC.Base.map */(_9b/* LudoJS.$fToAnyGameState_$ctoAny1 */, E(_aj/* shCf */).e)));
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
_bc/* $fEqOption_$c== */ = function(_bd/* sher */, _be/* shes */){
  var _bf/* shet */ = E(_bd/* sher */);
  if(!_bf/* shet */._){
    var _bg/* shev */ = E(_be/* shes */);
    if(!_bg/* shev */._){
      return new F(function(){return _b9/* GHC.Classes.eqInt */(_bf/* shet */.a, _bg/* shev */.a);});
    }else{
      return false;
    }
  }else{
    var _bh/* sheB */ = E(_be/* shes */);
    if(!_bh/* sheB */._){
      return false;
    }else{
      if(E(_bf/* shet */.a)!=E(_bh/* sheB */.a)){
        return false;
      }else{
        return new F(function(){return _b9/* GHC.Classes.eqInt */(_bf/* shet */.b, _bh/* sheB */.b);});
      }
    }
  }
},
_bi/* $fEqOption_$c/= */ = function(_bj/* sheL */, _bk/* sheM */){
  return (!B(_bc/* LudoJS.$fEqOption_$c== */(_bj/* sheL */, _bk/* sheM */))) ? true : false;
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
_bt/* $s!1 */ = function(_bu/* sgKm */, _bv/* sgKn */){
  while(1){
    var _bw/* sgKo */ = E(_bv/* sgKn */);
    if(!_bw/* sgKo */._){
      var _bx/* sgKq */ = _bw/* sgKo */.b,
      _by/* sgKr */ = _bw/* sgKo */.c,
      _bz/* sgKs */ = _bw/* sgKo */.d,
      _bA/* sgKt */ = _bw/* sgKo */.e;
      switch(E(_bu/* sgKm */)){
        case 0:
          switch(E(_bx/* sgKq */)){
            case 0:
              return E(_by/* sgKr */);
            case 1:
              _bu/* sgKm */ = _bn/* LudoJS.Blue */;
              _bv/* sgKn */ = _bz/* sgKs */;
              continue;
            case 2:
              _bu/* sgKm */ = _bn/* LudoJS.Blue */;
              _bv/* sgKn */ = _bz/* sgKs */;
              continue;
            default:
              _bu/* sgKm */ = _bn/* LudoJS.Blue */;
              _bv/* sgKn */ = _bz/* sgKs */;
              continue;
          }
          break;
        case 1:
          switch(E(_bx/* sgKq */)){
            case 0:
              _bu/* sgKm */ = _bo/* LudoJS.Green */;
              _bv/* sgKn */ = _bA/* sgKt */;
              continue;
            case 1:
              return E(_by/* sgKr */);
            case 2:
              _bu/* sgKm */ = _bo/* LudoJS.Green */;
              _bv/* sgKn */ = _bz/* sgKs */;
              continue;
            default:
              _bu/* sgKm */ = _bo/* LudoJS.Green */;
              _bv/* sgKn */ = _bz/* sgKs */;
              continue;
          }
          break;
        case 2:
          switch(E(_bx/* sgKq */)){
            case 2:
              return E(_by/* sgKr */);
            case 3:
              _bu/* sgKm */ = _bp/* LudoJS.Red */;
              _bv/* sgKn */ = _bz/* sgKs */;
              continue;
            default:
              _bu/* sgKm */ = _bp/* LudoJS.Red */;
              _bv/* sgKn */ = _bA/* sgKt */;
              continue;
          }
          break;
        default:
          if(E(_bx/* sgKq */)==3){
            return E(_by/* sgKr */);
          }else{
            _bu/* sgKm */ = _bq/* LudoJS.Yellow */;
            _bv/* sgKn */ = _bA/* sgKt */;
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
_bG/* $sa */ = function(_bH/*  shI2 */, _bI/*  shI3 */, _bJ/*  shI4 */, _bK/*  shI5 */, _bL/*  shI6 */, _bM/*  shI7 */, _bN/*  shI8 */, _/* EXTERNAL */){
  while(1){
    var _bO/*  $sa */ = B((function(_bP/* shI2 */, _bQ/* shI3 */, _bR/* shI4 */, _bS/* shI5 */, _bT/* shI6 */, _bU/* shI7 */, _bV/* shI8 */, _/* EXTERNAL */){
      var _bW/* shIa */ = E(_bP/* shI2 */);
      if(!_bW/* shIa */._){
        return new T2(0,_bQ/* shI3 */,new T5(0,_bR/* shI4 */,_bS/* shI5 */,_bT/* shI6 */,_bU/* shI7 */,_bV/* shI8 */));
      }else{
        var _bX/* shId */ = _bW/* shIa */.a,
        _bY/*   shI4 */ = _bR/* shI4 */,
        _bZ/*   shI5 */ = _bS/* shI5 */,
        _c0/*   shI6 */ = _bT/* shI6 */,
        _c1/*   shI7 */ = _bU/* shI7 */,
        _c2/*   shI8 */ = _bV/* shI8 */;
        _bH/*  shI2 */ = _bW/* shIa */.b;
        _bI/*  shI3 */ = new T(function(){
          if(!B(_bB/* GHC.List.$wlenAcc */(B(_bt/* LudoJS.$s!1 */(_bX/* shId */, _bU/* shI7 */)), 0))){
            return E(_bQ/* shI3 */);
          }else{
            return B(_q/* GHC.Base.++ */(_bQ/* shI3 */, new T2(1,_bX/* shId */,_4/* GHC.Types.[] */)));
          }
        });
        _bJ/*  shI4 */ = _bY/*   shI4 */;
        _bK/*  shI5 */ = _bZ/*   shI5 */;
        _bL/*  shI6 */ = _c0/*   shI6 */;
        _bM/*  shI7 */ = _c1/*   shI7 */;
        _bN/*  shI8 */ = _c2/*   shI8 */;
        return __continue/* EXTERNAL */;
      }
    })(_bH/*  shI2 */, _bI/*  shI3 */, _bJ/*  shI4 */, _bK/*  shI5 */, _bL/*  shI6 */, _bM/*  shI7 */, _bN/*  shI8 */, _/* EXTERNAL */));
    if(_bO/*  $sa */!=__continue/* EXTERNAL */){
      return _bO/*  $sa */;
    }
  }
},
_c3/* $sa1 */ = function(_c4/* shIj */, _c5/* shIk */, _c6/* shIl */, _c7/* shIm */, _c8/* shIn */, _c9/* shIo */, _ca/* shIp */, _cb/* shIq */, _/* EXTERNAL */){
  return new F(function(){return _bG/* LudoJS.$sa */(_c5/* shIk */, new T(function(){
    if(!B(_bB/* GHC.List.$wlenAcc */(B(_bt/* LudoJS.$s!1 */(_c4/* shIj */, _ca/* shIp */)), 0))){
      return E(_c6/* shIl */);
    }else{
      return B(_q/* GHC.Base.++ */(_c6/* shIl */, new T2(1,_c4/* shIj */,_4/* GHC.Types.[] */)));
    }
  }), _c7/* shIm */, _c8/* shIn */, _c9/* shIo */, _ca/* shIp */, _cb/* shIq */, _/* EXTERNAL */);});
},
_cc/* $fFromAny()4 */ = function(_/* EXTERNAL */){
  return _2s/* GHC.Tuple.() */;
},
_cd/* $fToAnyOption3 */ = "Move",
_ce/* $fToAnyOption4 */ = "option",
_cf/* $fToAnyOption2 */ = new T2(0,_ce/* LudoJS.$fToAnyOption4 */,_cd/* LudoJS.$fToAnyOption3 */),
_cg/* $fToAnyOption7 */ = "Play",
_ch/* $fToAnyOption6 */ = new T2(0,_ce/* LudoJS.$fToAnyOption4 */,_cg/* LudoJS.$fToAnyOption7 */),
_ci/* $w$ctoAny2 */ = function(_cj/* shbG */){
  var _ck/* shbH */ = E(_cj/* shbG */);
  if(!_ck/* shbH */._){
    return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,_ch/* LudoJS.$fToAnyOption6 */,new T2(1,new T2(0,_9N/* LudoJS.$fToAnyOption5 */,_ck/* shbH */.a),_4/* GHC.Types.[] */)));});
  }else{
    return new F(function(){return _9h/* Haste.Prim.Any.$wtoObject */(new T2(1,_cf/* LudoJS.$fToAnyOption2 */,new T2(1,new T2(0,_9N/* LudoJS.$fToAnyOption5 */,_ck/* shbH */.a),new T2(1,new T2(0,_9M/* LudoJS.$fToAnyOption1 */,_ck/* shbH */.b),_4/* GHC.Types.[] */))));});
  }
},
_cl/* $fToAnyOption_$ctoAny */ = function(_cm/* shbT */){
  return new F(function(){return _ci/* LudoJS.$w$ctoAny2 */(_cm/* shbT */);});
},
_cn/* a42 */ = function(_co/* sgR0 */, _cp/* sgR1 */, _/* EXTERNAL */){
  var _cq/* sgTd */ = new T(function(){
    var _cr/* sgR3 */ = E(_cp/* sgR1 */),
    _cs/* sgR9 */ = function(_ct/* sgRa */){
      var _cu/* sgRb */ = E(_ct/* sgRa */);
      if(!_cu/* sgRb */._){
        return __Z/* EXTERNAL */;
      }else{
        var _cv/* sgRd */ = _cu/* sgRb */.b,
        _cw/* sgRe */ = E(_cu/* sgRb */.a),
        _cx/* sgRf */ = _cw/* sgRe */.a,
        _cy/* sgRh */ = E(_cw/* sgRe */.b);
        if(!_cy/* sgRh */._){
          var _cz/* sgRk */ = E(_co/* sgR0 */);
          if(_cz/* sgRk */==6){
            var _cA/* sgS6 */ = new T(function(){
              var _cB/* sgRI */ = function(_cC/*  sgRJ */){
                while(1){
                  var _cD/*  sgRI */ = B((function(_cE/* sgRJ */){
                    var _cF/* sgRK */ = E(_cE/* sgRJ */);
                    if(!_cF/* sgRK */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _cG/* sgRM */ = _cF/* sgRK */.b,
                      _cH/* sgRN */ = E(_cF/* sgRK */.a),
                      _cI/* sgRO */ = _cH/* sgRN */.a,
                      _cJ/* sgRQ */ = E(_cH/* sgRN */.b);
                      if(!_cJ/* sgRQ */._){
                        return new T2(1,new T1(0,_cI/* sgRO */),new T(function(){
                          return B(_cB/* sgRI */(_cG/* sgRM */));
                        }));
                      }else{
                        var _cK/* sgRU */ = E(_cJ/* sgRQ */.a);
                        if(_cK/* sgRU */>=51){
                          if((6+_cK/* sgRU */|0)==56){
                            return new T2(1,new T2(1,_cI/* sgRO */,56),new T(function(){
                              return B(_cB/* sgRI */(_cG/* sgRM */));
                            }));
                          }else{
                            _cC/*  sgRJ */ = _cG/* sgRM */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          return new T2(1,new T2(1,_cI/* sgRO */,6+_cK/* sgRU */|0),new T(function(){
                            return B(_cB/* sgRI */(_cG/* sgRM */));
                          }));
                        }
                      }
                    }
                  })(_cC/*  sgRJ */));
                  if(_cD/*  sgRI */!=__continue/* EXTERNAL */){
                    return _cD/*  sgRI */;
                  }
                }
              };
              return B(_cB/* sgRI */(_cv/* sgRd */));
            });
            return new T2(1,new T1(0,_cx/* sgRf */),_cA/* sgS6 */);
          }else{
            var _cL/* sgRl */ = function(_cM/*  sgRm */){
              while(1){
                var _cN/*  sgRl */ = B((function(_cO/* sgRm */){
                  var _cP/* sgRn */ = E(_cO/* sgRm */);
                  if(!_cP/* sgRn */._){
                    return __Z/* EXTERNAL */;
                  }else{
                    var _cQ/* sgRp */ = _cP/* sgRn */.b,
                    _cR/* sgRq */ = E(_cP/* sgRn */.a),
                    _cS/* sgRr */ = _cR/* sgRq */.a,
                    _cT/* sgRt */ = E(_cR/* sgRq */.b);
                    if(!_cT/* sgRt */._){
                      _cM/*  sgRm */ = _cQ/* sgRp */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _cU/* sgRv */ = E(_cT/* sgRt */.a);
                      if(_cU/* sgRv */>=51){
                        if((_cz/* sgRk */+_cU/* sgRv */|0)==56){
                          return new T2(1,new T2(1,_cS/* sgRr */,56),new T(function(){
                            return B(_cL/* sgRl */(_cQ/* sgRp */));
                          }));
                        }else{
                          _cM/*  sgRm */ = _cQ/* sgRp */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        return new T2(1,new T2(1,_cS/* sgRr */,_cz/* sgRk */+_cU/* sgRv */|0),new T(function(){
                          return B(_cL/* sgRl */(_cQ/* sgRp */));
                        }));
                      }
                    }
                  }
                })(_cM/*  sgRm */));
                if(_cN/*  sgRl */!=__continue/* EXTERNAL */){
                  return _cN/*  sgRl */;
                }
              }
            };
            return new F(function(){return _cL/* sgRl */(_cv/* sgRd */);});
          }
        }else{
          var _cV/* sgS8 */ = E(_cy/* sgRh */.a);
          if(_cV/* sgS8 */>=51){
            var _cW/* sgSc */ = E(_co/* sgR0 */);
            if((_cW/* sgSc */+_cV/* sgS8 */|0)==56){
              var _cX/* sgT5 */ = new T(function(){
                var _cY/* sgSG */ = function(_cZ/*  sgSH */){
                  while(1){
                    var _d0/*  sgSG */ = B((function(_d1/* sgSH */){
                      var _d2/* sgSI */ = E(_d1/* sgSH */);
                      if(!_d2/* sgSI */._){
                        return __Z/* EXTERNAL */;
                      }else{
                        var _d3/* sgSK */ = _d2/* sgSI */.b,
                        _d4/* sgSL */ = E(_d2/* sgSI */.a),
                        _d5/* sgSM */ = _d4/* sgSL */.a,
                        _d6/* sgSO */ = E(_d4/* sgSL */.b);
                        if(!_d6/* sgSO */._){
                          if(E(_cW/* sgSc */)==6){
                            return new T2(1,new T1(0,_d5/* sgSM */),new T(function(){
                              return B(_cY/* sgSG */(_d3/* sgSK */));
                            }));
                          }else{
                            _cZ/*  sgSH */ = _d3/* sgSK */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          var _d7/* sgST */ = E(_d6/* sgSO */.a);
                          if(_d7/* sgST */>=51){
                            if((_cW/* sgSc */+_d7/* sgST */|0)==56){
                              return new T2(1,new T2(1,_d5/* sgSM */,56),new T(function(){
                                return B(_cY/* sgSG */(_d3/* sgSK */));
                              }));
                            }else{
                              _cZ/*  sgSH */ = _d3/* sgSK */;
                              return __continue/* EXTERNAL */;
                            }
                          }else{
                            return new T2(1,new T2(1,_d5/* sgSM */,_cW/* sgSc */+_d7/* sgST */|0),new T(function(){
                              return B(_cY/* sgSG */(_d3/* sgSK */));
                            }));
                          }
                        }
                      }
                    })(_cZ/*  sgSH */));
                    if(_d0/*  sgSG */!=__continue/* EXTERNAL */){
                      return _d0/*  sgSG */;
                    }
                  }
                };
                return B(_cY/* sgSG */(_cv/* sgRd */));
              });
              return new T2(1,new T2(1,_cx/* sgRf */,56),_cX/* sgT5 */);
            }else{
              var _d8/* sgSf */ = function(_d9/*  sgSg */){
                while(1){
                  var _da/*  sgSf */ = B((function(_db/* sgSg */){
                    var _dc/* sgSh */ = E(_db/* sgSg */);
                    if(!_dc/* sgSh */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _dd/* sgSj */ = _dc/* sgSh */.b,
                      _de/* sgSk */ = E(_dc/* sgSh */.a),
                      _df/* sgSl */ = _de/* sgSk */.a,
                      _dg/* sgSn */ = E(_de/* sgSk */.b);
                      if(!_dg/* sgSn */._){
                        if(E(_cW/* sgSc */)==6){
                          return new T2(1,new T1(0,_df/* sgSl */),new T(function(){
                            return B(_d8/* sgSf */(_dd/* sgSj */));
                          }));
                        }else{
                          _d9/*  sgSg */ = _dd/* sgSj */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        var _dh/* sgSs */ = E(_dg/* sgSn */.a);
                        if(_dh/* sgSs */>=51){
                          if((_cW/* sgSc */+_dh/* sgSs */|0)==56){
                            return new T2(1,new T2(1,_df/* sgSl */,56),new T(function(){
                              return B(_d8/* sgSf */(_dd/* sgSj */));
                            }));
                          }else{
                            _d9/*  sgSg */ = _dd/* sgSj */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          return new T2(1,new T2(1,_df/* sgSl */,_cW/* sgSc */+_dh/* sgSs */|0),new T(function(){
                            return B(_d8/* sgSf */(_dd/* sgSj */));
                          }));
                        }
                      }
                    }
                  })(_d9/*  sgSg */));
                  if(_da/*  sgSf */!=__continue/* EXTERNAL */){
                    return _da/*  sgSf */;
                  }
                }
              };
              return new F(function(){return _d8/* sgSf */(_cv/* sgRd */);});
            }
          }else{
            return new T2(1,new T2(1,_cx/* sgRf */,new T(function(){
              return E(_co/* sgR0 */)+_cV/* sgS8 */|0;
            })),new T(function(){
              return B(_cs/* sgR9 */(_cv/* sgRd */));
            }));
          }
        }
      }
    };
    return B(_cs/* sgR9 */(B(_bt/* LudoJS.$s!1 */(_cr/* sgR3 */.b, _cr/* sgR3 */.d))));
  });
  return new T2(0,_cq/* sgTd */,_cp/* sgR1 */);
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
_dm/* $w$j */ = function(_/* EXTERNAL */, _dn/* shGv */, _do/* shGw */, _dp/* shGx */, _dq/* shGy */, _dr/* shGz */){
  var _ds/* shGA */ = function(_dt/* shGB */, _du/* shGC */){
    var _dv/* shGF */ = new T5(0,_dn/* shGv */,_do/* shGw */,_dp/* shGx */,_dq/* shGy */,_dr/* shGz */),
    _dw/* shGG */ = function(_/* EXTERNAL */, _dx/* shGI */, _dy/* shGJ */){
      var _dz/* shGO */ = __lst2arr/* EXTERNAL */(B(_ae/* GHC.Base.map */(_cl/* LudoJS.$fToAnyOption_$ctoAny */, _dx/* shGI */))),
      _dA/* shGU */ = __app3/* EXTERNAL */(E(_dj/* LudoJS.f1 */), B(_ai/* LudoJS.$w$ctoAny */(_dv/* shGF */)), _dz/* shGO */, _dt/* shGB */);
      return new T2(0,_2s/* GHC.Tuple.() */,_dy/* shGJ */);
    };
    if(E(_dt/* shGB */)==( -1)){
      var _dB/* shHo */ = B(_dw/* shGG */(_/* EXTERNAL */, _4/* GHC.Types.[] */, _dv/* shGF */)),
      _dC/* shHE */ = __app1/* EXTERNAL */(E(_dl/* LudoJS.play_f1 */), B(_ai/* LudoJS.$w$ctoAny */(E(E(_dB/* shHo */).b))));
      return new F(function(){return _cc/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
    }else{
      var _dD/* shGZ */ = B(_cn/* LudoJS.a42 */(_du/* shGC */, _dv/* shGF */, _/* EXTERNAL */)),
      _dE/* shH2 */ = E(_dD/* shGZ */),
      _dF/* shH5 */ = B(_dw/* shGG */(_/* EXTERNAL */, _dE/* shH2 */.a, _dE/* shH2 */.b)),
      _dG/* shHl */ = __app1/* EXTERNAL */(E(_dl/* LudoJS.play_f1 */), B(_ai/* LudoJS.$w$ctoAny */(E(E(_dF/* shH5 */).b))));
      return new F(function(){return _cc/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
    }
  },
  _dH/* shHH */ = E(_dn/* shGv */);
  switch(_dH/* shHH */._){
    case 0:
      var _dI/* shHJ */ = E(_dH/* shHH */.a);
      if(!_dI/* shHJ */._){
        return new F(function(){return _ds/* shGA */( -1, _9v/* LudoJS.$fToAnyGameState18 */);});
      }else{
        var _dJ/* shHL */ = E(_dI/* shHJ */.a);
        return new F(function(){return _ds/* shGA */(_dJ/* shHL */, _dJ/* shHL */);});
      }
      break;
    case 1:
      var _dK/* shHO */ = E(_dH/* shHH */.a);
      return new F(function(){return _ds/* shGA */(_dK/* shHO */, _dK/* shHO */);});
      break;
    case 2:
      var _dL/* shHS */ = E(_dH/* shHH */.a);
      return new F(function(){return _ds/* shGA */(_dL/* shHS */, _dL/* shHS */);});
      break;
    default:
      return E(_dk/* LudoJS.lvl14 */);
  }
},
_dM/* $fFromAnyGameState4 */ = function(_dN/* shc2 */, _/* EXTERNAL */){
  var _dO/* shc4 */ = E(_dN/* shc2 */);
  if(!_dO/* shc4 */._){
    return _4/* GHC.Types.[] */;
  }else{
    var _dP/* shc7 */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_dO/* shc4 */.a, _/* EXTERNAL */)),
    _dQ/* shca */ = B(_dM/* LudoJS.$fFromAnyGameState4 */(_dO/* shc4 */.b, _/* EXTERNAL */));
    return new T2(1,_dP/* shc7 */,_dQ/* shca */);
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
_fi/* $sinsert_$sgo10 */ = function(_fj/* sgMy */, _fk/* sgMz */, _fl/* sgMA */){
  var _fm/* sgMB */ = E(_fj/* sgMy */),
  _fn/* sgMC */ = E(_fl/* sgMA */);
  if(!_fn/* sgMC */._){
    var _fo/* sgMD */ = _fn/* sgMC */.a,
    _fp/* sgME */ = _fn/* sgMC */.b,
    _fq/* sgMF */ = _fn/* sgMC */.c,
    _fr/* sgMG */ = _fn/* sgMC */.d,
    _fs/* sgMH */ = _fn/* sgMC */.e;
    switch(E(_fm/* sgMB */)){
      case 0:
        switch(E(_fp/* sgME */)){
          case 0:
            return new T5(0,_fo/* sgMD */,E(_bn/* LudoJS.Blue */),_fk/* sgMz */,E(_fr/* sgMG */),E(_fs/* sgMH */));
          case 1:
            return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bo/* LudoJS.Green */, _fq/* sgMF */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bn/* LudoJS.Blue */, _fk/* sgMz */, _fr/* sgMG */)), _fs/* sgMH */);});
            break;
          case 2:
            return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bp/* LudoJS.Red */, _fq/* sgMF */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bn/* LudoJS.Blue */, _fk/* sgMz */, _fr/* sgMG */)), _fs/* sgMH */);});
            break;
          default:
            return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bq/* LudoJS.Yellow */, _fq/* sgMF */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bn/* LudoJS.Blue */, _fk/* sgMz */, _fr/* sgMG */)), _fs/* sgMH */);});
        }
        break;
      case 1:
        switch(E(_fp/* sgME */)){
          case 0:
            return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bn/* LudoJS.Blue */, _fq/* sgMF */, _fr/* sgMG */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bo/* LudoJS.Green */, _fk/* sgMz */, _fs/* sgMH */)));});
            break;
          case 1:
            return new T5(0,_fo/* sgMD */,E(_bo/* LudoJS.Green */),_fk/* sgMz */,E(_fr/* sgMG */),E(_fs/* sgMH */));
          case 2:
            return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bp/* LudoJS.Red */, _fq/* sgMF */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bo/* LudoJS.Green */, _fk/* sgMz */, _fr/* sgMG */)), _fs/* sgMH */);});
            break;
          default:
            return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bq/* LudoJS.Yellow */, _fq/* sgMF */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bo/* LudoJS.Green */, _fk/* sgMz */, _fr/* sgMG */)), _fs/* sgMH */);});
        }
        break;
      case 2:
        switch(E(_fp/* sgME */)){
          case 0:
            return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bn/* LudoJS.Blue */, _fq/* sgMF */, _fr/* sgMG */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bp/* LudoJS.Red */, _fk/* sgMz */, _fs/* sgMH */)));});
            break;
          case 1:
            return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bo/* LudoJS.Green */, _fq/* sgMF */, _fr/* sgMG */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bp/* LudoJS.Red */, _fk/* sgMz */, _fs/* sgMH */)));});
            break;
          case 2:
            return new T5(0,_fo/* sgMD */,E(_bp/* LudoJS.Red */),_fk/* sgMz */,E(_fr/* sgMG */),E(_fs/* sgMH */));
          default:
            return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bq/* LudoJS.Yellow */, _fq/* sgMF */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bp/* LudoJS.Red */, _fk/* sgMz */, _fr/* sgMG */)), _fs/* sgMH */);});
        }
        break;
      default:
        switch(E(_fp/* sgME */)){
          case 0:
            return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bn/* LudoJS.Blue */, _fq/* sgMF */, _fr/* sgMG */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bq/* LudoJS.Yellow */, _fk/* sgMz */, _fs/* sgMH */)));});
            break;
          case 1:
            return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bo/* LudoJS.Green */, _fq/* sgMF */, _fr/* sgMG */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bq/* LudoJS.Yellow */, _fk/* sgMz */, _fs/* sgMH */)));});
            break;
          case 2:
            return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bp/* LudoJS.Red */, _fq/* sgMF */, _fr/* sgMG */, B(_fi/* LudoJS.$sinsert_$sgo10 */(_bq/* LudoJS.Yellow */, _fk/* sgMz */, _fs/* sgMH */)));});
            break;
          default:
            return new T5(0,_fo/* sgMD */,E(_bq/* LudoJS.Yellow */),_fk/* sgMz */,E(_fr/* sgMG */),E(_fs/* sgMH */));
        }
    }
  }else{
    return new T5(0,1,E(_fm/* sgMB */),_fk/* sgMz */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
  }
},
_ft/* poly_go10 */ = function(_fu/* sgNb */, _fv/* sgNc */){
  while(1){
    var _fw/* sgNd */ = E(_fv/* sgNc */);
    if(!_fw/* sgNd */._){
      return E(_fu/* sgNb */);
    }else{
      var _fx/* sgNg */ = E(_fw/* sgNd */.a),
      _fy/*  sgNb */ = B(_fi/* LudoJS.$sinsert_$sgo10 */(_fx/* sgNg */.a, _fx/* sgNg */.b, _fu/* sgNb */));
      _fu/* sgNb */ = _fy/*  sgNb */;
      _fv/* sgNc */ = _fw/* sgNd */.b;
      continue;
    }
  }
},
_fz/* $sfromList_$spoly_go10 */ = function(_fA/* sgN6 */, _fB/* sgN7 */, _fC/* sgN8 */, _fD/* sgN9 */){
  return new F(function(){return _ft/* LudoJS.poly_go10 */(B(_fi/* LudoJS.$sinsert_$sgo10 */(_fB/* sgN7 */, _fC/* sgN8 */, _fA/* sgN6 */)), _fD/* sgN9 */);});
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
_gK/* $s$wpoly_create */ = function(_gL/* sgKz */, _gM/* sgKA */, _gN/* sgKB */, _gO/* sgKC */){
  var _gP/* sgKD */ = E(_gL/* sgKz */);
  if(_gP/* sgKD */==1){
    var _gQ/* sgLB */ = E(_gO/* sgKC */);
    if(!_gQ/* sgLB */._){
      return new T3(0,new T(function(){
        return new T5(0,1,E(_gM/* sgKA */),_gN/* sgKB */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
      }),_4/* GHC.Types.[] */,_4/* GHC.Types.[] */);
    }else{
      var _gR/* sgLH */ = E(_gQ/* sgLB */.a).a;
      switch(E(_gM/* sgKA */)){
        case 0:
          switch(E(_gR/* sgLH */)){
            case 0:
              return new T3(0,new T5(0,1,E(_bn/* LudoJS.Blue */),_gN/* sgKB */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_gQ/* sgLB */);
            case 1:
              return new T3(0,new T5(0,1,E(_bn/* LudoJS.Blue */),_gN/* sgKB */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_gQ/* sgLB */,_4/* GHC.Types.[] */);
            case 2:
              return new T3(0,new T5(0,1,E(_bn/* LudoJS.Blue */),_gN/* sgKB */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_gQ/* sgLB */,_4/* GHC.Types.[] */);
            default:
              return new T3(0,new T5(0,1,E(_bn/* LudoJS.Blue */),_gN/* sgKB */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_gQ/* sgLB */,_4/* GHC.Types.[] */);
          }
          break;
        case 1:
          switch(E(_gR/* sgLH */)){
            case 2:
              return new T3(0,new T5(0,1,E(_bo/* LudoJS.Green */),_gN/* sgKB */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_gQ/* sgLB */,_4/* GHC.Types.[] */);
            case 3:
              return new T3(0,new T5(0,1,E(_bo/* LudoJS.Green */),_gN/* sgKB */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_gQ/* sgLB */,_4/* GHC.Types.[] */);
            default:
              return new T3(0,new T5(0,1,E(_bo/* LudoJS.Green */),_gN/* sgKB */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_gQ/* sgLB */);
          }
          break;
        case 2:
          return (E(_gR/* sgLH */)==3) ? new T3(0,new T5(0,1,E(_bp/* LudoJS.Red */),_gN/* sgKB */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_gQ/* sgLB */,_4/* GHC.Types.[] */) : new T3(0,new T5(0,1,E(_bp/* LudoJS.Red */),_gN/* sgKB */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_gQ/* sgLB */);
        default:
          var _gS/* sgLW */ = E(_gR/* sgLH */);
          return new T3(0,new T5(0,1,E(_bq/* LudoJS.Yellow */),_gN/* sgKB */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)),_4/* GHC.Types.[] */,_gQ/* sgLB */);
      }
    }
  }else{
    var _gT/* sgKF */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgKD */>>1, _gM/* sgKA */, _gN/* sgKB */, _gO/* sgKC */)),
    _gU/* sgKG */ = _gT/* sgKF */.a,
    _gV/* sgKI */ = _gT/* sgKF */.c,
    _gW/* sgKJ */ = E(_gT/* sgKF */.b);
    if(!_gW/* sgKJ */._){
      return new T3(0,_gU/* sgKG */,_4/* GHC.Types.[] */,_gV/* sgKI */);
    }else{
      var _gX/* sgKM */ = E(_gW/* sgKJ */.a),
      _gY/* sgKN */ = _gX/* sgKM */.a,
      _gZ/* sgKO */ = _gX/* sgKM */.b,
      _h0/* sgKP */ = E(_gW/* sgKJ */.b);
      if(!_h0/* sgKP */._){
        return new T3(0,new T(function(){
          return B(_fH/* Data.Map.Base.insertMax */(_gY/* sgKN */, _gZ/* sgKO */, _gU/* sgKG */));
        }),_4/* GHC.Types.[] */,_gV/* sgKI */);
      }else{
        var _h1/* sgKS */ = _h0/* sgKP */.b,
        _h2/* sgKT */ = E(_h0/* sgKP */.a),
        _h3/* sgKU */ = _h2/* sgKT */.a,
        _h4/* sgKV */ = _h2/* sgKT */.b;
        switch(E(_gY/* sgKN */)){
          case 0:
            switch(E(_h3/* sgKU */)){
              case 0:
                return new T3(0,_gU/* sgKG */,_4/* GHC.Types.[] */,_gW/* sgKJ */);
              case 1:
                var _h5/* sgKZ */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgKD */>>1, _bo/* LudoJS.Green */, _h4/* sgKV */, _h1/* sgKS */));
                return new T3(0,new T(function(){
                  return B(_gt/* Data.Map.Base.link */(_bn/* LudoJS.Blue */, _gZ/* sgKO */, _gU/* sgKG */, _h5/* sgKZ */.a));
                }),_h5/* sgKZ */.b,_h5/* sgKZ */.c);
              case 2:
                var _h6/* sgL5 */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgKD */>>1, _bp/* LudoJS.Red */, _h4/* sgKV */, _h1/* sgKS */));
                return new T3(0,new T(function(){
                  return B(_gt/* Data.Map.Base.link */(_bn/* LudoJS.Blue */, _gZ/* sgKO */, _gU/* sgKG */, _h6/* sgL5 */.a));
                }),_h6/* sgL5 */.b,_h6/* sgL5 */.c);
              default:
                var _h7/* sgLb */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgKD */>>1, _bq/* LudoJS.Yellow */, _h4/* sgKV */, _h1/* sgKS */));
                return new T3(0,new T(function(){
                  return B(_gt/* Data.Map.Base.link */(_bn/* LudoJS.Blue */, _gZ/* sgKO */, _gU/* sgKG */, _h7/* sgLb */.a));
                }),_h7/* sgLb */.b,_h7/* sgLb */.c);
            }
            break;
          case 1:
            switch(E(_h3/* sgKU */)){
              case 2:
                var _h8/* sgLi */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgKD */>>1, _bp/* LudoJS.Red */, _h4/* sgKV */, _h1/* sgKS */));
                return new T3(0,new T(function(){
                  return B(_gt/* Data.Map.Base.link */(_bo/* LudoJS.Green */, _gZ/* sgKO */, _gU/* sgKG */, _h8/* sgLi */.a));
                }),_h8/* sgLi */.b,_h8/* sgLi */.c);
              case 3:
                var _h9/* sgLo */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgKD */>>1, _bq/* LudoJS.Yellow */, _h4/* sgKV */, _h1/* sgKS */));
                return new T3(0,new T(function(){
                  return B(_gt/* Data.Map.Base.link */(_bo/* LudoJS.Green */, _gZ/* sgKO */, _gU/* sgKG */, _h9/* sgLo */.a));
                }),_h9/* sgLo */.b,_h9/* sgLo */.c);
              default:
                return new T3(0,_gU/* sgKG */,_4/* GHC.Types.[] */,_gW/* sgKJ */);
            }
            break;
          case 2:
            if(E(_h3/* sgKU */)==3){
              var _ha/* sgLv */ = B(_gK/* LudoJS.$s$wpoly_create */(_gP/* sgKD */>>1, _bq/* LudoJS.Yellow */, _h4/* sgKV */, _h1/* sgKS */));
              return new T3(0,new T(function(){
                return B(_gt/* Data.Map.Base.link */(_bp/* LudoJS.Red */, _gZ/* sgKO */, _gU/* sgKG */, _ha/* sgLv */.a));
              }),_ha/* sgLv */.b,_ha/* sgLv */.c);
            }else{
              return new T3(0,_gU/* sgKG */,_4/* GHC.Types.[] */,_gW/* sgKJ */);
            }
            break;
          default:
            var _hb/* sgLA */ = E(_h3/* sgKU */);
            return new T3(0,_gU/* sgKG */,_4/* GHC.Types.[] */,_gW/* sgKJ */);
        }
      }
    }
  }
},
_hc/* $spoly_go10 */ = function(_hd/* sgMZ */, _he/* sgN0 */, _hf/* sgN1 */){
  var _hg/* sgN2 */ = E(_he/* sgN0 */);
  return new F(function(){return _ft/* LudoJS.poly_go10 */(B(_fi/* LudoJS.$sinsert_$sgo10 */(_hg/* sgN2 */.a, _hg/* sgN2 */.b, _hd/* sgMZ */)), _hf/* sgN1 */);});
},
_hh/* $wpoly_go10 */ = function(_hi/* sgO2 */, _hj/* sgO3 */, _hk/* sgO4 */){
  var _hl/* sgO5 */ = E(_hk/* sgO4 */);
  if(!_hl/* sgO5 */._){
    return E(_hj/* sgO3 */);
  }else{
    var _hm/* sgO8 */ = E(_hl/* sgO5 */.a),
    _hn/* sgO9 */ = _hm/* sgO8 */.a,
    _ho/* sgOa */ = _hm/* sgO8 */.b,
    _hp/* sgOb */ = E(_hl/* sgO5 */.b);
    if(!_hp/* sgOb */._){
      return new F(function(){return _fH/* Data.Map.Base.insertMax */(_hn/* sgO9 */, _ho/* sgOa */, _hj/* sgO3 */);});
    }else{
      var _hq/* sgOe */ = E(_hp/* sgOb */.a),
      _hr/* sgOf */ = _hq/* sgOe */.a,
      _hs/* sgOh */ = function(_ht/* sgOi */){
        var _hu/* sgOj */ = B(_gK/* LudoJS.$s$wpoly_create */(_hi/* sgO2 */, _hr/* sgOf */, _hq/* sgOe */.b, _hp/* sgOb */.b)),
        _hv/* sgOk */ = _hu/* sgOj */.a,
        _hw/* sgOn */ = E(_hu/* sgOj */.c);
        if(!_hw/* sgOn */._){
          return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(_hi/* sgO2 */<<1, B(_gt/* Data.Map.Base.link */(_hn/* sgO9 */, _ho/* sgOa */, _hj/* sgO3 */, _hv/* sgOk */)), _hu/* sgOj */.b);});
        }else{
          return new F(function(){return _hc/* LudoJS.$spoly_go10 */(B(_gt/* Data.Map.Base.link */(_hn/* sgO9 */, _ho/* sgOa */, _hj/* sgO3 */, _hv/* sgOk */)), _hw/* sgOn */.a, _hw/* sgOn */.b);});
        }
      };
      switch(E(_hn/* sgO9 */)){
        case 0:
          switch(E(_hr/* sgOf */)){
            case 0:
              return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(_hj/* sgO3 */, _bn/* LudoJS.Blue */, _ho/* sgOa */, _hp/* sgOb */);});
              break;
            case 1:
              return new F(function(){return _hs/* sgOh */(_/* EXTERNAL */);});
              break;
            case 2:
              return new F(function(){return _hs/* sgOh */(_/* EXTERNAL */);});
              break;
            default:
              return new F(function(){return _hs/* sgOh */(_/* EXTERNAL */);});
          }
          break;
        case 1:
          switch(E(_hr/* sgOf */)){
            case 2:
              return new F(function(){return _hs/* sgOh */(_/* EXTERNAL */);});
              break;
            case 3:
              return new F(function(){return _hs/* sgOh */(_/* EXTERNAL */);});
              break;
            default:
              return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(_hj/* sgO3 */, _bo/* LudoJS.Green */, _ho/* sgOa */, _hp/* sgOb */);});
          }
          break;
        case 2:
          if(E(_hr/* sgOf */)==3){
            return new F(function(){return _hs/* sgOh */(_/* EXTERNAL */);});
          }else{
            return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(_hj/* sgO3 */, _bp/* LudoJS.Red */, _ho/* sgOa */, _hp/* sgOb */);});
          }
          break;
        default:
          var _hx/* sgOx */ = E(_hr/* sgOf */);
          return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(_hj/* sgO3 */, _bq/* LudoJS.Yellow */, _ho/* sgOa */, _hp/* sgOb */);});
      }
    }
  }
},
_hy/* $sfromList */ = function(_hz/* sgQn */){
  var _hA/* sgQo */ = E(_hz/* sgQn */);
  if(!_hA/* sgQo */._){
    return new T0(1);
  }else{
    var _hB/* sgQr */ = E(_hA/* sgQo */.a),
    _hC/* sgQs */ = _hB/* sgQr */.a,
    _hD/* sgQt */ = _hB/* sgQr */.b,
    _hE/* sgQu */ = E(_hA/* sgQo */.b);
    if(!_hE/* sgQu */._){
      return new T5(0,1,E(_hC/* sgQs */),_hD/* sgQt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
    }else{
      var _hF/* sgQx */ = _hE/* sgQu */.b,
      _hG/* sgQy */ = E(_hE/* sgQu */.a),
      _hH/* sgQz */ = _hG/* sgQy */.a,
      _hI/* sgQA */ = _hG/* sgQy */.b;
      switch(E(_hC/* sgQs */)){
        case 0:
          switch(E(_hH/* sgQz */)){
            case 0:
              return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_bn/* LudoJS.Blue */),_hD/* sgQt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _bn/* LudoJS.Blue */, _hI/* sgQA */, _hF/* sgQx */);});
              break;
            case 1:
              return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_bn/* LudoJS.Blue */),_hD/* sgQt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hE/* sgQu */);});
              break;
            case 2:
              return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_bn/* LudoJS.Blue */),_hD/* sgQt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hE/* sgQu */);});
              break;
            default:
              return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_bn/* LudoJS.Blue */),_hD/* sgQt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hE/* sgQu */);});
          }
          break;
        case 1:
          var _hJ/* sgQH */ = E(_hH/* sgQz */);
          switch(_hJ/* sgQH */){
            case 2:
              return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_bo/* LudoJS.Green */),_hD/* sgQt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hE/* sgQu */);});
              break;
            case 3:
              return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_bo/* LudoJS.Green */),_hD/* sgQt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hE/* sgQu */);});
              break;
            default:
              return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_bo/* LudoJS.Green */),_hD/* sgQt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hJ/* sgQH */, _hI/* sgQA */, _hF/* sgQx */);});
          }
          break;
        case 2:
          var _hK/* sgQL */ = E(_hH/* sgQz */);
          if(_hK/* sgQL */==3){
            return new F(function(){return _hh/* LudoJS.$wpoly_go10 */(1, new T5(0,1,E(_bp/* LudoJS.Red */),_hD/* sgQt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hE/* sgQu */);});
          }else{
            return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_bp/* LudoJS.Red */),_hD/* sgQt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), _hK/* sgQL */, _hI/* sgQA */, _hF/* sgQx */);});
          }
          break;
        default:
          return new F(function(){return _fz/* LudoJS.$sfromList_$spoly_go10 */(new T5(0,1,E(_bq/* LudoJS.Yellow */),_hD/* sgQt */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */)), E(_hH/* sgQz */), _hI/* sgQA */, _hF/* sgQx */);});
      }
    }
  }
},
_hL/* $w$cshowsPrec */ = function(_hM/* sheO */, _hN/* sheP */){
  switch(E(_hM/* sheO */)){
    case 0:
      return new F(function(){return _q/* GHC.Base.++ */(_1u/* LudoJS.$fFromAnyGameState14 */, _hN/* sheP */);});
      break;
    case 1:
      return new F(function(){return _q/* GHC.Base.++ */(_1t/* LudoJS.$fFromAnyGameState13 */, _hN/* sheP */);});
      break;
    case 2:
      return new F(function(){return _q/* GHC.Base.++ */(_1s/* LudoJS.$fFromAnyGameState12 */, _hN/* sheP */);});
      break;
    default:
      return new F(function(){return _q/* GHC.Base.++ */(_1r/* LudoJS.$fFromAnyGameState11 */, _hN/* sheP */);});
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
_hZ/* $fFromAnyGameState6 */ = function(_i0/* sheV */, _/* EXTERNAL */){
  var _i1/* sheX */ = E(_i0/* sheV */),
  _i2/* shf5 */ = __get/* EXTERNAL */(_i1/* sheX */, toJSStr/* EXTERNAL */(B(_q/* GHC.Base.++ */(_1u/* LudoJS.$fFromAnyGameState14 */, _4/* GHC.Types.[] */)))),
  _i3/* shf7 */ = _i2/* shf5 */,
  _i4/* shf9 */ = function(_i5/*  shij */, _i6/*  shik */, _/* EXTERNAL */){
    while(1){
      var _i7/*  shf9 */ = B((function(_i8/* shij */, _i9/* shik */, _/* EXTERNAL */){
        var _ia/* shim */ = E(_i8/* shij */);
        if(!_ia/* shim */._){
          return _i9/* shik */;
        }else{
          var _ib/* shio */ = _ia/* shim */.b,
          _ic/* ship */ = E(_ia/* shim */.a),
          _id/* shit */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _ic/* ship */, _4/* GHC.Types.[] */))),
          _ie/* shix */ = __has/* EXTERNAL */(_i3/* shf7 */, _id/* shit */);
          if(!E(_ie/* shix */)){
            var _if/*   shik */ = _i9/* shik */;
            _i5/*  shij */ = _ib/* shio */;
            _i6/*  shik */ = _if/*   shik */;
            return __continue/* EXTERNAL */;
          }else{
            var _ig/* shiC */ = __get/* EXTERNAL */(_i3/* shf7 */, _id/* shit */),
            _ih/* shiF */ = E(_9N/* LudoJS.$fToAnyOption5 */),
            _ii/* shiI */ = __get/* EXTERNAL */(_ig/* shiC */, _ih/* shiF */),
            _ij/* shiM */ = String/* EXTERNAL */(_ii/* shiI */),
            _ik/* shiP */ = E(_hW/* LudoJS.lvl35 */),
            _il/* shiS */ = strEq/* EXTERNAL */(_ij/* shiM */, _ik/* shiP */);
            if(!E(_il/* shiS */)){
              var _im/* shk3 */ = E(_hV/* LudoJS.lvl34 */),
              _in/* shk6 */ = strEq/* EXTERNAL */(_ij/* shiM */, _im/* shk3 */);
              if(!E(_in/* shk6 */)){
                return E(_hU/* LudoJS.lvl33 */);
              }else{
                var _io/* shka */ = E(_9M/* LudoJS.$fToAnyOption1 */),
                _ip/* shkd */ = __get/* EXTERNAL */(_ig/* shiC */, _io/* shka */),
                _iq/* shkg */ = function(_ir/*  shkh */, _is/*  shki */, _/* EXTERNAL */){
                  while(1){
                    var _it/*  shkg */ = B((function(_iu/* shkh */, _iv/* shki */, _/* EXTERNAL */){
                      var _iw/* shkk */ = E(_iu/* shkh */);
                      if(!_iw/* shkk */._){
                        return _iv/* shki */;
                      }else{
                        var _ix/* shkm */ = _iw/* shkk */.b,
                        _iy/* shkn */ = E(_iw/* shkk */.a),
                        _iz/* shkr */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _iy/* shkn */, _4/* GHC.Types.[] */))),
                        _iA/* shkv */ = __has/* EXTERNAL */(_i3/* shf7 */, _iz/* shkr */);
                        if(!E(_iA/* shkv */)){
                          var _iB/*   shki */ = _iv/* shki */;
                          _ir/*  shkh */ = _ix/* shkm */;
                          _is/*  shki */ = _iB/*   shki */;
                          return __continue/* EXTERNAL */;
                        }else{
                          var _iC/* shkA */ = __get/* EXTERNAL */(_i3/* shf7 */, _iz/* shkr */),
                          _iD/* shkE */ = __get/* EXTERNAL */(_iC/* shkA */, _ih/* shiF */),
                          _iE/* shkI */ = String/* EXTERNAL */(_iD/* shkE */),
                          _iF/* shkM */ = strEq/* EXTERNAL */(_iE/* shkI */, _ik/* shiP */);
                          if(!E(_iF/* shkM */)){
                            var _iG/* shkU */ = strEq/* EXTERNAL */(_iE/* shkI */, _im/* shk3 */);
                            if(!E(_iG/* shkU */)){
                              return E(_hU/* LudoJS.lvl33 */);
                            }else{
                              var _iH/* shkZ */ = __get/* EXTERNAL */(_iC/* shkA */, _io/* shka */),
                              _iI/* shle */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_iv/* shki */, new T2(1,new T2(0,_iy/* shkn */,new T1(1,new T(function(){
                                  var _iJ/* shl3 */ = Number/* EXTERNAL */(_iH/* shkZ */);
                                  return jsTrunc/* EXTERNAL */(_iJ/* shl3 */);
                                }))),_4/* GHC.Types.[] */)));
                              });
                              _ir/*  shkh */ = _ix/* shkm */;
                              _is/*  shki */ = _iI/* shle */;
                              return __continue/* EXTERNAL */;
                            }
                          }else{
                            _ir/*  shkh */ = _ix/* shkm */;
                            _is/*  shki */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_iv/* shki */, new T2(1,new T2(0,_iy/* shkn */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                            });
                            return __continue/* EXTERNAL */;
                          }
                        }
                      }
                    })(_ir/*  shkh */, _is/*  shki */, _/* EXTERNAL */));
                    if(_it/*  shkg */!=__continue/* EXTERNAL */){
                      return _it/*  shkg */;
                    }
                  }
                },
                _iK/* shls */ = new T(function(){
                  return B(_q/* GHC.Base.++ */(_i9/* shik */, new T2(1,new T2(0,_ic/* ship */,new T1(1,new T(function(){
                    var _iL/* shlh */ = Number/* EXTERNAL */(_ip/* shkd */);
                    return jsTrunc/* EXTERNAL */(_iL/* shlh */);
                  }))),_4/* GHC.Types.[] */)));
                });
                return new F(function(){return _iq/* shkg */(_ib/* shio */, _iK/* shls */, _/* EXTERNAL */);});
              }
            }else{
              var _iM/* shiW */ = function(_iN/*  shiX */, _iO/*  shiY */, _/* EXTERNAL */){
                while(1){
                  var _iP/*  shiW */ = B((function(_iQ/* shiX */, _iR/* shiY */, _/* EXTERNAL */){
                    var _iS/* shj0 */ = E(_iQ/* shiX */);
                    if(!_iS/* shj0 */._){
                      return _iR/* shiY */;
                    }else{
                      var _iT/* shj2 */ = _iS/* shj0 */.b,
                      _iU/* shj3 */ = E(_iS/* shj0 */.a),
                      _iV/* shj7 */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _iU/* shj3 */, _4/* GHC.Types.[] */))),
                      _iW/* shjb */ = __has/* EXTERNAL */(_i3/* shf7 */, _iV/* shj7 */);
                      if(!E(_iW/* shjb */)){
                        var _iX/*   shiY */ = _iR/* shiY */;
                        _iN/*  shiX */ = _iT/* shj2 */;
                        _iO/*  shiY */ = _iX/*   shiY */;
                        return __continue/* EXTERNAL */;
                      }else{
                        var _iY/* shjg */ = __get/* EXTERNAL */(_i3/* shf7 */, _iV/* shj7 */),
                        _iZ/* shjk */ = __get/* EXTERNAL */(_iY/* shjg */, _ih/* shiF */),
                        _j0/* shjo */ = String/* EXTERNAL */(_iZ/* shjk */),
                        _j1/* shjs */ = strEq/* EXTERNAL */(_j0/* shjo */, _ik/* shiP */);
                        if(!E(_j1/* shjs */)){
                          var _j2/* shjC */ = strEq/* EXTERNAL */(_j0/* shjo */, E(_hV/* LudoJS.lvl34 */));
                          if(!E(_j2/* shjC */)){
                            return E(_hU/* LudoJS.lvl33 */);
                          }else{
                            var _j3/* shjJ */ = __get/* EXTERNAL */(_iY/* shjg */, E(_9M/* LudoJS.$fToAnyOption1 */)),
                            _j4/* shjY */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_iR/* shiY */, new T2(1,new T2(0,_iU/* shj3 */,new T1(1,new T(function(){
                                var _j5/* shjN */ = Number/* EXTERNAL */(_j3/* shjJ */);
                                return jsTrunc/* EXTERNAL */(_j5/* shjN */);
                              }))),_4/* GHC.Types.[] */)));
                            });
                            _iN/*  shiX */ = _iT/* shj2 */;
                            _iO/*  shiY */ = _j4/* shjY */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          _iN/*  shiX */ = _iT/* shj2 */;
                          _iO/*  shiY */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_iR/* shiY */, new T2(1,new T2(0,_iU/* shj3 */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                          });
                          return __continue/* EXTERNAL */;
                        }
                      }
                    }
                  })(_iN/*  shiX */, _iO/*  shiY */, _/* EXTERNAL */));
                  if(_iP/*  shiW */!=__continue/* EXTERNAL */){
                    return _iP/*  shiW */;
                  }
                }
              };
              return new F(function(){return _iM/* shiW */(_ib/* shio */, new T(function(){
                return B(_q/* GHC.Base.++ */(_i9/* shik */, new T2(1,new T2(0,_ic/* ship */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
              }), _/* EXTERNAL */);});
            }
          }
        }
      })(_i5/*  shij */, _i6/*  shik */, _/* EXTERNAL */));
      if(_i7/*  shf9 */!=__continue/* EXTERNAL */){
        return _i7/*  shf9 */;
      }
    }
  },
  _j6/* shf8 */ = function(_j7/* shfa */, _j8/* shfb */, _j9/* shfc */, _/* EXTERNAL */){
    var _ja/* shfg */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _j7/* shfa */, _4/* GHC.Types.[] */))),
    _jb/* shfk */ = __has/* EXTERNAL */(_i3/* shf7 */, _ja/* shfg */);
    if(!E(_jb/* shfk */)){
      return new F(function(){return _i4/* shf9 */(_j8/* shfb */, _j9/* shfc */, _/* EXTERNAL */);});
    }else{
      var _jc/* shfp */ = __get/* EXTERNAL */(_i3/* shf7 */, _ja/* shfg */),
      _jd/* shfs */ = E(_9N/* LudoJS.$fToAnyOption5 */),
      _je/* shfv */ = __get/* EXTERNAL */(_jc/* shfp */, _jd/* shfs */),
      _jf/* shfz */ = String/* EXTERNAL */(_je/* shfv */),
      _jg/* shfC */ = E(_hW/* LudoJS.lvl35 */),
      _jh/* shfF */ = strEq/* EXTERNAL */(_jf/* shfz */, _jg/* shfC */);
      if(!E(_jh/* shfF */)){
        var _ji/* shgR */ = E(_hV/* LudoJS.lvl34 */),
        _jj/* shgU */ = strEq/* EXTERNAL */(_jf/* shfz */, _ji/* shgR */);
        if(!E(_jj/* shgU */)){
          return E(_hU/* LudoJS.lvl33 */);
        }else{
          var _jk/* shgY */ = E(_9M/* LudoJS.$fToAnyOption1 */),
          _jl/* shh1 */ = __get/* EXTERNAL */(_jc/* shfp */, _jk/* shgY */),
          _jm/* shh4 */ = function(_jn/*  shh5 */, _jo/*  shh6 */, _/* EXTERNAL */){
            while(1){
              var _jp/*  shh4 */ = B((function(_jq/* shh5 */, _jr/* shh6 */, _/* EXTERNAL */){
                var _js/* shh8 */ = E(_jq/* shh5 */);
                if(!_js/* shh8 */._){
                  return _jr/* shh6 */;
                }else{
                  var _jt/* shha */ = _js/* shh8 */.b,
                  _ju/* shhb */ = E(_js/* shh8 */.a),
                  _jv/* shhf */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _ju/* shhb */, _4/* GHC.Types.[] */))),
                  _jw/* shhj */ = __has/* EXTERNAL */(_i3/* shf7 */, _jv/* shhf */);
                  if(!E(_jw/* shhj */)){
                    var _jx/*   shh6 */ = _jr/* shh6 */;
                    _jn/*  shh5 */ = _jt/* shha */;
                    _jo/*  shh6 */ = _jx/*   shh6 */;
                    return __continue/* EXTERNAL */;
                  }else{
                    var _jy/* shho */ = __get/* EXTERNAL */(_i3/* shf7 */, _jv/* shhf */),
                    _jz/* shhs */ = __get/* EXTERNAL */(_jy/* shho */, _jd/* shfs */),
                    _jA/* shhw */ = String/* EXTERNAL */(_jz/* shhs */),
                    _jB/* shhA */ = strEq/* EXTERNAL */(_jA/* shhw */, _jg/* shfC */);
                    if(!E(_jB/* shhA */)){
                      var _jC/* shhI */ = strEq/* EXTERNAL */(_jA/* shhw */, _ji/* shgR */);
                      if(!E(_jC/* shhI */)){
                        return E(_hU/* LudoJS.lvl33 */);
                      }else{
                        var _jD/* shhN */ = __get/* EXTERNAL */(_jy/* shho */, _jk/* shgY */),
                        _jE/* shi2 */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_jr/* shh6 */, new T2(1,new T2(0,_ju/* shhb */,new T1(1,new T(function(){
                            var _jF/* shhR */ = Number/* EXTERNAL */(_jD/* shhN */);
                            return jsTrunc/* EXTERNAL */(_jF/* shhR */);
                          }))),_4/* GHC.Types.[] */)));
                        });
                        _jn/*  shh5 */ = _jt/* shha */;
                        _jo/*  shh6 */ = _jE/* shi2 */;
                        return __continue/* EXTERNAL */;
                      }
                    }else{
                      _jn/*  shh5 */ = _jt/* shha */;
                      _jo/*  shh6 */ = new T(function(){
                        return B(_q/* GHC.Base.++ */(_jr/* shh6 */, new T2(1,new T2(0,_ju/* shhb */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                      });
                      return __continue/* EXTERNAL */;
                    }
                  }
                }
              })(_jn/*  shh5 */, _jo/*  shh6 */, _/* EXTERNAL */));
              if(_jp/*  shh4 */!=__continue/* EXTERNAL */){
                return _jp/*  shh4 */;
              }
            }
          },
          _jG/* shih */ = new T(function(){
            return B(_q/* GHC.Base.++ */(_j9/* shfc */, new T2(1,new T2(0,_j7/* shfa */,new T1(1,new T(function(){
              var _jH/* shi6 */ = Number/* EXTERNAL */(_jl/* shh1 */);
              return jsTrunc/* EXTERNAL */(_jH/* shi6 */);
            }))),_4/* GHC.Types.[] */)));
          });
          return new F(function(){return _jm/* shh4 */(_j8/* shfb */, _jG/* shih */, _/* EXTERNAL */);});
        }
      }else{
        var _jI/* shfJ */ = function(_jJ/*  shfK */, _jK/*  shfL */, _/* EXTERNAL */){
          while(1){
            var _jL/*  shfJ */ = B((function(_jM/* shfK */, _jN/* shfL */, _/* EXTERNAL */){
              var _jO/* shfN */ = E(_jM/* shfK */);
              if(!_jO/* shfN */._){
                return _jN/* shfL */;
              }else{
                var _jP/* shfP */ = _jO/* shfN */.b,
                _jQ/* shfQ */ = E(_jO/* shfN */.a),
                _jR/* shfU */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _jQ/* shfQ */, _4/* GHC.Types.[] */))),
                _jS/* shfY */ = __has/* EXTERNAL */(_i3/* shf7 */, _jR/* shfU */);
                if(!E(_jS/* shfY */)){
                  var _jT/*   shfL */ = _jN/* shfL */;
                  _jJ/*  shfK */ = _jP/* shfP */;
                  _jK/*  shfL */ = _jT/*   shfL */;
                  return __continue/* EXTERNAL */;
                }else{
                  var _jU/* shg3 */ = __get/* EXTERNAL */(_i3/* shf7 */, _jR/* shfU */),
                  _jV/* shg7 */ = __get/* EXTERNAL */(_jU/* shg3 */, _jd/* shfs */),
                  _jW/* shgb */ = String/* EXTERNAL */(_jV/* shg7 */),
                  _jX/* shgf */ = strEq/* EXTERNAL */(_jW/* shgb */, _jg/* shfC */);
                  if(!E(_jX/* shgf */)){
                    var _jY/* shgp */ = strEq/* EXTERNAL */(_jW/* shgb */, E(_hV/* LudoJS.lvl34 */));
                    if(!E(_jY/* shgp */)){
                      return E(_hU/* LudoJS.lvl33 */);
                    }else{
                      var _jZ/* shgw */ = __get/* EXTERNAL */(_jU/* shg3 */, E(_9M/* LudoJS.$fToAnyOption1 */)),
                      _k0/* shgL */ = new T(function(){
                        return B(_q/* GHC.Base.++ */(_jN/* shfL */, new T2(1,new T2(0,_jQ/* shfQ */,new T1(1,new T(function(){
                          var _k1/* shgA */ = Number/* EXTERNAL */(_jZ/* shgw */);
                          return jsTrunc/* EXTERNAL */(_k1/* shgA */);
                        }))),_4/* GHC.Types.[] */)));
                      });
                      _jJ/*  shfK */ = _jP/* shfP */;
                      _jK/*  shfL */ = _k0/* shgL */;
                      return __continue/* EXTERNAL */;
                    }
                  }else{
                    _jJ/*  shfK */ = _jP/* shfP */;
                    _jK/*  shfL */ = new T(function(){
                      return B(_q/* GHC.Base.++ */(_jN/* shfL */, new T2(1,new T2(0,_jQ/* shfQ */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                    });
                    return __continue/* EXTERNAL */;
                  }
                }
              }
            })(_jJ/*  shfK */, _jK/*  shfL */, _/* EXTERNAL */));
            if(_jL/*  shfJ */!=__continue/* EXTERNAL */){
              return _jL/*  shfJ */;
            }
          }
        };
        return new F(function(){return _jI/* shfJ */(_j8/* shfb */, new T(function(){
          return B(_q/* GHC.Base.++ */(_j9/* shfc */, new T2(1,new T2(0,_j7/* shfa */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
        }), _/* EXTERNAL */);});
      }
    }
  },
  _k2/* shlu */ = B(_j6/* shf8 */(1, _hT/* LudoJS.a34 */, _4/* GHC.Types.[] */, _/* EXTERNAL */)),
  _k3/* shly */ = function(_k4/* shsf */, _/* EXTERNAL */){
    var _k5/* shsh */ = E(_k4/* shsf */);
    if(!_k5/* shsh */._){
      return _4/* GHC.Types.[] */;
    }else{
      var _k6/* shsi */ = _k5/* shsh */.a,
      _k7/* shsq */ = __get/* EXTERNAL */(_i1/* sheX */, toJSStr/* EXTERNAL */(B(_hL/* LudoJS.$w$cshowsPrec */(_k6/* shsi */, _4/* GHC.Types.[] */)))),
      _k8/* shss */ = _k7/* shsq */,
      _k9/* shsu */ = function(_ka/*  shvE */, _kb/*  shvF */, _/* EXTERNAL */){
        while(1){
          var _kc/*  shsu */ = B((function(_kd/* shvE */, _ke/* shvF */, _/* EXTERNAL */){
            var _kf/* shvH */ = E(_kd/* shvE */);
            if(!_kf/* shvH */._){
              return _ke/* shvF */;
            }else{
              var _kg/* shvJ */ = _kf/* shvH */.b,
              _kh/* shvK */ = E(_kf/* shvH */.a),
              _ki/* shvO */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _kh/* shvK */, _4/* GHC.Types.[] */))),
              _kj/* shvS */ = __has/* EXTERNAL */(_k8/* shss */, _ki/* shvO */);
              if(!E(_kj/* shvS */)){
                var _kk/*   shvF */ = _ke/* shvF */;
                _ka/*  shvE */ = _kg/* shvJ */;
                _kb/*  shvF */ = _kk/*   shvF */;
                return __continue/* EXTERNAL */;
              }else{
                var _kl/* shvX */ = __get/* EXTERNAL */(_k8/* shss */, _ki/* shvO */),
                _km/* shw0 */ = E(_9N/* LudoJS.$fToAnyOption5 */),
                _kn/* shw3 */ = __get/* EXTERNAL */(_kl/* shvX */, _km/* shw0 */),
                _ko/* shw7 */ = String/* EXTERNAL */(_kn/* shw3 */),
                _kp/* shwa */ = E(_hW/* LudoJS.lvl35 */),
                _kq/* shwd */ = strEq/* EXTERNAL */(_ko/* shw7 */, _kp/* shwa */);
                if(!E(_kq/* shwd */)){
                  var _kr/* shxo */ = E(_hV/* LudoJS.lvl34 */),
                  _ks/* shxr */ = strEq/* EXTERNAL */(_ko/* shw7 */, _kr/* shxo */);
                  if(!E(_ks/* shxr */)){
                    return E(_hU/* LudoJS.lvl33 */);
                  }else{
                    var _kt/* shxv */ = E(_9M/* LudoJS.$fToAnyOption1 */),
                    _ku/* shxy */ = __get/* EXTERNAL */(_kl/* shvX */, _kt/* shxv */),
                    _kv/* shxB */ = function(_kw/*  shxC */, _kx/*  shxD */, _/* EXTERNAL */){
                      while(1){
                        var _ky/*  shxB */ = B((function(_kz/* shxC */, _kA/* shxD */, _/* EXTERNAL */){
                          var _kB/* shxF */ = E(_kz/* shxC */);
                          if(!_kB/* shxF */._){
                            return _kA/* shxD */;
                          }else{
                            var _kC/* shxH */ = _kB/* shxF */.b,
                            _kD/* shxI */ = E(_kB/* shxF */.a),
                            _kE/* shxM */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _kD/* shxI */, _4/* GHC.Types.[] */))),
                            _kF/* shxQ */ = __has/* EXTERNAL */(_k8/* shss */, _kE/* shxM */);
                            if(!E(_kF/* shxQ */)){
                              var _kG/*   shxD */ = _kA/* shxD */;
                              _kw/*  shxC */ = _kC/* shxH */;
                              _kx/*  shxD */ = _kG/*   shxD */;
                              return __continue/* EXTERNAL */;
                            }else{
                              var _kH/* shxV */ = __get/* EXTERNAL */(_k8/* shss */, _kE/* shxM */),
                              _kI/* shxZ */ = __get/* EXTERNAL */(_kH/* shxV */, _km/* shw0 */),
                              _kJ/* shy3 */ = String/* EXTERNAL */(_kI/* shxZ */),
                              _kK/* shy7 */ = strEq/* EXTERNAL */(_kJ/* shy3 */, _kp/* shwa */);
                              if(!E(_kK/* shy7 */)){
                                var _kL/* shyf */ = strEq/* EXTERNAL */(_kJ/* shy3 */, _kr/* shxo */);
                                if(!E(_kL/* shyf */)){
                                  return E(_hU/* LudoJS.lvl33 */);
                                }else{
                                  var _kM/* shyk */ = __get/* EXTERNAL */(_kH/* shxV */, _kt/* shxv */),
                                  _kN/* shyz */ = new T(function(){
                                    return B(_q/* GHC.Base.++ */(_kA/* shxD */, new T2(1,new T2(0,_kD/* shxI */,new T1(1,new T(function(){
                                      var _kO/* shyo */ = Number/* EXTERNAL */(_kM/* shyk */);
                                      return jsTrunc/* EXTERNAL */(_kO/* shyo */);
                                    }))),_4/* GHC.Types.[] */)));
                                  });
                                  _kw/*  shxC */ = _kC/* shxH */;
                                  _kx/*  shxD */ = _kN/* shyz */;
                                  return __continue/* EXTERNAL */;
                                }
                              }else{
                                _kw/*  shxC */ = _kC/* shxH */;
                                _kx/*  shxD */ = new T(function(){
                                  return B(_q/* GHC.Base.++ */(_kA/* shxD */, new T2(1,new T2(0,_kD/* shxI */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                                });
                                return __continue/* EXTERNAL */;
                              }
                            }
                          }
                        })(_kw/*  shxC */, _kx/*  shxD */, _/* EXTERNAL */));
                        if(_ky/*  shxB */!=__continue/* EXTERNAL */){
                          return _ky/*  shxB */;
                        }
                      }
                    },
                    _kP/* shyN */ = new T(function(){
                      return B(_q/* GHC.Base.++ */(_ke/* shvF */, new T2(1,new T2(0,_kh/* shvK */,new T1(1,new T(function(){
                        var _kQ/* shyC */ = Number/* EXTERNAL */(_ku/* shxy */);
                        return jsTrunc/* EXTERNAL */(_kQ/* shyC */);
                      }))),_4/* GHC.Types.[] */)));
                    });
                    return new F(function(){return _kv/* shxB */(_kg/* shvJ */, _kP/* shyN */, _/* EXTERNAL */);});
                  }
                }else{
                  var _kR/* shwh */ = function(_kS/*  shwi */, _kT/*  shwj */, _/* EXTERNAL */){
                    while(1){
                      var _kU/*  shwh */ = B((function(_kV/* shwi */, _kW/* shwj */, _/* EXTERNAL */){
                        var _kX/* shwl */ = E(_kV/* shwi */);
                        if(!_kX/* shwl */._){
                          return _kW/* shwj */;
                        }else{
                          var _kY/* shwn */ = _kX/* shwl */.b,
                          _kZ/* shwo */ = E(_kX/* shwl */.a),
                          _l0/* shws */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _kZ/* shwo */, _4/* GHC.Types.[] */))),
                          _l1/* shww */ = __has/* EXTERNAL */(_k8/* shss */, _l0/* shws */);
                          if(!E(_l1/* shww */)){
                            var _l2/*   shwj */ = _kW/* shwj */;
                            _kS/*  shwi */ = _kY/* shwn */;
                            _kT/*  shwj */ = _l2/*   shwj */;
                            return __continue/* EXTERNAL */;
                          }else{
                            var _l3/* shwB */ = __get/* EXTERNAL */(_k8/* shss */, _l0/* shws */),
                            _l4/* shwF */ = __get/* EXTERNAL */(_l3/* shwB */, _km/* shw0 */),
                            _l5/* shwJ */ = String/* EXTERNAL */(_l4/* shwF */),
                            _l6/* shwN */ = strEq/* EXTERNAL */(_l5/* shwJ */, _kp/* shwa */);
                            if(!E(_l6/* shwN */)){
                              var _l7/* shwX */ = strEq/* EXTERNAL */(_l5/* shwJ */, E(_hV/* LudoJS.lvl34 */));
                              if(!E(_l7/* shwX */)){
                                return E(_hU/* LudoJS.lvl33 */);
                              }else{
                                var _l8/* shx4 */ = __get/* EXTERNAL */(_l3/* shwB */, E(_9M/* LudoJS.$fToAnyOption1 */)),
                                _l9/* shxj */ = new T(function(){
                                  return B(_q/* GHC.Base.++ */(_kW/* shwj */, new T2(1,new T2(0,_kZ/* shwo */,new T1(1,new T(function(){
                                    var _la/* shx8 */ = Number/* EXTERNAL */(_l8/* shx4 */);
                                    return jsTrunc/* EXTERNAL */(_la/* shx8 */);
                                  }))),_4/* GHC.Types.[] */)));
                                });
                                _kS/*  shwi */ = _kY/* shwn */;
                                _kT/*  shwj */ = _l9/* shxj */;
                                return __continue/* EXTERNAL */;
                              }
                            }else{
                              _kS/*  shwi */ = _kY/* shwn */;
                              _kT/*  shwj */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_kW/* shwj */, new T2(1,new T2(0,_kZ/* shwo */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                              });
                              return __continue/* EXTERNAL */;
                            }
                          }
                        }
                      })(_kS/*  shwi */, _kT/*  shwj */, _/* EXTERNAL */));
                      if(_kU/*  shwh */!=__continue/* EXTERNAL */){
                        return _kU/*  shwh */;
                      }
                    }
                  };
                  return new F(function(){return _kR/* shwh */(_kg/* shvJ */, new T(function(){
                    return B(_q/* GHC.Base.++ */(_ke/* shvF */, new T2(1,new T2(0,_kh/* shvK */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                  }), _/* EXTERNAL */);});
                }
              }
            }
          })(_ka/*  shvE */, _kb/*  shvF */, _/* EXTERNAL */));
          if(_kc/*  shsu */!=__continue/* EXTERNAL */){
            return _kc/*  shsu */;
          }
        }
      },
      _lb/* shst */ = function(_lc/* shsv */, _ld/* shsw */, _le/* shsx */, _/* EXTERNAL */){
        var _lf/* shsB */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _lc/* shsv */, _4/* GHC.Types.[] */))),
        _lg/* shsF */ = __has/* EXTERNAL */(_k8/* shss */, _lf/* shsB */);
        if(!E(_lg/* shsF */)){
          return new F(function(){return _k9/* shsu */(_ld/* shsw */, _le/* shsx */, _/* EXTERNAL */);});
        }else{
          var _lh/* shsK */ = __get/* EXTERNAL */(_k8/* shss */, _lf/* shsB */),
          _li/* shsN */ = E(_9N/* LudoJS.$fToAnyOption5 */),
          _lj/* shsQ */ = __get/* EXTERNAL */(_lh/* shsK */, _li/* shsN */),
          _lk/* shsU */ = String/* EXTERNAL */(_lj/* shsQ */),
          _ll/* shsX */ = E(_hW/* LudoJS.lvl35 */),
          _lm/* sht0 */ = strEq/* EXTERNAL */(_lk/* shsU */, _ll/* shsX */);
          if(!E(_lm/* sht0 */)){
            var _ln/* shuc */ = E(_hV/* LudoJS.lvl34 */),
            _lo/* shuf */ = strEq/* EXTERNAL */(_lk/* shsU */, _ln/* shuc */);
            if(!E(_lo/* shuf */)){
              return E(_hU/* LudoJS.lvl33 */);
            }else{
              var _lp/* shuj */ = E(_9M/* LudoJS.$fToAnyOption1 */),
              _lq/* shum */ = __get/* EXTERNAL */(_lh/* shsK */, _lp/* shuj */),
              _lr/* shup */ = function(_ls/*  shuq */, _lt/*  shur */, _/* EXTERNAL */){
                while(1){
                  var _lu/*  shup */ = B((function(_lv/* shuq */, _lw/* shur */, _/* EXTERNAL */){
                    var _lx/* shut */ = E(_lv/* shuq */);
                    if(!_lx/* shut */._){
                      return _lw/* shur */;
                    }else{
                      var _ly/* shuv */ = _lx/* shut */.b,
                      _lz/* shuw */ = E(_lx/* shut */.a),
                      _lA/* shuA */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _lz/* shuw */, _4/* GHC.Types.[] */))),
                      _lB/* shuE */ = __has/* EXTERNAL */(_k8/* shss */, _lA/* shuA */);
                      if(!E(_lB/* shuE */)){
                        var _lC/*   shur */ = _lw/* shur */;
                        _ls/*  shuq */ = _ly/* shuv */;
                        _lt/*  shur */ = _lC/*   shur */;
                        return __continue/* EXTERNAL */;
                      }else{
                        var _lD/* shuJ */ = __get/* EXTERNAL */(_k8/* shss */, _lA/* shuA */),
                        _lE/* shuN */ = __get/* EXTERNAL */(_lD/* shuJ */, _li/* shsN */),
                        _lF/* shuR */ = String/* EXTERNAL */(_lE/* shuN */),
                        _lG/* shuV */ = strEq/* EXTERNAL */(_lF/* shuR */, _ll/* shsX */);
                        if(!E(_lG/* shuV */)){
                          var _lH/* shv3 */ = strEq/* EXTERNAL */(_lF/* shuR */, _ln/* shuc */);
                          if(!E(_lH/* shv3 */)){
                            return E(_hU/* LudoJS.lvl33 */);
                          }else{
                            var _lI/* shv8 */ = __get/* EXTERNAL */(_lD/* shuJ */, _lp/* shuj */),
                            _lJ/* shvn */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_lw/* shur */, new T2(1,new T2(0,_lz/* shuw */,new T1(1,new T(function(){
                                var _lK/* shvc */ = Number/* EXTERNAL */(_lI/* shv8 */);
                                return jsTrunc/* EXTERNAL */(_lK/* shvc */);
                              }))),_4/* GHC.Types.[] */)));
                            });
                            _ls/*  shuq */ = _ly/* shuv */;
                            _lt/*  shur */ = _lJ/* shvn */;
                            return __continue/* EXTERNAL */;
                          }
                        }else{
                          _ls/*  shuq */ = _ly/* shuv */;
                          _lt/*  shur */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_lw/* shur */, new T2(1,new T2(0,_lz/* shuw */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                          });
                          return __continue/* EXTERNAL */;
                        }
                      }
                    }
                  })(_ls/*  shuq */, _lt/*  shur */, _/* EXTERNAL */));
                  if(_lu/*  shup */!=__continue/* EXTERNAL */){
                    return _lu/*  shup */;
                  }
                }
              },
              _lL/* shvC */ = new T(function(){
                return B(_q/* GHC.Base.++ */(_le/* shsx */, new T2(1,new T2(0,_lc/* shsv */,new T1(1,new T(function(){
                  var _lM/* shvr */ = Number/* EXTERNAL */(_lq/* shum */);
                  return jsTrunc/* EXTERNAL */(_lM/* shvr */);
                }))),_4/* GHC.Types.[] */)));
              });
              return new F(function(){return _lr/* shup */(_ld/* shsw */, _lL/* shvC */, _/* EXTERNAL */);});
            }
          }else{
            var _lN/* sht4 */ = function(_lO/*  sht5 */, _lP/*  sht6 */, _/* EXTERNAL */){
              while(1){
                var _lQ/*  sht4 */ = B((function(_lR/* sht5 */, _lS/* sht6 */, _/* EXTERNAL */){
                  var _lT/* sht8 */ = E(_lR/* sht5 */);
                  if(!_lT/* sht8 */._){
                    return _lS/* sht6 */;
                  }else{
                    var _lU/* shta */ = _lT/* sht8 */.b,
                    _lV/* shtb */ = E(_lT/* sht8 */.a),
                    _lW/* shtf */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _lV/* shtb */, _4/* GHC.Types.[] */))),
                    _lX/* shtj */ = __has/* EXTERNAL */(_k8/* shss */, _lW/* shtf */);
                    if(!E(_lX/* shtj */)){
                      var _lY/*   sht6 */ = _lS/* sht6 */;
                      _lO/*  sht5 */ = _lU/* shta */;
                      _lP/*  sht6 */ = _lY/*   sht6 */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _lZ/* shto */ = __get/* EXTERNAL */(_k8/* shss */, _lW/* shtf */),
                      _m0/* shts */ = __get/* EXTERNAL */(_lZ/* shto */, _li/* shsN */),
                      _m1/* shtw */ = String/* EXTERNAL */(_m0/* shts */),
                      _m2/* shtA */ = strEq/* EXTERNAL */(_m1/* shtw */, _ll/* shsX */);
                      if(!E(_m2/* shtA */)){
                        var _m3/* shtK */ = strEq/* EXTERNAL */(_m1/* shtw */, E(_hV/* LudoJS.lvl34 */));
                        if(!E(_m3/* shtK */)){
                          return E(_hU/* LudoJS.lvl33 */);
                        }else{
                          var _m4/* shtR */ = __get/* EXTERNAL */(_lZ/* shto */, E(_9M/* LudoJS.$fToAnyOption1 */)),
                          _m5/* shu6 */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_lS/* sht6 */, new T2(1,new T2(0,_lV/* shtb */,new T1(1,new T(function(){
                              var _m6/* shtV */ = Number/* EXTERNAL */(_m4/* shtR */);
                              return jsTrunc/* EXTERNAL */(_m6/* shtV */);
                            }))),_4/* GHC.Types.[] */)));
                          });
                          _lO/*  sht5 */ = _lU/* shta */;
                          _lP/*  sht6 */ = _m5/* shu6 */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        _lO/*  sht5 */ = _lU/* shta */;
                        _lP/*  sht6 */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_lS/* sht6 */, new T2(1,new T2(0,_lV/* shtb */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                        });
                        return __continue/* EXTERNAL */;
                      }
                    }
                  }
                })(_lO/*  sht5 */, _lP/*  sht6 */, _/* EXTERNAL */));
                if(_lQ/*  sht4 */!=__continue/* EXTERNAL */){
                  return _lQ/*  sht4 */;
                }
              }
            };
            return new F(function(){return _lN/* sht4 */(_ld/* shsw */, new T(function(){
              return B(_q/* GHC.Base.++ */(_le/* shsx */, new T2(1,new T2(0,_lc/* shsv */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
            }), _/* EXTERNAL */);});
          }
        }
      },
      _m7/* shyP */ = B(_lb/* shst */(1, _hT/* LudoJS.a34 */, _4/* GHC.Types.[] */, _/* EXTERNAL */)),
      _m8/* shyS */ = B(_k3/* shly */(_k5/* shsh */.b, _/* EXTERNAL */));
      return new T2(1,new T2(0,_k6/* shsi */,_m7/* shyP */),_m8/* shyS */);
    }
  },
  _m9/* shlx */ = function(_ma/* shlz */, _mb/* shlA */, _/* EXTERNAL */){
    var _mc/* shlI */ = __get/* EXTERNAL */(_i1/* sheX */, toJSStr/* EXTERNAL */(B(_hL/* LudoJS.$w$cshowsPrec */(_ma/* shlz */, _4/* GHC.Types.[] */)))),
    _md/* shlK */ = _mc/* shlI */,
    _me/* shlM */ = function(_mf/*  shoW */, _mg/*  shoX */, _/* EXTERNAL */){
      while(1){
        var _mh/*  shlM */ = B((function(_mi/* shoW */, _mj/* shoX */, _/* EXTERNAL */){
          var _mk/* shoZ */ = E(_mi/* shoW */);
          if(!_mk/* shoZ */._){
            return _mj/* shoX */;
          }else{
            var _ml/* shp1 */ = _mk/* shoZ */.b,
            _mm/* shp2 */ = E(_mk/* shoZ */.a),
            _mn/* shp6 */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _mm/* shp2 */, _4/* GHC.Types.[] */))),
            _mo/* shpa */ = __has/* EXTERNAL */(_md/* shlK */, _mn/* shp6 */);
            if(!E(_mo/* shpa */)){
              var _mp/*   shoX */ = _mj/* shoX */;
              _mf/*  shoW */ = _ml/* shp1 */;
              _mg/*  shoX */ = _mp/*   shoX */;
              return __continue/* EXTERNAL */;
            }else{
              var _mq/* shpf */ = __get/* EXTERNAL */(_md/* shlK */, _mn/* shp6 */),
              _mr/* shpi */ = E(_9N/* LudoJS.$fToAnyOption5 */),
              _ms/* shpl */ = __get/* EXTERNAL */(_mq/* shpf */, _mr/* shpi */),
              _mt/* shpp */ = String/* EXTERNAL */(_ms/* shpl */),
              _mu/* shps */ = E(_hW/* LudoJS.lvl35 */),
              _mv/* shpv */ = strEq/* EXTERNAL */(_mt/* shpp */, _mu/* shps */);
              if(!E(_mv/* shpv */)){
                var _mw/* shqG */ = E(_hV/* LudoJS.lvl34 */),
                _mx/* shqJ */ = strEq/* EXTERNAL */(_mt/* shpp */, _mw/* shqG */);
                if(!E(_mx/* shqJ */)){
                  return E(_hU/* LudoJS.lvl33 */);
                }else{
                  var _my/* shqN */ = E(_9M/* LudoJS.$fToAnyOption1 */),
                  _mz/* shqQ */ = __get/* EXTERNAL */(_mq/* shpf */, _my/* shqN */),
                  _mA/* shqT */ = function(_mB/*  shqU */, _mC/*  shqV */, _/* EXTERNAL */){
                    while(1){
                      var _mD/*  shqT */ = B((function(_mE/* shqU */, _mF/* shqV */, _/* EXTERNAL */){
                        var _mG/* shqX */ = E(_mE/* shqU */);
                        if(!_mG/* shqX */._){
                          return _mF/* shqV */;
                        }else{
                          var _mH/* shqZ */ = _mG/* shqX */.b,
                          _mI/* shr0 */ = E(_mG/* shqX */.a),
                          _mJ/* shr4 */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _mI/* shr0 */, _4/* GHC.Types.[] */))),
                          _mK/* shr8 */ = __has/* EXTERNAL */(_md/* shlK */, _mJ/* shr4 */);
                          if(!E(_mK/* shr8 */)){
                            var _mL/*   shqV */ = _mF/* shqV */;
                            _mB/*  shqU */ = _mH/* shqZ */;
                            _mC/*  shqV */ = _mL/*   shqV */;
                            return __continue/* EXTERNAL */;
                          }else{
                            var _mM/* shrd */ = __get/* EXTERNAL */(_md/* shlK */, _mJ/* shr4 */),
                            _mN/* shrh */ = __get/* EXTERNAL */(_mM/* shrd */, _mr/* shpi */),
                            _mO/* shrl */ = String/* EXTERNAL */(_mN/* shrh */),
                            _mP/* shrp */ = strEq/* EXTERNAL */(_mO/* shrl */, _mu/* shps */);
                            if(!E(_mP/* shrp */)){
                              var _mQ/* shrx */ = strEq/* EXTERNAL */(_mO/* shrl */, _mw/* shqG */);
                              if(!E(_mQ/* shrx */)){
                                return E(_hU/* LudoJS.lvl33 */);
                              }else{
                                var _mR/* shrC */ = __get/* EXTERNAL */(_mM/* shrd */, _my/* shqN */),
                                _mS/* shrR */ = new T(function(){
                                  return B(_q/* GHC.Base.++ */(_mF/* shqV */, new T2(1,new T2(0,_mI/* shr0 */,new T1(1,new T(function(){
                                    var _mT/* shrG */ = Number/* EXTERNAL */(_mR/* shrC */);
                                    return jsTrunc/* EXTERNAL */(_mT/* shrG */);
                                  }))),_4/* GHC.Types.[] */)));
                                });
                                _mB/*  shqU */ = _mH/* shqZ */;
                                _mC/*  shqV */ = _mS/* shrR */;
                                return __continue/* EXTERNAL */;
                              }
                            }else{
                              _mB/*  shqU */ = _mH/* shqZ */;
                              _mC/*  shqV */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_mF/* shqV */, new T2(1,new T2(0,_mI/* shr0 */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                              });
                              return __continue/* EXTERNAL */;
                            }
                          }
                        }
                      })(_mB/*  shqU */, _mC/*  shqV */, _/* EXTERNAL */));
                      if(_mD/*  shqT */!=__continue/* EXTERNAL */){
                        return _mD/*  shqT */;
                      }
                    }
                  },
                  _mU/* shs5 */ = new T(function(){
                    return B(_q/* GHC.Base.++ */(_mj/* shoX */, new T2(1,new T2(0,_mm/* shp2 */,new T1(1,new T(function(){
                      var _mV/* shrU */ = Number/* EXTERNAL */(_mz/* shqQ */);
                      return jsTrunc/* EXTERNAL */(_mV/* shrU */);
                    }))),_4/* GHC.Types.[] */)));
                  });
                  return new F(function(){return _mA/* shqT */(_ml/* shp1 */, _mU/* shs5 */, _/* EXTERNAL */);});
                }
              }else{
                var _mW/* shpz */ = function(_mX/*  shpA */, _mY/*  shpB */, _/* EXTERNAL */){
                  while(1){
                    var _mZ/*  shpz */ = B((function(_n0/* shpA */, _n1/* shpB */, _/* EXTERNAL */){
                      var _n2/* shpD */ = E(_n0/* shpA */);
                      if(!_n2/* shpD */._){
                        return _n1/* shpB */;
                      }else{
                        var _n3/* shpF */ = _n2/* shpD */.b,
                        _n4/* shpG */ = E(_n2/* shpD */.a),
                        _n5/* shpK */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _n4/* shpG */, _4/* GHC.Types.[] */))),
                        _n6/* shpO */ = __has/* EXTERNAL */(_md/* shlK */, _n5/* shpK */);
                        if(!E(_n6/* shpO */)){
                          var _n7/*   shpB */ = _n1/* shpB */;
                          _mX/*  shpA */ = _n3/* shpF */;
                          _mY/*  shpB */ = _n7/*   shpB */;
                          return __continue/* EXTERNAL */;
                        }else{
                          var _n8/* shpT */ = __get/* EXTERNAL */(_md/* shlK */, _n5/* shpK */),
                          _n9/* shpX */ = __get/* EXTERNAL */(_n8/* shpT */, _mr/* shpi */),
                          _na/* shq1 */ = String/* EXTERNAL */(_n9/* shpX */),
                          _nb/* shq5 */ = strEq/* EXTERNAL */(_na/* shq1 */, _mu/* shps */);
                          if(!E(_nb/* shq5 */)){
                            var _nc/* shqf */ = strEq/* EXTERNAL */(_na/* shq1 */, E(_hV/* LudoJS.lvl34 */));
                            if(!E(_nc/* shqf */)){
                              return E(_hU/* LudoJS.lvl33 */);
                            }else{
                              var _nd/* shqm */ = __get/* EXTERNAL */(_n8/* shpT */, E(_9M/* LudoJS.$fToAnyOption1 */)),
                              _ne/* shqB */ = new T(function(){
                                return B(_q/* GHC.Base.++ */(_n1/* shpB */, new T2(1,new T2(0,_n4/* shpG */,new T1(1,new T(function(){
                                  var _nf/* shqq */ = Number/* EXTERNAL */(_nd/* shqm */);
                                  return jsTrunc/* EXTERNAL */(_nf/* shqq */);
                                }))),_4/* GHC.Types.[] */)));
                              });
                              _mX/*  shpA */ = _n3/* shpF */;
                              _mY/*  shpB */ = _ne/* shqB */;
                              return __continue/* EXTERNAL */;
                            }
                          }else{
                            _mX/*  shpA */ = _n3/* shpF */;
                            _mY/*  shpB */ = new T(function(){
                              return B(_q/* GHC.Base.++ */(_n1/* shpB */, new T2(1,new T2(0,_n4/* shpG */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                            });
                            return __continue/* EXTERNAL */;
                          }
                        }
                      }
                    })(_mX/*  shpA */, _mY/*  shpB */, _/* EXTERNAL */));
                    if(_mZ/*  shpz */!=__continue/* EXTERNAL */){
                      return _mZ/*  shpz */;
                    }
                  }
                };
                return new F(function(){return _mW/* shpz */(_ml/* shp1 */, new T(function(){
                  return B(_q/* GHC.Base.++ */(_mj/* shoX */, new T2(1,new T2(0,_mm/* shp2 */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                }), _/* EXTERNAL */);});
              }
            }
          }
        })(_mf/*  shoW */, _mg/*  shoX */, _/* EXTERNAL */));
        if(_mh/*  shlM */!=__continue/* EXTERNAL */){
          return _mh/*  shlM */;
        }
      }
    },
    _ng/* shlL */ = function(_nh/* shlN */, _ni/* shlO */, _nj/* shlP */, _/* EXTERNAL */){
      var _nk/* shlT */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _nh/* shlN */, _4/* GHC.Types.[] */))),
      _nl/* shlX */ = __has/* EXTERNAL */(_md/* shlK */, _nk/* shlT */);
      if(!E(_nl/* shlX */)){
        return new F(function(){return _me/* shlM */(_ni/* shlO */, _nj/* shlP */, _/* EXTERNAL */);});
      }else{
        var _nm/* shm2 */ = __get/* EXTERNAL */(_md/* shlK */, _nk/* shlT */),
        _nn/* shm5 */ = E(_9N/* LudoJS.$fToAnyOption5 */),
        _no/* shm8 */ = __get/* EXTERNAL */(_nm/* shm2 */, _nn/* shm5 */),
        _np/* shmc */ = String/* EXTERNAL */(_no/* shm8 */),
        _nq/* shmf */ = E(_hW/* LudoJS.lvl35 */),
        _nr/* shmi */ = strEq/* EXTERNAL */(_np/* shmc */, _nq/* shmf */);
        if(!E(_nr/* shmi */)){
          var _ns/* shnu */ = E(_hV/* LudoJS.lvl34 */),
          _nt/* shnx */ = strEq/* EXTERNAL */(_np/* shmc */, _ns/* shnu */);
          if(!E(_nt/* shnx */)){
            return E(_hU/* LudoJS.lvl33 */);
          }else{
            var _nu/* shnB */ = E(_9M/* LudoJS.$fToAnyOption1 */),
            _nv/* shnE */ = __get/* EXTERNAL */(_nm/* shm2 */, _nu/* shnB */),
            _nw/* shnH */ = function(_nx/*  shnI */, _ny/*  shnJ */, _/* EXTERNAL */){
              while(1){
                var _nz/*  shnH */ = B((function(_nA/* shnI */, _nB/* shnJ */, _/* EXTERNAL */){
                  var _nC/* shnL */ = E(_nA/* shnI */);
                  if(!_nC/* shnL */._){
                    return _nB/* shnJ */;
                  }else{
                    var _nD/* shnN */ = _nC/* shnL */.b,
                    _nE/* shnO */ = E(_nC/* shnL */.a),
                    _nF/* shnS */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _nE/* shnO */, _4/* GHC.Types.[] */))),
                    _nG/* shnW */ = __has/* EXTERNAL */(_md/* shlK */, _nF/* shnS */);
                    if(!E(_nG/* shnW */)){
                      var _nH/*   shnJ */ = _nB/* shnJ */;
                      _nx/*  shnI */ = _nD/* shnN */;
                      _ny/*  shnJ */ = _nH/*   shnJ */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _nI/* sho1 */ = __get/* EXTERNAL */(_md/* shlK */, _nF/* shnS */),
                      _nJ/* sho5 */ = __get/* EXTERNAL */(_nI/* sho1 */, _nn/* shm5 */),
                      _nK/* sho9 */ = String/* EXTERNAL */(_nJ/* sho5 */),
                      _nL/* shod */ = strEq/* EXTERNAL */(_nK/* sho9 */, _nq/* shmf */);
                      if(!E(_nL/* shod */)){
                        var _nM/* shol */ = strEq/* EXTERNAL */(_nK/* sho9 */, _ns/* shnu */);
                        if(!E(_nM/* shol */)){
                          return E(_hU/* LudoJS.lvl33 */);
                        }else{
                          var _nN/* shoq */ = __get/* EXTERNAL */(_nI/* sho1 */, _nu/* shnB */),
                          _nO/* shoF */ = new T(function(){
                            return B(_q/* GHC.Base.++ */(_nB/* shnJ */, new T2(1,new T2(0,_nE/* shnO */,new T1(1,new T(function(){
                              var _nP/* shou */ = Number/* EXTERNAL */(_nN/* shoq */);
                              return jsTrunc/* EXTERNAL */(_nP/* shou */);
                            }))),_4/* GHC.Types.[] */)));
                          });
                          _nx/*  shnI */ = _nD/* shnN */;
                          _ny/*  shnJ */ = _nO/* shoF */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        _nx/*  shnI */ = _nD/* shnN */;
                        _ny/*  shnJ */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_nB/* shnJ */, new T2(1,new T2(0,_nE/* shnO */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                        });
                        return __continue/* EXTERNAL */;
                      }
                    }
                  }
                })(_nx/*  shnI */, _ny/*  shnJ */, _/* EXTERNAL */));
                if(_nz/*  shnH */!=__continue/* EXTERNAL */){
                  return _nz/*  shnH */;
                }
              }
            },
            _nQ/* shoU */ = new T(function(){
              return B(_q/* GHC.Base.++ */(_nj/* shlP */, new T2(1,new T2(0,_nh/* shlN */,new T1(1,new T(function(){
                var _nR/* shoJ */ = Number/* EXTERNAL */(_nv/* shnE */);
                return jsTrunc/* EXTERNAL */(_nR/* shoJ */);
              }))),_4/* GHC.Types.[] */)));
            });
            return new F(function(){return _nw/* shnH */(_ni/* shlO */, _nQ/* shoU */, _/* EXTERNAL */);});
          }
        }else{
          var _nS/* shmm */ = function(_nT/*  shmn */, _nU/*  shmo */, _/* EXTERNAL */){
            while(1){
              var _nV/*  shmm */ = B((function(_nW/* shmn */, _nX/* shmo */, _/* EXTERNAL */){
                var _nY/* shmq */ = E(_nW/* shmn */);
                if(!_nY/* shmq */._){
                  return _nX/* shmo */;
                }else{
                  var _nZ/* shms */ = _nY/* shmq */.b,
                  _o0/* shmt */ = E(_nY/* shmq */.a),
                  _o1/* shmx */ = toJSStr/* EXTERNAL */(B(_5L/* GHC.Show.$wshowSignedInt */(0, _o0/* shmt */, _4/* GHC.Types.[] */))),
                  _o2/* shmB */ = __has/* EXTERNAL */(_md/* shlK */, _o1/* shmx */);
                  if(!E(_o2/* shmB */)){
                    var _o3/*   shmo */ = _nX/* shmo */;
                    _nT/*  shmn */ = _nZ/* shms */;
                    _nU/*  shmo */ = _o3/*   shmo */;
                    return __continue/* EXTERNAL */;
                  }else{
                    var _o4/* shmG */ = __get/* EXTERNAL */(_md/* shlK */, _o1/* shmx */),
                    _o5/* shmK */ = __get/* EXTERNAL */(_o4/* shmG */, _nn/* shm5 */),
                    _o6/* shmO */ = String/* EXTERNAL */(_o5/* shmK */),
                    _o7/* shmS */ = strEq/* EXTERNAL */(_o6/* shmO */, _nq/* shmf */);
                    if(!E(_o7/* shmS */)){
                      var _o8/* shn2 */ = strEq/* EXTERNAL */(_o6/* shmO */, E(_hV/* LudoJS.lvl34 */));
                      if(!E(_o8/* shn2 */)){
                        return E(_hU/* LudoJS.lvl33 */);
                      }else{
                        var _o9/* shn9 */ = __get/* EXTERNAL */(_o4/* shmG */, E(_9M/* LudoJS.$fToAnyOption1 */)),
                        _oa/* shno */ = new T(function(){
                          return B(_q/* GHC.Base.++ */(_nX/* shmo */, new T2(1,new T2(0,_o0/* shmt */,new T1(1,new T(function(){
                            var _ob/* shnd */ = Number/* EXTERNAL */(_o9/* shn9 */);
                            return jsTrunc/* EXTERNAL */(_ob/* shnd */);
                          }))),_4/* GHC.Types.[] */)));
                        });
                        _nT/*  shmn */ = _nZ/* shms */;
                        _nU/*  shmo */ = _oa/* shno */;
                        return __continue/* EXTERNAL */;
                      }
                    }else{
                      _nT/*  shmn */ = _nZ/* shms */;
                      _nU/*  shmo */ = new T(function(){
                        return B(_q/* GHC.Base.++ */(_nX/* shmo */, new T2(1,new T2(0,_o0/* shmt */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
                      });
                      return __continue/* EXTERNAL */;
                    }
                  }
                }
              })(_nT/*  shmn */, _nU/*  shmo */, _/* EXTERNAL */));
              if(_nV/*  shmm */!=__continue/* EXTERNAL */){
                return _nV/*  shmm */;
              }
            }
          };
          return new F(function(){return _nS/* shmm */(_ni/* shlO */, new T(function(){
            return B(_q/* GHC.Base.++ */(_nj/* shlP */, new T2(1,new T2(0,_nh/* shlN */,_hO/* LudoJS.Out */),_4/* GHC.Types.[] */)));
          }), _/* EXTERNAL */);});
        }
      }
    },
    _oc/* shs7 */ = B(_ng/* shlL */(1, _hT/* LudoJS.a34 */, _4/* GHC.Types.[] */, _/* EXTERNAL */)),
    _od/* shsa */ = B(_k3/* shly */(_mb/* shlA */, _/* EXTERNAL */));
    return new T2(1,new T2(0,_ma/* shlz */,_oc/* shs7 */),_od/* shsa */);
  },
  _oe/* shyX */ = B(_m9/* shlx */(_bo/* LudoJS.Green */, _hY/* LudoJS.lvl9 */, _/* EXTERNAL */));
  return new T(function(){
    return B(_hy/* LudoJS.$sfromList */(new T2(1,new T2(0,_bn/* LudoJS.Blue */,_k2/* shlu */),_oe/* shyX */)));
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
_ol/* $wa2 */ = function(_om/* sgOB */, _/* EXTERNAL */){
  var _on/* sgOG */ = __get/* EXTERNAL */(_om/* sgOB */, E(_95/* LudoJS.$fFromAnyGameState16 */)),
  _oo/* sgOK */ = String/* EXTERNAL */(_on/* sgOG */),
  _op/* sgOQ */ = strEq/* EXTERNAL */(_oo/* sgOK */, E(_ok/* LudoJS.lvl32 */));
  if(!E(_op/* sgOQ */)){
    var _oq/* sgPf */ = strEq/* EXTERNAL */(_oo/* sgOK */, E(_oj/* LudoJS.lvl31 */));
    if(!E(_oq/* sgPf */)){
      var _or/* sgPC */ = strEq/* EXTERNAL */(_oo/* sgOK */, E(_oi/* LudoJS.lvl30 */));
      if(!E(_or/* sgPC */)){
        var _os/* sgQe */ = strEq/* EXTERNAL */(_oo/* sgOK */, E(_oh/* LudoJS.lvl29 */));
        return (E(_os/* sgQe */)==0) ? E(_og/* LudoJS.lvl28 */) : _of/* LudoJS.GameFinished */;
      }else{
        var _ot/* sgPJ */ = __get/* EXTERNAL */(_om/* sgOB */, E(_9s/* LudoJS.$fToAnyGameState11 */)),
        _ou/* sgPP */ = __get/* EXTERNAL */(_om/* sgOB */, E(_9D/* LudoJS.$fToAnyGameState6 */));
        return new T2(2,new T(function(){
          var _ov/* sgPT */ = Number/* EXTERNAL */(_ot/* sgPJ */);
          return jsTrunc/* EXTERNAL */(_ov/* sgPT */);
        }),new T(function(){
          var _ow/* sgQ2 */ = Number/* EXTERNAL */(_ou/* sgPP */);
          return jsTrunc/* EXTERNAL */(_ow/* sgQ2 */);
        }));
      }
    }else{
      var _ox/* sgPm */ = __get/* EXTERNAL */(_om/* sgOB */, E(_9s/* LudoJS.$fToAnyGameState11 */));
      return new T1(1,new T(function(){
        var _oy/* sgPq */ = Number/* EXTERNAL */(_ox/* sgPm */);
        return jsTrunc/* EXTERNAL */(_oy/* sgPq */);
      }));
    }
  }else{
    var _oz/* sgOX */ = __get/* EXTERNAL */(_om/* sgOB */, E(_9s/* LudoJS.$fToAnyGameState11 */));
    return new T1(0,new T(function(){
      var _oA/* sgP1 */ = Number/* EXTERNAL */(_oz/* sgOX */),
      _oB/* sgP5 */ = jsTrunc/* EXTERNAL */(_oA/* sgP1 */),
      _oC/* sgP8 */ = E(_oB/* sgP5 */);
      if(_oC/* sgP8 */==( -1)){
        return __Z/* EXTERNAL */;
      }else{
        return new T1(1,_oC/* sgP8 */);
      }
    }));
  }
},
_oD/* $wa1 */ = function(_oE/* shzq */, _/* EXTERNAL */){
  var _oF/* shzv */ = __get/* EXTERNAL */(_oE/* shzq */, E(_95/* LudoJS.$fFromAnyGameState16 */)),
  _oG/* shzy */ = B(_ol/* LudoJS.$wa2 */(_oF/* shzv */, _/* EXTERNAL */)),
  _oH/* shzE */ = __get/* EXTERNAL */(_oE/* shzq */, E(_94/* LudoJS.$fFromAnyGameState15 */)),
  _oI/* shzI */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_oH/* shzE */, _/* EXTERNAL */)),
  _oJ/* shzO */ = __get/* EXTERNAL */(_oE/* shzq */, E(_98/* LudoJS.$fFromAnyGameState8 */)),
  _oK/* shzU */ = __get/* EXTERNAL */(_oE/* shzq */, E(_97/* LudoJS.$fFromAnyGameState7 */)),
  _oL/* shzY */ = B(_hZ/* LudoJS.$fFromAnyGameState6 */(_oK/* shzU */, _/* EXTERNAL */)),
  _oM/* shA4 */ = __get/* EXTERNAL */(_oE/* shzq */, E(_96/* LudoJS.$fFromAnyGameState5 */)),
  _oN/* shA8 */ = __arr2lst/* EXTERNAL */(0, _oM/* shA4 */),
  _oO/* shAc */ = B(_dM/* LudoJS.$fFromAnyGameState4 */(_oN/* shA8 */, _/* EXTERNAL */));
  return new T5(0,_oG/* shzy */,_oI/* shzI */,new T(function(){
    var _oP/* shAg */ = Number/* EXTERNAL */(_oJ/* shzO */);
    return jsTrunc/* EXTERNAL */(_oP/* shAg */);
  }),_oL/* shzY */,_oO/* shAc */);
},
_oQ/* go2 */ = function(_oR/* shEs */){
  while(1){
    var _oS/* shEt */ = E(_oR/* shEs */);
    if(!_oS/* shEt */._){
      return true;
    }else{
      if(!E(E(_oS/* shEt */.a).b)._){
        _oR/* shEs */ = _oS/* shEt */.b;
        continue;
      }else{
        return false;
      }
    }
  }
},
_oT/* lvl7 */ = 1,
_oU/* $wa12 */ = function(_oV/* shEB */, _/* EXTERNAL */){
  var _oW/* shEW */ = new T(function(){
    var _oX/* shED */ = E(_oV/* shEB */),
    _oY/* shEH */ = _oX/* shED */.d,
    _oZ/* shEJ */ = new T(function(){
      switch(E(_oX/* shED */.b)){
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
    return new T5(0,_oX/* shED */.a,_oZ/* shEJ */,new T(function(){
      if(!B(_oQ/* LudoJS.go2 */(B(_bt/* LudoJS.$s!1 */(_oZ/* shEJ */, _oY/* shEH */))))){
        return E(_oT/* LudoJS.lvl7 */);
      }else{
        return E(_b7/* LudoJS.play10 */);
      }
    }),_oY/* shEH */,_oX/* shED */.e);
  });
  return new T2(0,_2s/* GHC.Tuple.() */,_oW/* shEW */);
},
_p0/* $wa13 */ = function(_p1/* shEY */, _p2/* shEZ */, _p3/* shF0 */, _p4/* shF1 */, _p5/* shF2 */, _/* EXTERNAL */){
  if(_p3/* shF0 */>=1){
    return new T2(0,_4/* GHC.Types.[] */,new T5(0,_p1/* shEY */,_p2/* shEZ */,_p3/* shF0 */,_p4/* shF1 */,_p5/* shF2 */));
  }else{
    var _p6/* shFb */ = B(_oU/* LudoJS.$wa12 */(new T5(0,_p1/* shEY */,_p2/* shEZ */,_p3/* shF0 */,_p4/* shF1 */,_p5/* shF2 */), _/* EXTERNAL */)),
    _p7/* shFe */ = E(_p6/* shFb */),
    _p8/* shFh */ = E(_p7/* shFe */.b),
    _p9/* shFp */ = B(_p0/* LudoJS.$wa13 */(_p8/* shFh */.a, _p8/* shFh */.b, E(_p8/* shFh */.c), _p8/* shFh */.d, _p8/* shFh */.e, _/* EXTERNAL */));
    return new T2(0,new T2(1,_p7/* shFe */.a,new T(function(){
      return E(E(_p9/* shFp */).a);
    })),new T(function(){
      return E(E(_p9/* shFp */).b);
    }));
  }
},
_pa/* $wa14 */ = function(_pb/* shFC */, _pc/* shFD */, _pd/* shFE */, _pe/* shFF */, _pf/* shFG */, _/* EXTERNAL */){
  var _pg/* shFK */ = new T5(0,_pb/* shFC */,_pc/* shFD */,_pd/* shFE */,_pe/* shFF */,_pf/* shFG */),
  _ph/* shFL */ = function(_pi/* shFM */){
    var _pj/* shFN */ = B(_oU/* LudoJS.$wa12 */(_pg/* shFK */, _/* EXTERNAL */)),
    _pk/* shFQ */ = E(_pj/* shFN */),
    _pl/* shFT */ = E(_pk/* shFQ */.b),
    _pm/* shFZ */ = B(_pa/* LudoJS.$wa14 */(_pl/* shFT */.a, _pl/* shFT */.b, _pl/* shFT */.c, _pl/* shFT */.d, _pl/* shFT */.e, _/* EXTERNAL */));
    return new T2(0,new T2(1,_pk/* shFQ */.a,new T(function(){
      return E(E(_pm/* shFZ */).a);
    })),new T(function(){
      return E(E(_pm/* shFZ */).b);
    }));
  };
  if(!E(B(_bB/* GHC.List.$wlenAcc */(B(_bt/* LudoJS.$s!1 */(_pc/* shFD */, _pe/* shFF */)), 0)))){
    return new F(function(){return _ph/* shFL */(_/* EXTERNAL */);});
  }else{
    if(E(_pd/* shFE */)>=1){
      return new T2(0,_4/* GHC.Types.[] */,_pg/* shFK */);
    }else{
      return new F(function(){return _ph/* shFL */(_/* EXTERNAL */);});
    }
  }
},
_pn/* neInt */ = function(_po/* scEM */, _pp/* scEN */){
  return E(_po/* scEM */)!=E(_pp/* scEN */);
},
_pq/* $fEqInt */ = new T2(0,_b9/* GHC.Classes.eqInt */,_pn/* GHC.Classes.neInt */),
_pr/* $soutByCell */ = function(_ps/* sgTw */, _pt/* sgTx */){
  var _pu/* sgTy */ = E(_ps/* sgTw */);
  if(!_pu/* sgTy */._){
    return __Z/* EXTERNAL */;
  }else{
    var _pv/* sgTA */ = _pu/* sgTy */.b,
    _pw/* sgTB */ = E(_pu/* sgTy */.a),
    _px/* sgTE */ = E(_pw/* sgTB */.b);
    return (_px/* sgTE */._==0) ? new T2(1,_pw/* sgTB */,new T(function(){
      return B(_pr/* LudoJS.$soutByCell */(_pv/* sgTA */, _pt/* sgTx */));
    })) : (_pt/* sgTx */!=E(_px/* sgTE */.a)) ? new T2(1,_pw/* sgTB */,new T(function(){
      return B(_pr/* LudoJS.$soutByCell */(_pv/* sgTA */, _pt/* sgTx */));
    })) : new T2(1,new T2(0,_pw/* sgTB */.a,_hO/* LudoJS.Out */),new T(function(){
      return B(_pr/* LudoJS.$soutByCell */(_pv/* sgTA */, _pt/* sgTx */));
    }));
  }
},
_py/* $sremoveFrom */ = function(_pz/* sgU8 */, _pA/* sgU9 */){
  var _pB/* sgUa */ = E(_pz/* sgU8 */);
  if(!_pB/* sgUa */._){
    return __Z/* EXTERNAL */;
  }else{
    var _pC/* sgUc */ = _pB/* sgUa */.b,
    _pD/* sgUd */ = E(_pB/* sgUa */.a);
    return (_pA/* sgU9 */!=E(_pD/* sgUd */.a)) ? new T2(1,_pD/* sgUd */,new T(function(){
      return B(_py/* LudoJS.$sremoveFrom */(_pC/* sgUc */, _pA/* sgU9 */));
    })) : E(_pC/* sgUc */);
  }
},
_pE/* lvl1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("!!: negative index"));
}),
_pF/* prel_list_str */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Prelude."));
}),
_pG/* lvl2 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_pF/* GHC.List.prel_list_str */, _pE/* GHC.List.lvl1 */));
}),
_pH/* negIndex */ = new T(function(){
  return B(err/* EXTERNAL */(_pG/* GHC.List.lvl2 */));
}),
_pI/* lvl3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("!!: index too large"));
}),
_pJ/* lvl4 */ = new T(function(){
  return B(_q/* GHC.Base.++ */(_pF/* GHC.List.prel_list_str */, _pI/* GHC.List.lvl3 */));
}),
_pK/* !!1 */ = new T(function(){
  return B(err/* EXTERNAL */(_pJ/* GHC.List.lvl4 */));
}),
_pL/* poly_$wgo2 */ = function(_pM/* sbFw */, _pN/* sbFx */){
  while(1){
    var _pO/* sbFy */ = E(_pM/* sbFw */);
    if(!_pO/* sbFy */._){
      return E(_pK/* GHC.List.!!1 */);
    }else{
      var _pP/* sbFB */ = E(_pN/* sbFx */);
      if(!_pP/* sbFB */){
        return E(_pO/* sbFy */.a);
      }else{
        _pM/* sbFw */ = _pO/* sbFy */.b;
        _pN/* sbFx */ = _pP/* sbFB */-1|0;
        continue;
      }
    }
  }
},
_pQ/* $w!! */ = function(_pR/* sbFD */, _pS/* sbFE */){
  if(_pS/* sbFE */>=0){
    return new F(function(){return _pL/* GHC.List.poly_$wgo2 */(_pR/* sbFD */, _pS/* sbFE */);});
  }else{
    return E(_pH/* GHC.List.negIndex */);
  }
},
_pT/* $s!_$spoly_go1 */ = function(_pU/* sgK6 */){
  while(1){
    var _pV/* sgK7 */ = E(_pU/* sgK6 */);
    if(!_pV/* sgK7 */._){
      var _pW/* sgKb */ = _pV/* sgK7 */.d;
      switch(E(_pV/* sgK7 */.b)){
        case 0:
          _pU/* sgK6 */ = _pV/* sgK7 */.e;
          continue;
        case 1:
          return E(_pV/* sgK7 */.c);
        case 2:
          _pU/* sgK6 */ = _pW/* sgKb */;
          continue;
        default:
          _pU/* sgK6 */ = _pW/* sgKb */;
          continue;
      }
    }else{
      return E(_bs/* LudoJS.lvl27 */);
    }
  }
},
_pX/* $s!_$spoly_go10 */ = function(_pY/* sgJY */){
  while(1){
    var _pZ/* sgJZ */ = E(_pY/* sgJY */);
    if(!_pZ/* sgJZ */._){
      if(E(_pZ/* sgJZ */.b)==3){
        return E(_pZ/* sgJZ */.c);
      }else{
        _pY/* sgJY */ = _pZ/* sgJZ */.e;
        continue;
      }
    }else{
      return E(_bs/* LudoJS.lvl27 */);
    }
  }
},
_q0/* $s!_$spoly_go2 */ = function(_q1/* sgKe */){
  while(1){
    var _q2/* sgKf */ = E(_q1/* sgKe */);
    if(!_q2/* sgKf */._){
      var _q3/* sgKj */ = _q2/* sgKf */.d;
      switch(E(_q2/* sgKf */.b)){
        case 0:
          return E(_q2/* sgKf */.c);
        case 1:
          _q1/* sgKe */ = _q3/* sgKj */;
          continue;
        case 2:
          _q1/* sgKe */ = _q3/* sgKj */;
          continue;
        default:
          _q1/* sgKe */ = _q3/* sgKj */;
          continue;
      }
    }else{
      return E(_bs/* LudoJS.lvl27 */);
    }
  }
},
_q4/* $sinsert_$s$sgo1 */ = function(_q5/* sgMa */, _q6/* sgMb */){
  var _q7/* sgMc */ = E(_q6/* sgMb */);
  if(!_q7/* sgMc */._){
    var _q8/* sgMf */ = _q7/* sgMc */.c,
    _q9/* sgMg */ = _q7/* sgMc */.d,
    _qa/* sgMh */ = _q7/* sgMc */.e;
    switch(E(_q7/* sgMc */.b)){
      case 0:
        return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bn/* LudoJS.Blue */, _q8/* sgMf */, _q9/* sgMg */, B(_q4/* LudoJS.$sinsert_$s$sgo1 */(_q5/* sgMa */, _qa/* sgMh */)));});
        break;
      case 1:
        return new T5(0,_q7/* sgMc */.a,E(_bo/* LudoJS.Green */),_q5/* sgMa */,E(_q9/* sgMg */),E(_qa/* sgMh */));
      case 2:
        return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bp/* LudoJS.Red */, _q8/* sgMf */, B(_q4/* LudoJS.$sinsert_$s$sgo1 */(_q5/* sgMa */, _q9/* sgMg */)), _qa/* sgMh */);});
        break;
      default:
        return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bq/* LudoJS.Yellow */, _q8/* sgMf */, B(_q4/* LudoJS.$sinsert_$s$sgo1 */(_q5/* sgMa */, _q9/* sgMg */)), _qa/* sgMh */);});
    }
  }else{
    return new T5(0,1,E(_bo/* LudoJS.Green */),_q5/* sgMa */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
  }
},
_qb/* $sinsert_$s$sgo10 */ = function(_qc/* sgLY */, _qd/* sgLZ */){
  var _qe/* sgM0 */ = E(_qd/* sgLZ */);
  if(!_qe/* sgM0 */._){
    var _qf/* sgM3 */ = _qe/* sgM0 */.c,
    _qg/* sgM4 */ = _qe/* sgM0 */.d,
    _qh/* sgM5 */ = _qe/* sgM0 */.e;
    switch(E(_qe/* sgM0 */.b)){
      case 0:
        return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bn/* LudoJS.Blue */, _qf/* sgM3 */, _qg/* sgM4 */, B(_qb/* LudoJS.$sinsert_$s$sgo10 */(_qc/* sgLY */, _qh/* sgM5 */)));});
        break;
      case 1:
        return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bo/* LudoJS.Green */, _qf/* sgM3 */, _qg/* sgM4 */, B(_qb/* LudoJS.$sinsert_$s$sgo10 */(_qc/* sgLY */, _qh/* sgM5 */)));});
        break;
      case 2:
        return new F(function(){return _eD/* Data.Map.Base.balanceR */(_bp/* LudoJS.Red */, _qf/* sgM3 */, _qg/* sgM4 */, B(_qb/* LudoJS.$sinsert_$s$sgo10 */(_qc/* sgLY */, _qh/* sgM5 */)));});
        break;
      default:
        return new T5(0,_qe/* sgM0 */.a,E(_bq/* LudoJS.Yellow */),_qc/* sgLY */,E(_qg/* sgM4 */),E(_qh/* sgM5 */));
    }
  }else{
    return new T5(0,1,E(_bq/* LudoJS.Yellow */),_qc/* sgLY */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
  }
},
_qi/* $sinsert_$s$sgo2 */ = function(_qj/* sgMm */, _qk/* sgMn */){
  var _ql/* sgMo */ = E(_qk/* sgMn */);
  if(!_ql/* sgMo */._){
    var _qm/* sgMr */ = _ql/* sgMo */.c,
    _qn/* sgMs */ = _ql/* sgMo */.d,
    _qo/* sgMt */ = _ql/* sgMo */.e;
    switch(E(_ql/* sgMo */.b)){
      case 0:
        return new T5(0,_ql/* sgMo */.a,E(_bn/* LudoJS.Blue */),_qj/* sgMm */,E(_qn/* sgMs */),E(_qo/* sgMt */));
      case 1:
        return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bo/* LudoJS.Green */, _qm/* sgMr */, B(_qi/* LudoJS.$sinsert_$s$sgo2 */(_qj/* sgMm */, _qn/* sgMs */)), _qo/* sgMt */);});
        break;
      case 2:
        return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bp/* LudoJS.Red */, _qm/* sgMr */, B(_qi/* LudoJS.$sinsert_$s$sgo2 */(_qj/* sgMm */, _qn/* sgMs */)), _qo/* sgMt */);});
        break;
      default:
        return new F(function(){return _dW/* Data.Map.Base.balanceL */(_bq/* LudoJS.Yellow */, _qm/* sgMr */, B(_qi/* LudoJS.$sinsert_$s$sgo2 */(_qj/* sgMm */, _qn/* sgMs */)), _qo/* sgMt */);});
    }
  }else{
    return new T5(0,1,E(_bn/* LudoJS.Blue */),_qj/* sgMm */,E(_dR/* Data.Map.Base.Tip */),E(_dR/* Data.Map.Base.Tip */));
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
_qy/* lvl11 */ = new T1(0,2),
_qz/* lvl12 */ = new T1(0,0),
_qA/* lvl13 */ = new T1(0,1),
_qB/* outByCell */ = function(_qC/* sgTO */, _qD/* sgTP */){
  var _qE/* sgTQ */ = E(_qC/* sgTO */);
  if(!_qE/* sgTQ */._){
    return __Z/* EXTERNAL */;
  }else{
    var _qF/* sgTS */ = _qE/* sgTQ */.b,
    _qG/* sgTT */ = E(_qE/* sgTQ */.a),
    _qH/* sgTW */ = E(_qG/* sgTT */.b);
    if(!_qH/* sgTW */._){
      return new T2(1,_qG/* sgTT */,new T(function(){
        return B(_qB/* LudoJS.outByCell */(_qF/* sgTS */, _qD/* sgTP */));
      }));
    }else{
      var _qI/* sgTZ */ = E(_qD/* sgTP */);
      return (_qI/* sgTZ */!=E(_qH/* sgTW */.a)) ? new T2(1,_qG/* sgTT */,new T(function(){
        return B(_pr/* LudoJS.$soutByCell */(_qF/* sgTS */, _qI/* sgTZ */));
      })) : new T2(1,new T2(0,_qG/* sgTT */.a,_hO/* LudoJS.Out */),new T(function(){
        return B(_pr/* LudoJS.$soutByCell */(_qF/* sgTS */, _qI/* sgTZ */));
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
_qS/* a43 */ = function(_qT/* sgWq */, _qU/* sgWr */, _qV/* sgWs */, _/* EXTERNAL */){
  var _qW/* sgWu */ = function(_qX/* sgWv */){
    var _qY/* sgWw */ = E(_qX/* sgWv */);
    if(!_qY/* sgWw */._){
      return E(_qz/* LudoJS.lvl12 */);
    }else{
      var _qZ/* sgWy */ = _qY/* sgWw */.b,
      _r0/* sgWz */ = E(_qU/* sgWr */);
      if(_r0/* sgWz */!=E(_qY/* sgWw */.a)){
        var _r1/* sgWF */ = function(_r2/* sgWG */){
          while(1){
            var _r3/* sgWH */ = E(_r2/* sgWG */);
            if(!_r3/* sgWH */._){
              return E(_qz/* LudoJS.lvl12 */);
            }else{
              var _r4/* sgWJ */ = _r3/* sgWH */.b;
              if(_r0/* sgWz */!=E(_r3/* sgWH */.a)){
                _r2/* sgWG */ = _r4/* sgWJ */;
                continue;
              }else{
                return new F(function(){return _qJ/* GHC.Integer.Type.plusInteger */(B(_r1/* sgWF */(_r4/* sgWJ */)), _qA/* LudoJS.lvl13 */);});
              }
            }
          }
        };
        return new F(function(){return _r1/* sgWF */(_qZ/* sgWy */);});
      }else{
        var _r5/* sgWP */ = function(_r6/* sgWQ */){
          while(1){
            var _r7/* sgWR */ = E(_r6/* sgWQ */);
            if(!_r7/* sgWR */._){
              return E(_qz/* LudoJS.lvl12 */);
            }else{
              var _r8/* sgWT */ = _r7/* sgWR */.b;
              if(_r0/* sgWz */!=E(_r7/* sgWR */.a)){
                _r6/* sgWQ */ = _r8/* sgWT */;
                continue;
              }else{
                return new F(function(){return _qJ/* GHC.Integer.Type.plusInteger */(B(_r5/* sgWP */(_r8/* sgWT */)), _qA/* LudoJS.lvl13 */);});
              }
            }
          }
        };
        return new F(function(){return _qJ/* GHC.Integer.Type.plusInteger */(B(_r5/* sgWP */(_qZ/* sgWy */)), _qA/* LudoJS.lvl13 */);});
      }
    }
  },
  _r9/* sgX0 */ = function(_ra/* sgX1 */, _/* EXTERNAL */){
    var _rb/* sgXr */ = new T(function(){
      var _rc/* sgX9 */ = function(_rd/*  sgXa */){
        while(1){
          var _re/*  sgX9 */ = B((function(_rf/* sgXa */){
            var _rg/* sgXb */ = E(_rf/* sgXa */);
            if(!_rg/* sgXb */._){
              return __Z/* EXTERNAL */;
            }else{
              var _rh/* sgXd */ = _rg/* sgXb */.b,
              _ri/* sgXh */ = E(E(_rg/* sgXb */.a).b);
              if(!_ri/* sgXh */._){
                _rd/*  sgXa */ = _rh/* sgXd */;
                return __continue/* EXTERNAL */;
              }else{
                return new T2(1,new T(function(){
                  return B(_1L/* LudoJS.$wconvertCell */(_bn/* LudoJS.Blue */, E(_ri/* sgXh */.a), _qT/* sgWq */));
                }),new T(function(){
                  return B(_rc/* sgX9 */(_rh/* sgXd */));
                }));
              }
            }
          })(_rd/*  sgXa */));
          if(_re/*  sgX9 */!=__continue/* EXTERNAL */){
            return _re/*  sgX9 */;
          }
        }
      };
      return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* sgWu */(B(_rc/* sgX9 */(B(_q0/* LudoJS.$s!_$spoly_go2 */(E(_ra/* sgX1 */).d)))))), _qy/* LudoJS.lvl11 */));
    });
    return new T2(0,_rb/* sgXr */,_ra/* sgX1 */);
  },
  _rj/* sgXt */ = function(_/* EXTERNAL */, _rk/* sgXv */){
    var _rl/* sgXw */ = E(_rk/* sgXv */),
    _rm/* sgXy */ = _rl/* sgXw */.b;
    if(!E(_rl/* sgXw */.a)){
      var _rn/* sgXA */ = function(_/* EXTERNAL */, _ro/* sgXC */, _rp/* sgXD */){
        var _rq/* sgXE */ = function(_/* EXTERNAL */, _rr/* sgXG */, _rs/* sgXH */){
          var _rt/* sgXI */ = function(_/* EXTERNAL */, _ru/* sgXK */){
            var _rv/* sgXL */ = E(_qT/* sgWq */);
            if(_rv/* sgXL */==2){
              return new T2(0,_2s/* GHC.Tuple.() */,new T(function(){
                return E(E(_ru/* sgXK */).b);
              }));
            }else{
              var _rw/* sgYk */ = new T(function(){
                var _rx/* sgXP */ = E(E(_ru/* sgXK */).b),
                _ry/* sgXT */ = _rx/* sgXP */.d,
                _rz/* sgYj */ = new T(function(){
                  var _rA/* sgYi */ = new T(function(){
                    return B(_qB/* LudoJS.outByCell */(B(_bt/* LudoJS.$s!1 */(_bp/* LudoJS.Red */, _ry/* sgXT */)), new T(function(){
                      var _rB/* sgXW */ = E(_qU/* sgWr */);
                      switch(E(_rv/* sgXL */)){
                        case 0:
                          return B(_1E/* GHC.Classes.modInt# */(_rB/* sgXW */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52));
                          break;
                        case 1:
                          return B(_1E/* GHC.Classes.modInt# */(_rB/* sgXW */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52));
                          break;
                        default:
                          return B(_1E/* GHC.Classes.modInt# */(_rB/* sgXW */+(imul/* EXTERNAL */(13, 4-(4-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52));
                      }
                    },1)));
                  });
                  return B(_fi/* LudoJS.$sinsert_$sgo10 */(_bp/* LudoJS.Red */, _rA/* sgYi */, _ry/* sgXT */));
                });
                return new T5(0,_rx/* sgXP */.a,_rx/* sgXP */.b,_rx/* sgXP */.c,_rz/* sgYj */,_rx/* sgXP */.e);
              });
              return new T2(0,_2s/* GHC.Tuple.() */,_rw/* sgYk */);
            }
          },
          _rC/* sgYr */ = E(_qT/* sgWq */);
          if(_rC/* sgYr */==3){
            return new F(function(){return _rt/* sgXI */(_/* EXTERNAL */, new T2(0,_2s/* GHC.Tuple.() */,_rs/* sgXH */));});
          }else{
            var _rD/* sgYX */ = new T(function(){
              var _rE/* sgYs */ = E(_rs/* sgXH */),
              _rF/* sgYw */ = _rE/* sgYs */.d,
              _rG/* sgYW */ = new T(function(){
                var _rH/* sgYV */ = new T(function(){
                  return B(_qB/* LudoJS.outByCell */(B(_pX/* LudoJS.$s!_$spoly_go10 */(_rF/* sgYw */)), new T(function(){
                    var _rI/* sgYz */ = E(_qU/* sgWr */);
                    switch(E(_rC/* sgYr */)){
                      case 0:
                        return B(_1E/* GHC.Classes.modInt# */(_rI/* sgYz */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52));
                        break;
                      case 1:
                        return B(_1E/* GHC.Classes.modInt# */(_rI/* sgYz */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52));
                        break;
                      default:
                        return B(_1E/* GHC.Classes.modInt# */(_rI/* sgYz */+(imul/* EXTERNAL */(13, 4-(3-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52));
                    }
                  },1)));
                });
                return B(_qb/* LudoJS.$sinsert_$s$sgo10 */(_rH/* sgYV */, _rF/* sgYw */));
              });
              return new T5(0,_rE/* sgYs */.a,_rE/* sgYs */.b,_rE/* sgYs */.c,_rG/* sgYW */,_rE/* sgYs */.e);
            });
            return new F(function(){return _rt/* sgXI */(_/* EXTERNAL */, new T2(0,_2s/* GHC.Tuple.() */,_rD/* sgYX */));});
          }
        },
        _rJ/* sgZ0 */ = E(_qT/* sgWq */);
        if(_rJ/* sgZ0 */==1){
          return new F(function(){return _rq/* sgXE */(_/* EXTERNAL */, _2s/* GHC.Tuple.() */, _rp/* sgXD */);});
        }else{
          var _rK/* sgZw */ = new T(function(){
            var _rL/* sgZ1 */ = E(_rp/* sgXD */),
            _rM/* sgZ5 */ = _rL/* sgZ1 */.d,
            _rN/* sgZv */ = new T(function(){
              var _rO/* sgZu */ = new T(function(){
                return B(_qB/* LudoJS.outByCell */(B(_pT/* LudoJS.$s!_$spoly_go1 */(_rM/* sgZ5 */)), new T(function(){
                  var _rP/* sgZ8 */ = E(_qU/* sgWr */);
                  switch(E(_rJ/* sgZ0 */)){
                    case 0:
                      return B(_1E/* GHC.Classes.modInt# */(_rP/* sgZ8 */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(1, 4))|0)|0)|0)|0, 52));
                      break;
                    case 2:
                      return B(_1E/* GHC.Classes.modInt# */(_rP/* sgZ8 */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52));
                      break;
                    default:
                      return B(_1E/* GHC.Classes.modInt# */(_rP/* sgZ8 */+(imul/* EXTERNAL */(13, 4-(2-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52));
                  }
                },1)));
              });
              return B(_q4/* LudoJS.$sinsert_$s$sgo1 */(_rO/* sgZu */, _rM/* sgZ5 */));
            });
            return new T5(0,_rL/* sgZ1 */.a,_rL/* sgZ1 */.b,_rL/* sgZ1 */.c,_rN/* sgZv */,_rL/* sgZ1 */.e);
          },1);
          return new F(function(){return _rq/* sgXE */(_/* EXTERNAL */, _2s/* GHC.Tuple.() */, _rK/* sgZw */);});
        }
      },
      _rQ/* sgZx */ = E(_qT/* sgWq */);
      if(!_rQ/* sgZx */){
        return new F(function(){return _rn/* sgXA */(_/* EXTERNAL */, _2s/* GHC.Tuple.() */, _rm/* sgXy */);});
      }else{
        var _rR/* sh03 */ = new T(function(){
          var _rS/* sgZy */ = E(_rm/* sgXy */),
          _rT/* sgZC */ = _rS/* sgZy */.d,
          _rU/* sh02 */ = new T(function(){
            var _rV/* sh01 */ = new T(function(){
              return B(_qB/* LudoJS.outByCell */(B(_q0/* LudoJS.$s!_$spoly_go2 */(_rT/* sgZC */)), new T(function(){
                var _rW/* sgZF */ = E(_qU/* sgWr */);
                switch(E(_rQ/* sgZx */)){
                  case 1:
                    return B(_1E/* GHC.Classes.modInt# */(_rW/* sgZF */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(2, 4))|0)|0)|0)|0, 52));
                    break;
                  case 2:
                    return B(_1E/* GHC.Classes.modInt# */(_rW/* sgZF */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(4, 4))|0)|0)|0)|0, 52));
                    break;
                  default:
                    return B(_1E/* GHC.Classes.modInt# */(_rW/* sgZF */+(imul/* EXTERNAL */(13, 4-(1-B(_1E/* GHC.Classes.modInt# */(3, 4))|0)|0)|0)|0, 52));
                }
              },1)));
            });
            return B(_qi/* LudoJS.$sinsert_$s$sgo2 */(_rV/* sh01 */, _rT/* sgZC */));
          });
          return new T5(0,_rS/* sgZy */.a,_rS/* sgZy */.b,_rS/* sgZy */.c,_rU/* sh02 */,_rS/* sgZy */.e);
        },1);
        return new F(function(){return _rn/* sgXA */(_/* EXTERNAL */, _2s/* GHC.Tuple.() */, _rR/* sh03 */);});
      }
    }else{
      var _rX/* sh0d */ = new T(function(){
        var _rY/* sh04 */ = E(_rm/* sgXy */),
        _rZ/* sh08 */ = _rY/* sh04 */.d,
        _s0/* sh0c */ = new T(function(){
          return B(_fi/* LudoJS.$sinsert_$sgo10 */(_qT/* sgWq */, new T(function(){
            return B(_qB/* LudoJS.outByCell */(B(_bt/* LudoJS.$s!1 */(_qT/* sgWq */, _rZ/* sh08 */)), _qU/* sgWr */));
          }), _rZ/* sh08 */));
        });
        return new T5(0,_rY/* sh04 */.a,_rY/* sh04 */.b,_rY/* sh04 */.c,_s0/* sh0c */,_rY/* sh04 */.e);
      });
      return new T2(0,_2s/* GHC.Tuple.() */,_rX/* sh0d */);
    }
  };
  switch(E(_qT/* sgWq */)){
    case 0:
      var _s1/* sh0i */ = function(_s2/*  sh1k */, _s3/*  sh1l */, _s4/*  sh1m */, _/* EXTERNAL */){
        while(1){
          var _s5/*  sh0i */ = B((function(_s6/* sh1k */, _s7/* sh1l */, _s8/* sh1m */, _/* EXTERNAL */){
            var _s9/* sh1o */ = E(_s6/* sh1k */);
            if(!_s9/* sh1o */._){
              return new T2(0,_s7/* sh1l */,_s8/* sh1m */);
            }else{
              var _sa/* sh1r */ = _s9/* sh1o */.b,
              _sb/* sh1s */ = E(_s9/* sh1o */.a);
              if(!_sb/* sh1s */){
                var _sc/*   sh1l */ = _s7/* sh1l */,
                _sd/*   sh1m */ = _s8/* sh1m */;
                _s2/*  sh1k */ = _sa/* sh1r */;
                _s3/*  sh1l */ = _sc/*   sh1l */;
                _s4/*  sh1m */ = _sd/*   sh1m */;
                return __continue/* EXTERNAL */;
              }else{
                var _se/* sh1S */ = new T(function(){
                  if(!E(_s7/* sh1l */)){
                    var _sf/* sh1A */ = function(_sg/*  sh1B */){
                      while(1){
                        var _sh/*  sh1A */ = B((function(_si/* sh1B */){
                          var _sj/* sh1C */ = E(_si/* sh1B */);
                          if(!_sj/* sh1C */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _sk/* sh1E */ = _sj/* sh1C */.b,
                            _sl/* sh1I */ = E(E(_sj/* sh1C */.a).b);
                            if(!_sl/* sh1I */._){
                              _sg/*  sh1B */ = _sk/* sh1E */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_1L/* LudoJS.$wconvertCell */(_sb/* sh1s */, E(_sl/* sh1I */.a), _bn/* LudoJS.Blue */));
                              }),new T(function(){
                                return B(_sf/* sh1A */(_sk/* sh1E */));
                              }));
                            }
                          }
                        })(_sg/*  sh1B */));
                        if(_sh/*  sh1A */!=__continue/* EXTERNAL */){
                          return _sh/*  sh1A */;
                        }
                      }
                    };
                    return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* sgWu */(B(_sf/* sh1A */(B(_bt/* LudoJS.$s!1 */(_sb/* sh1s */, E(_s8/* sh1m */).d)))))), _qy/* LudoJS.lvl11 */));
                  }else{
                    return true;
                  }
                }),
                _sd/*   sh1m */ = _s8/* sh1m */;
                _s2/*  sh1k */ = _sa/* sh1r */;
                _s3/*  sh1l */ = _se/* sh1S */;
                _s4/*  sh1m */ = _sd/*   sh1m */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_s2/*  sh1k */, _s3/*  sh1l */, _s4/*  sh1m */, _/* EXTERNAL */));
          if(_s5/*  sh0i */!=__continue/* EXTERNAL */){
            return _s5/*  sh0i */;
          }
        }
      },
      _sm/* sh0g */ = function(_sn/*  sh0j */, _so/*  sh0k */, _/* EXTERNAL */){
        while(1){
          var _sp/*  sh0g */ = B((function(_sq/* sh0j */, _sr/* sh0k */, _/* EXTERNAL */){
            var _ss/* sh0m */ = E(_sq/* sh0j */);
            if(!_ss/* sh0m */._){
              return new T2(0,_qp/* GHC.Types.False */,_sr/* sh0k */);
            }else{
              var _st/* sh0p */ = _ss/* sh0m */.b,
              _su/* sh0q */ = E(_ss/* sh0m */.a);
              if(!_su/* sh0q */){
                var _sv/*   sh0k */ = _sr/* sh0k */;
                _sn/*  sh0j */ = _st/* sh0p */;
                _so/*  sh0k */ = _sv/*   sh0k */;
                return __continue/* EXTERNAL */;
              }else{
                var _sw/* sh0P */ = new T(function(){
                  var _sx/* sh0x */ = function(_sy/*  sh0y */){
                    while(1){
                      var _sz/*  sh0x */ = B((function(_sA/* sh0y */){
                        var _sB/* sh0z */ = E(_sA/* sh0y */);
                        if(!_sB/* sh0z */._){
                          return __Z/* EXTERNAL */;
                        }else{
                          var _sC/* sh0B */ = _sB/* sh0z */.b,
                          _sD/* sh0F */ = E(E(_sB/* sh0z */.a).b);
                          if(!_sD/* sh0F */._){
                            _sy/*  sh0y */ = _sC/* sh0B */;
                            return __continue/* EXTERNAL */;
                          }else{
                            return new T2(1,new T(function(){
                              return B(_1L/* LudoJS.$wconvertCell */(_su/* sh0q */, E(_sD/* sh0F */.a), _bn/* LudoJS.Blue */));
                            }),new T(function(){
                              return B(_sx/* sh0x */(_sC/* sh0B */));
                            }));
                          }
                        }
                      })(_sy/*  sh0y */));
                      if(_sz/*  sh0x */!=__continue/* EXTERNAL */){
                        return _sz/*  sh0x */;
                      }
                    }
                  };
                  return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* sgWu */(B(_sx/* sh0x */(B(_bt/* LudoJS.$s!1 */(_su/* sh0q */, E(_sr/* sh0k */).d)))))), _qy/* LudoJS.lvl11 */));
                });
                return new F(function(){return _s1/* sh0i */(_st/* sh0p */, _sw/* sh0P */, _sr/* sh0k */, _/* EXTERNAL */);});
              }
            }
          })(_sn/*  sh0j */, _so/*  sh0k */, _/* EXTERNAL */));
          if(_sp/*  sh0g */!=__continue/* EXTERNAL */){
            return _sp/*  sh0g */;
          }
        }
      },
      _sE/* sh0h */ = function(_sF/* sh0Q */, _sG/* sh0R */, _sH/* sh0S */, _/* EXTERNAL */){
        var _sI/* sh0U */ = E(_sF/* sh0Q */);
        if(!_sI/* sh0U */){
          return new F(function(){return _sm/* sh0g */(_sG/* sh0R */, _sH/* sh0S */, _/* EXTERNAL */);});
        }else{
          var _sJ/* sh1j */ = new T(function(){
            var _sK/* sh11 */ = function(_sL/*  sh12 */){
              while(1){
                var _sM/*  sh11 */ = B((function(_sN/* sh12 */){
                  var _sO/* sh13 */ = E(_sN/* sh12 */);
                  if(!_sO/* sh13 */._){
                    return __Z/* EXTERNAL */;
                  }else{
                    var _sP/* sh15 */ = _sO/* sh13 */.b,
                    _sQ/* sh19 */ = E(E(_sO/* sh13 */.a).b);
                    if(!_sQ/* sh19 */._){
                      _sL/*  sh12 */ = _sP/* sh15 */;
                      return __continue/* EXTERNAL */;
                    }else{
                      return new T2(1,new T(function(){
                        return B(_1L/* LudoJS.$wconvertCell */(_sI/* sh0U */, E(_sQ/* sh19 */.a), _bn/* LudoJS.Blue */));
                      }),new T(function(){
                        return B(_sK/* sh11 */(_sP/* sh15 */));
                      }));
                    }
                  }
                })(_sL/*  sh12 */));
                if(_sM/*  sh11 */!=__continue/* EXTERNAL */){
                  return _sM/*  sh11 */;
                }
              }
            };
            return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* sgWu */(B(_sK/* sh11 */(B(_bt/* LudoJS.$s!1 */(_sI/* sh0U */, E(_sH/* sh0S */).d)))))), _qy/* LudoJS.lvl11 */));
          });
          return new F(function(){return _s1/* sh0i */(_sG/* sh0R */, _sJ/* sh1j */, _sH/* sh0S */, _/* EXTERNAL */);});
        }
      },
      _sR/* sh1T */ = B(_sE/* sh0h */(_bo/* LudoJS.Green */, _hY/* LudoJS.lvl9 */, _qV/* sgWs */, _/* EXTERNAL */));
      return new F(function(){return _rj/* sgXt */(_/* EXTERNAL */, _sR/* sh1T */);});
      break;
    case 1:
      var _sS/* sh1W */ = B(_r9/* sgX0 */(_qV/* sgWs */, _/* EXTERNAL */)),
      _sT/* sh20 */ = function(_sU/*  sh25 */, _sV/*  sh26 */, _sW/*  sh27 */, _/* EXTERNAL */){
        while(1){
          var _sX/*  sh20 */ = B((function(_sY/* sh25 */, _sZ/* sh26 */, _t0/* sh27 */, _/* EXTERNAL */){
            var _t1/* sh29 */ = E(_sY/* sh25 */);
            if(!_t1/* sh29 */._){
              return new T2(0,_sZ/* sh26 */,_t0/* sh27 */);
            }else{
              var _t2/* sh2c */ = _t1/* sh29 */.b,
              _t3/* sh2d */ = E(_t1/* sh29 */.a);
              if(_t3/* sh2d */==1){
                var _t4/*   sh26 */ = _sZ/* sh26 */,
                _t5/*   sh27 */ = _t0/* sh27 */;
                _sU/*  sh25 */ = _t2/* sh2c */;
                _sV/*  sh26 */ = _t4/*   sh26 */;
                _sW/*  sh27 */ = _t5/*   sh27 */;
                return __continue/* EXTERNAL */;
              }else{
                var _t6/* sh2D */ = new T(function(){
                  if(!E(_sZ/* sh26 */)){
                    var _t7/* sh2l */ = function(_t8/*  sh2m */){
                      while(1){
                        var _t9/*  sh2l */ = B((function(_ta/* sh2m */){
                          var _tb/* sh2n */ = E(_ta/* sh2m */);
                          if(!_tb/* sh2n */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _tc/* sh2p */ = _tb/* sh2n */.b,
                            _td/* sh2t */ = E(E(_tb/* sh2n */.a).b);
                            if(!_td/* sh2t */._){
                              _t8/*  sh2m */ = _tc/* sh2p */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_1L/* LudoJS.$wconvertCell */(_t3/* sh2d */, E(_td/* sh2t */.a), _bo/* LudoJS.Green */));
                              }),new T(function(){
                                return B(_t7/* sh2l */(_tc/* sh2p */));
                              }));
                            }
                          }
                        })(_t8/*  sh2m */));
                        if(_t9/*  sh2l */!=__continue/* EXTERNAL */){
                          return _t9/*  sh2l */;
                        }
                      }
                    };
                    return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* sgWu */(B(_t7/* sh2l */(B(_bt/* LudoJS.$s!1 */(_t3/* sh2d */, E(_t0/* sh27 */).d)))))), _qy/* LudoJS.lvl11 */));
                  }else{
                    return true;
                  }
                }),
                _t5/*   sh27 */ = _t0/* sh27 */;
                _sU/*  sh25 */ = _t2/* sh2c */;
                _sV/*  sh26 */ = _t6/* sh2D */;
                _sW/*  sh27 */ = _t5/*   sh27 */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_sU/*  sh25 */, _sV/*  sh26 */, _sW/*  sh27 */, _/* EXTERNAL */));
          if(_sX/*  sh20 */!=__continue/* EXTERNAL */){
            return _sX/*  sh20 */;
          }
        }
      },
      _te/* sh2M */ = B((function(_tf/* sh21 */, _tg/* sh22 */, _th/* sh23 */, _/* EXTERNAL */){
        return new F(function(){return _sT/* sh20 */(_tf/* sh21 */, _tg/* sh22 */, _th/* sh23 */, _/* EXTERNAL */);});
      })(_hY/* LudoJS.lvl9 */, new T(function(){
        return E(E(_sS/* sh1W */).a);
      }), new T(function(){
        return E(E(_sS/* sh1W */).b);
      }), _/* EXTERNAL */));
      return new F(function(){return _rj/* sgXt */(_/* EXTERNAL */, _te/* sh2M */);});
      break;
    case 2:
      var _ti/* sh2P */ = B(_r9/* sgX0 */(_qV/* sgWs */, _/* EXTERNAL */)),
      _tj/* sh2T */ = function(_tk/*  sh3q */, _tl/*  sh3r */, _tm/*  sh3s */, _/* EXTERNAL */){
        while(1){
          var _tn/*  sh2T */ = B((function(_to/* sh3q */, _tp/* sh3r */, _tq/* sh3s */, _/* EXTERNAL */){
            var _tr/* sh3u */ = E(_to/* sh3q */);
            if(!_tr/* sh3u */._){
              return new T2(0,_tp/* sh3r */,_tq/* sh3s */);
            }else{
              var _ts/* sh3x */ = _tr/* sh3u */.b,
              _tt/* sh3y */ = E(_tr/* sh3u */.a);
              if(_tt/* sh3y */==2){
                var _tu/*   sh3r */ = _tp/* sh3r */,
                _tv/*   sh3s */ = _tq/* sh3s */;
                _tk/*  sh3q */ = _ts/* sh3x */;
                _tl/*  sh3r */ = _tu/*   sh3r */;
                _tm/*  sh3s */ = _tv/*   sh3s */;
                return __continue/* EXTERNAL */;
              }else{
                var _tw/* sh3Y */ = new T(function(){
                  if(!E(_tp/* sh3r */)){
                    var _tx/* sh3G */ = function(_ty/*  sh3H */){
                      while(1){
                        var _tz/*  sh3G */ = B((function(_tA/* sh3H */){
                          var _tB/* sh3I */ = E(_tA/* sh3H */);
                          if(!_tB/* sh3I */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _tC/* sh3K */ = _tB/* sh3I */.b,
                            _tD/* sh3O */ = E(E(_tB/* sh3I */.a).b);
                            if(!_tD/* sh3O */._){
                              _ty/*  sh3H */ = _tC/* sh3K */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_1L/* LudoJS.$wconvertCell */(_tt/* sh3y */, E(_tD/* sh3O */.a), _bp/* LudoJS.Red */));
                              }),new T(function(){
                                return B(_tx/* sh3G */(_tC/* sh3K */));
                              }));
                            }
                          }
                        })(_ty/*  sh3H */));
                        if(_tz/*  sh3G */!=__continue/* EXTERNAL */){
                          return _tz/*  sh3G */;
                        }
                      }
                    };
                    return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* sgWu */(B(_tx/* sh3G */(B(_bt/* LudoJS.$s!1 */(_tt/* sh3y */, E(_tq/* sh3s */).d)))))), _qy/* LudoJS.lvl11 */));
                  }else{
                    return true;
                  }
                }),
                _tv/*   sh3s */ = _tq/* sh3s */;
                _tk/*  sh3q */ = _ts/* sh3x */;
                _tl/*  sh3r */ = _tw/* sh3Y */;
                _tm/*  sh3s */ = _tv/*   sh3s */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_tk/*  sh3q */, _tl/*  sh3r */, _tm/*  sh3s */, _/* EXTERNAL */));
          if(_tn/*  sh2T */!=__continue/* EXTERNAL */){
            return _tn/*  sh2T */;
          }
        }
      },
      _tE/* sh2S */ = function(_tF/* sh2U */, _tG/* sh2V */, _tH/* sh2W */, _tI/* sh2X */, _/* EXTERNAL */){
        var _tJ/* sh2Z */ = E(_tF/* sh2U */);
        if(_tJ/* sh2Z */==2){
          return new F(function(){return _tj/* sh2T */(_tG/* sh2V */, _tH/* sh2W */, _tI/* sh2X */, _/* EXTERNAL */);});
        }else{
          var _tK/* sh3p */ = new T(function(){
            if(!E(_tH/* sh2W */)){
              var _tL/* sh37 */ = function(_tM/*  sh38 */){
                while(1){
                  var _tN/*  sh37 */ = B((function(_tO/* sh38 */){
                    var _tP/* sh39 */ = E(_tO/* sh38 */);
                    if(!_tP/* sh39 */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _tQ/* sh3b */ = _tP/* sh39 */.b,
                      _tR/* sh3f */ = E(E(_tP/* sh39 */.a).b);
                      if(!_tR/* sh3f */._){
                        _tM/*  sh38 */ = _tQ/* sh3b */;
                        return __continue/* EXTERNAL */;
                      }else{
                        return new T2(1,new T(function(){
                          return B(_1L/* LudoJS.$wconvertCell */(_tJ/* sh2Z */, E(_tR/* sh3f */.a), _bp/* LudoJS.Red */));
                        }),new T(function(){
                          return B(_tL/* sh37 */(_tQ/* sh3b */));
                        }));
                      }
                    }
                  })(_tM/*  sh38 */));
                  if(_tN/*  sh37 */!=__continue/* EXTERNAL */){
                    return _tN/*  sh37 */;
                  }
                }
              };
              return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* sgWu */(B(_tL/* sh37 */(B(_bt/* LudoJS.$s!1 */(_tJ/* sh2Z */, E(_tI/* sh2X */).d)))))), _qy/* LudoJS.lvl11 */));
            }else{
              return true;
            }
          });
          return new F(function(){return _tj/* sh2T */(_tG/* sh2V */, _tK/* sh3p */, _tI/* sh2X */, _/* EXTERNAL */);});
        }
      },
      _tS/* sh47 */ = B(_tE/* sh2S */(_bo/* LudoJS.Green */, _hY/* LudoJS.lvl9 */, new T(function(){
        return E(E(_ti/* sh2P */).a);
      }), new T(function(){
        return E(E(_ti/* sh2P */).b);
      }), _/* EXTERNAL */));
      return new F(function(){return _rj/* sgXt */(_/* EXTERNAL */, _tS/* sh47 */);});
      break;
    default:
      var _tT/* sh4a */ = B(_r9/* sgX0 */(_qV/* sgWs */, _/* EXTERNAL */)),
      _tU/* sh4e */ = function(_tV/*  sh4L */, _tW/*  sh4M */, _tX/*  sh4N */, _/* EXTERNAL */){
        while(1){
          var _tY/*  sh4e */ = B((function(_tZ/* sh4L */, _u0/* sh4M */, _u1/* sh4N */, _/* EXTERNAL */){
            var _u2/* sh4P */ = E(_tZ/* sh4L */);
            if(!_u2/* sh4P */._){
              return new T2(0,_u0/* sh4M */,_u1/* sh4N */);
            }else{
              var _u3/* sh4S */ = _u2/* sh4P */.b,
              _u4/* sh4T */ = E(_u2/* sh4P */.a);
              if(_u4/* sh4T */==3){
                var _u5/*   sh4M */ = _u0/* sh4M */,
                _u6/*   sh4N */ = _u1/* sh4N */;
                _tV/*  sh4L */ = _u3/* sh4S */;
                _tW/*  sh4M */ = _u5/*   sh4M */;
                _tX/*  sh4N */ = _u6/*   sh4N */;
                return __continue/* EXTERNAL */;
              }else{
                var _u7/* sh5j */ = new T(function(){
                  if(!E(_u0/* sh4M */)){
                    var _u8/* sh51 */ = function(_u9/*  sh52 */){
                      while(1){
                        var _ua/*  sh51 */ = B((function(_ub/* sh52 */){
                          var _uc/* sh53 */ = E(_ub/* sh52 */);
                          if(!_uc/* sh53 */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _ud/* sh55 */ = _uc/* sh53 */.b,
                            _ue/* sh59 */ = E(E(_uc/* sh53 */.a).b);
                            if(!_ue/* sh59 */._){
                              _u9/*  sh52 */ = _ud/* sh55 */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,new T(function(){
                                return B(_1L/* LudoJS.$wconvertCell */(_u4/* sh4T */, E(_ue/* sh59 */.a), _bq/* LudoJS.Yellow */));
                              }),new T(function(){
                                return B(_u8/* sh51 */(_ud/* sh55 */));
                              }));
                            }
                          }
                        })(_u9/*  sh52 */));
                        if(_ua/*  sh51 */!=__continue/* EXTERNAL */){
                          return _ua/*  sh51 */;
                        }
                      }
                    };
                    return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* sgWu */(B(_u8/* sh51 */(B(_bt/* LudoJS.$s!1 */(_u4/* sh4T */, E(_u1/* sh4N */).d)))))), _qy/* LudoJS.lvl11 */));
                  }else{
                    return true;
                  }
                }),
                _u6/*   sh4N */ = _u1/* sh4N */;
                _tV/*  sh4L */ = _u3/* sh4S */;
                _tW/*  sh4M */ = _u7/* sh5j */;
                _tX/*  sh4N */ = _u6/*   sh4N */;
                return __continue/* EXTERNAL */;
              }
            }
          })(_tV/*  sh4L */, _tW/*  sh4M */, _tX/*  sh4N */, _/* EXTERNAL */));
          if(_tY/*  sh4e */!=__continue/* EXTERNAL */){
            return _tY/*  sh4e */;
          }
        }
      },
      _uf/* sh4d */ = function(_ug/* sh4f */, _uh/* sh4g */, _ui/* sh4h */, _uj/* sh4i */, _/* EXTERNAL */){
        var _uk/* sh4k */ = E(_ug/* sh4f */);
        if(_uk/* sh4k */==3){
          return new F(function(){return _tU/* sh4e */(_uh/* sh4g */, _ui/* sh4h */, _uj/* sh4i */, _/* EXTERNAL */);});
        }else{
          var _ul/* sh4K */ = new T(function(){
            if(!E(_ui/* sh4h */)){
              var _um/* sh4s */ = function(_un/*  sh4t */){
                while(1){
                  var _uo/*  sh4s */ = B((function(_up/* sh4t */){
                    var _uq/* sh4u */ = E(_up/* sh4t */);
                    if(!_uq/* sh4u */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _ur/* sh4w */ = _uq/* sh4u */.b,
                      _us/* sh4A */ = E(E(_uq/* sh4u */.a).b);
                      if(!_us/* sh4A */._){
                        _un/*  sh4t */ = _ur/* sh4w */;
                        return __continue/* EXTERNAL */;
                      }else{
                        return new T2(1,new T(function(){
                          return B(_1L/* LudoJS.$wconvertCell */(_uk/* sh4k */, E(_us/* sh4A */.a), _bq/* LudoJS.Yellow */));
                        }),new T(function(){
                          return B(_um/* sh4s */(_ur/* sh4w */));
                        }));
                      }
                    }
                  })(_un/*  sh4t */));
                  if(_uo/*  sh4s */!=__continue/* EXTERNAL */){
                    return _uo/*  sh4s */;
                  }
                }
              };
              return B(_qq/* GHC.Integer.Type.geInteger */(B(_qW/* sgWu */(B(_um/* sh4s */(B(_bt/* LudoJS.$s!1 */(_uk/* sh4k */, E(_uj/* sh4i */).d)))))), _qy/* LudoJS.lvl11 */));
            }else{
              return true;
            }
          });
          return new F(function(){return _tU/* sh4e */(_uh/* sh4g */, _ul/* sh4K */, _uj/* sh4i */, _/* EXTERNAL */);});
        }
      },
      _ut/* sh5s */ = B(_uf/* sh4d */(_bo/* LudoJS.Green */, _hY/* LudoJS.lvl9 */, new T(function(){
        return E(E(_tT/* sh4a */).a);
      }), new T(function(){
        return E(E(_tT/* sh4a */).b);
      }), _/* EXTERNAL */));
      return new F(function(){return _rj/* sgXt */(_/* EXTERNAL */, _ut/* sh5s */);});
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
  return B(_bB/* GHC.List.$wlenAcc */(_vl/* LudoJS.starCells */, 0));
}),
_vo/* $wa6 */ = function(_vp/* sh5v */, _vq/* sh5w */, _vr/* sh5x */, _/* EXTERNAL */){
  var _vs/* sh5z */ = new T(function(){
    return E(E(_vr/* sh5x */).b);
  }),
  _vt/* sh5G */ = new T(function(){
    return E(E(_vr/* sh5x */).d);
  }),
  _vu/* sh5N */ = E(_vq/* sh5w */);
  if(_vu/* sh5N */==56){
    var _vv/* sha9 */ = new T(function(){
      var _vw/* sh9O */ = E(_vr/* sh5x */),
      _vx/* sha8 */ = new T(function(){
        var _vy/* sha7 */ = new T(function(){
          var _vz/* sh9U */ = B(_bt/* LudoJS.$s!1 */(_vs/* sh5z */, _vt/* sh5G */));
          if(!_vz/* sh9U */._){
            return __Z/* EXTERNAL */;
          }else{
            var _vA/* sh9W */ = _vz/* sh9U */.b,
            _vB/* sh9X */ = E(_vz/* sh9U */.a),
            _vC/* sha0 */ = E(_vp/* sh5v */);
            if(_vC/* sha0 */!=E(_vB/* sh9X */.a)){
              return new T2(1,_vB/* sh9X */,new T(function(){
                return B(_py/* LudoJS.$sremoveFrom */(_vA/* sh9W */, _vC/* sha0 */));
              }));
            }else{
              return E(_vA/* sh9W */);
            }
          }
        });
        return B(_fi/* LudoJS.$sinsert_$sgo10 */(_vs/* sh5z */, _vy/* sha7 */, _vt/* sh5G */));
      });
      return new T5(0,_vw/* sh9O */.a,_vw/* sh9O */.b,_vw/* sh9O */.c,_vx/* sha8 */,_vw/* sh9O */.e);
    });
    return new T2(0,_2s/* GHC.Tuple.() */,_vv/* sha9 */);
  }else{
    if(!B(_uw/* GHC.List.elem */(_pq/* GHC.Classes.$fEqInt */, _vu/* sh5N */, _vl/* LudoJS.starCells */))){
      if(!B(_uw/* GHC.List.elem */(_pq/* GHC.Classes.$fEqInt */, _vu/* sh5N */, _v3/* LudoJS.globeCells */))){
        if(_vu/* sh5N */<51){
          var _vD/* sh6h */ = new T(function(){
            var _vE/* sh5T */ = E(_vr/* sh5x */),
            _vF/* sh6g */ = new T(function(){
              var _vG/* sh6e */ = new T(function(){
                var _vH/* sh61 */ = B(_bt/* LudoJS.$s!1 */(_vs/* sh5z */, _vt/* sh5G */));
                if(!_vH/* sh61 */._){
                  return __Z/* EXTERNAL */;
                }else{
                  var _vI/* sh63 */ = _vH/* sh61 */.b,
                  _vJ/* sh64 */ = E(_vH/* sh61 */.a),
                  _vK/* sh67 */ = E(_vp/* sh5v */);
                  if(_vK/* sh67 */!=E(_vJ/* sh64 */.a)){
                    return new T2(1,_vJ/* sh64 */,new T(function(){
                      return B(_py/* LudoJS.$sremoveFrom */(_vI/* sh63 */, _vK/* sh67 */));
                    }));
                  }else{
                    return E(_vI/* sh63 */);
                  }
                }
              });
              return B(_fi/* LudoJS.$sinsert_$sgo10 */(_vs/* sh5z */, new T2(1,new T2(0,_vp/* sh5v */,new T1(1,_vu/* sh5N */)),_vG/* sh6e */), _vt/* sh5G */));
            });
            return new T5(0,_vE/* sh5T */.a,_vE/* sh5T */.b,_vE/* sh5T */.c,_vF/* sh6g */,_vE/* sh5T */.e);
          });
          return new F(function(){return _qS/* LudoJS.a43 */(_vs/* sh5z */, _vu/* sh5N */, _vD/* sh6h */, _/* EXTERNAL */);});
        }else{
          var _vL/* sh6G */ = new T(function(){
            var _vM/* sh6i */ = E(_vr/* sh5x */),
            _vN/* sh6F */ = new T(function(){
              var _vO/* sh6D */ = new T(function(){
                var _vP/* sh6q */ = B(_bt/* LudoJS.$s!1 */(_vs/* sh5z */, _vt/* sh5G */));
                if(!_vP/* sh6q */._){
                  return __Z/* EXTERNAL */;
                }else{
                  var _vQ/* sh6s */ = _vP/* sh6q */.b,
                  _vR/* sh6t */ = E(_vP/* sh6q */.a),
                  _vS/* sh6w */ = E(_vp/* sh5v */);
                  if(_vS/* sh6w */!=E(_vR/* sh6t */.a)){
                    return new T2(1,_vR/* sh6t */,new T(function(){
                      return B(_py/* LudoJS.$sremoveFrom */(_vQ/* sh6s */, _vS/* sh6w */));
                    }));
                  }else{
                    return E(_vQ/* sh6s */);
                  }
                }
              });
              return B(_fi/* LudoJS.$sinsert_$sgo10 */(_vs/* sh5z */, new T2(1,new T2(0,_vp/* sh5v */,new T1(1,_vu/* sh5N */)),_vO/* sh6D */), _vt/* sh5G */));
            });
            return new T5(0,_vM/* sh6i */.a,_vM/* sh6i */.b,_vM/* sh6i */.c,_vN/* sh6F */,_vM/* sh6i */.e);
          });
          return new T2(0,_2s/* GHC.Tuple.() */,_vL/* sh6G */);
        }
      }else{
        var _vT/* sh6I */ = E(_vr/* sh5x */),
        _vU/* sh6J */ = _vT/* sh6I */.a,
        _vV/* sh6K */ = _vT/* sh6I */.b,
        _vW/* sh6L */ = _vT/* sh6I */.c,
        _vX/* sh6N */ = _vT/* sh6I */.e,
        _vY/* sh6O */ = function(_vZ/* sh6P */, _w0/* sh6Q */, _w1/* sh6R */, _w2/* sh6S */, _w3/* sh6T */, _w4/* sh6U */, _/* EXTERNAL */){
          var _w5/* sh6W */ = new T(function(){
            return B(_1L/* LudoJS.$wconvertCell */(_w1/* sh6R */, _vu/* sh5N */, _vZ/* sh6P */));
          }),
          _w6/* sh6Y */ = function(_w7/*  sh6Z */){
            while(1){
              var _w8/*  sh6Y */ = B((function(_w9/* sh6Z */){
                var _wa/* sh70 */ = E(_w9/* sh6Z */);
                if(!_wa/* sh70 */._){
                  return false;
                }else{
                  var _wb/* sh72 */ = _wa/* sh70 */.b,
                  _wc/* sh76 */ = E(E(_wa/* sh70 */.a).b);
                  if(!_wc/* sh76 */._){
                    _w7/*  sh6Z */ = _wb/* sh72 */;
                    return __continue/* EXTERNAL */;
                  }else{
                    var _wd/* sh78 */ = E(_w5/* sh6W */);
                    if(_wd/* sh78 */!=E(_wc/* sh76 */.a)){
                      var _we/* sh7e */ = function(_wf/* sh7f */){
                        while(1){
                          var _wg/* sh7g */ = E(_wf/* sh7f */);
                          if(!_wg/* sh7g */._){
                            return false;
                          }else{
                            var _wh/* sh7i */ = _wg/* sh7g */.b,
                            _wi/* sh7m */ = E(E(_wg/* sh7g */.a).b);
                            if(!_wi/* sh7m */._){
                              _wf/* sh7f */ = _wh/* sh7i */;
                              continue;
                            }else{
                              if(_wd/* sh78 */!=E(_wi/* sh7m */.a)){
                                _wf/* sh7f */ = _wh/* sh7i */;
                                continue;
                              }else{
                                return true;
                              }
                            }
                          }
                        }
                      };
                      return new F(function(){return _we/* sh7e */(_wb/* sh72 */);});
                    }else{
                      return true;
                    }
                  }
                }
              })(_w7/*  sh6Z */));
              if(_w8/*  sh6Y */!=__continue/* EXTERNAL */){
                return _w8/*  sh6Y */;
              }
            }
          };
          if(!B(_w6/* sh6Y */(B(_bt/* LudoJS.$s!1 */(_vZ/* sh6P */, _w3/* sh6T */))))){
            return new T2(0,_2s/* GHC.Tuple.() */,new T5(0,_w0/* sh6Q */,_w1/* sh6R */,_w2/* sh6S */,_w3/* sh6T */,_w4/* sh6U */));
          }else{
            var _wj/* sh7y */ = new T(function(){
              return B(_fi/* LudoJS.$sinsert_$sgo10 */(_w1/* sh6R */, new T(function(){
                return B(_pr/* LudoJS.$soutByCell */(B(_bt/* LudoJS.$s!1 */(_w1/* sh6R */, _w3/* sh6T */)), _vu/* sh5N */));
              }), _w3/* sh6T */));
            });
            return new T2(0,_2s/* GHC.Tuple.() */,new T5(0,_w0/* sh6Q */,_w1/* sh6R */,_w2/* sh6S */,_wj/* sh7y */,_w4/* sh6U */));
          }
        },
        _wk/* sh7B */ = function(_wl/* sh7C */, _wm/* sh7D */, _wn/* sh7E */, _wo/* sh7F */, _wp/* sh7G */, _/* EXTERNAL */){
          var _wq/* sh7I */ = function(_wr/* sh7J */, _ws/* sh7K */, _wt/* sh7L */, _wu/* sh7M */, _wv/* sh7N */, _/* EXTERNAL */){
            var _ww/* sh7P */ = E(_vs/* sh5z */);
            if(_ww/* sh7P */==3){
              return new F(function(){return _vY/* sh6O */(_bp/* LudoJS.Red */, _wr/* sh7J */, _ws/* sh7K */, _wt/* sh7L */, _wu/* sh7M */, _wv/* sh7N */, _/* EXTERNAL */);});
            }else{
              var _wx/* sh7Q */ = B(_vY/* sh6O */(_bq/* LudoJS.Yellow */, _wr/* sh7J */, _ws/* sh7K */, _wt/* sh7L */, _wu/* sh7M */, _wv/* sh7N */, _/* EXTERNAL */));
              if(E(_ww/* sh7P */)==2){
                return new T2(0,_2s/* GHC.Tuple.() */,new T(function(){
                  return E(E(_wx/* sh7Q */).b);
                }));
              }else{
                var _wy/* sh7X */ = E(E(_wx/* sh7Q */).b);
                return new F(function(){return _vY/* sh6O */(_bp/* LudoJS.Red */, _wy/* sh7X */.a, _wy/* sh7X */.b, _wy/* sh7X */.c, _wy/* sh7X */.d, _wy/* sh7X */.e, _/* EXTERNAL */);});
              }
            }
          };
          if(E(_vs/* sh5z */)==1){
            return new F(function(){return _wq/* sh7I */(_wl/* sh7C */, _wm/* sh7D */, _wn/* sh7E */, _wo/* sh7F */, _wp/* sh7G */, _/* EXTERNAL */);});
          }else{
            var _wz/* sh89 */ = B(_vY/* sh6O */(_bo/* LudoJS.Green */, _wl/* sh7C */, _wm/* sh7D */, _wn/* sh7E */, _wo/* sh7F */, _wp/* sh7G */, _/* EXTERNAL */)),
            _wA/* sh8f */ = E(E(_wz/* sh89 */).b);
            return new F(function(){return _wq/* sh7I */(_wA/* sh8f */.a, _wA/* sh8f */.b, _wA/* sh8f */.c, _wA/* sh8f */.d, _wA/* sh8f */.e, _/* EXTERNAL */);});
          }
        },
        _wB/* sh8l */ = new T(function(){
          var _wC/* sh8B */ = new T(function(){
            var _wD/* sh8o */ = B(_bt/* LudoJS.$s!1 */(_vs/* sh5z */, _vt/* sh5G */));
            if(!_wD/* sh8o */._){
              return __Z/* EXTERNAL */;
            }else{
              var _wE/* sh8q */ = _wD/* sh8o */.b,
              _wF/* sh8r */ = E(_wD/* sh8o */.a),
              _wG/* sh8u */ = E(_vp/* sh5v */);
              if(_wG/* sh8u */!=E(_wF/* sh8r */.a)){
                return new T2(1,_wF/* sh8r */,new T(function(){
                  return B(_py/* LudoJS.$sremoveFrom */(_wE/* sh8q */, _wG/* sh8u */));
                }));
              }else{
                return E(_wE/* sh8q */);
              }
            }
          });
          return B(_fi/* LudoJS.$sinsert_$sgo10 */(_vs/* sh5z */, new T2(1,new T2(0,_vp/* sh5v */,new T1(1,_vu/* sh5N */)),_wC/* sh8B */), _vt/* sh5G */));
        });
        if(!E(_vs/* sh5z */)){
          return new F(function(){return _wk/* sh7B */(_vU/* sh6J */, _vV/* sh6K */, _vW/* sh6L */, _wB/* sh8l */, _vX/* sh6N */, _/* EXTERNAL */);});
        }else{
          var _wH/* sh8E */ = B(_vY/* sh6O */(_bn/* LudoJS.Blue */, _vU/* sh6J */, _vV/* sh6K */, _vW/* sh6L */, _wB/* sh8l */, _vX/* sh6N */, _/* EXTERNAL */)),
          _wI/* sh8K */ = E(E(_wH/* sh8E */).b);
          return new F(function(){return _wk/* sh7B */(_wI/* sh8K */.a, _wI/* sh8K */.b, _wI/* sh8K */.c, _wI/* sh8K */.d, _wI/* sh8K */.e, _/* EXTERNAL */);});
        }
      }
    }else{
      var _wJ/* sh8Q */ = new T(function(){
        var _wK/* sh8S */ = B(_uB/* Data.OldList.findIndex */(function(_wL/* B1 */){
          return new F(function(){return _b9/* GHC.Classes.eqInt */(_vu/* sh5N */, _wL/* B1 */);});
        }, _vl/* LudoJS.starCells */));
        if(!_wK/* sh8S */._){
          return E(_uP/* Data.Maybe.fromJust1 */);
        }else{
          return E(_wK/* sh8S */.a);
        }
      }),
      _wM/* sh8U */ = new T(function(){
        return B(_pQ/* GHC.List.$w!! */(_vm/* LudoJS.lvl5 */, E(_wJ/* sh8Q */)+1|0));
      }),
      _wN/* sh9G */ = new T(function(){
        var _wO/* sh8Y */ = E(_vr/* sh5x */),
        _wP/* sh9F */ = new T(function(){
          var _wQ/* sh9E */ = new T(function(){
            if((E(_wJ/* sh8Q */)+1|0)!=E(_vn/* LudoJS.lvl6 */)){
              var _wR/* sh9q */ = new T(function(){
                var _wS/* sh9d */ = B(_bt/* LudoJS.$s!1 */(_vs/* sh5z */, _vt/* sh5G */));
                if(!_wS/* sh9d */._){
                  return __Z/* EXTERNAL */;
                }else{
                  var _wT/* sh9f */ = _wS/* sh9d */.b,
                  _wU/* sh9g */ = E(_wS/* sh9d */.a),
                  _wV/* sh9j */ = E(_vp/* sh5v */);
                  if(_wV/* sh9j */!=E(_wU/* sh9g */.a)){
                    return new T2(1,_wU/* sh9g */,new T(function(){
                      return B(_py/* LudoJS.$sremoveFrom */(_wT/* sh9f */, _wV/* sh9j */));
                    }));
                  }else{
                    return E(_wT/* sh9f */);
                  }
                }
              });
              return new T2(1,new T2(0,_vp/* sh5v */,new T1(1,_wM/* sh8U */)),_wR/* sh9q */);
            }else{
              var _wW/* sh9r */ = B(_bt/* LudoJS.$s!1 */(_vs/* sh5z */, _vt/* sh5G */));
              if(!_wW/* sh9r */._){
                return __Z/* EXTERNAL */;
              }else{
                var _wX/* sh9t */ = _wW/* sh9r */.b,
                _wY/* sh9u */ = E(_wW/* sh9r */.a),
                _wZ/* sh9x */ = E(_vp/* sh5v */);
                if(_wZ/* sh9x */!=E(_wY/* sh9u */.a)){
                  return new T2(1,_wY/* sh9u */,new T(function(){
                    return B(_py/* LudoJS.$sremoveFrom */(_wX/* sh9t */, _wZ/* sh9x */));
                  }));
                }else{
                  return E(_wX/* sh9t */);
                }
              }
            }
          });
          return B(_fi/* LudoJS.$sinsert_$sgo10 */(_vs/* sh5z */, _wQ/* sh9E */, _vt/* sh5G */));
        });
        return new T5(0,_wO/* sh8Y */.a,_wO/* sh8Y */.b,_wO/* sh8Y */.c,_wP/* sh9F */,_wO/* sh8Y */.e);
      }),
      _x0/* sh9H */ = B(_qS/* LudoJS.a43 */(_vs/* sh5z */, _vu/* sh5N */, _wN/* sh9G */, _/* EXTERNAL */));
      return new F(function(){return _qS/* LudoJS.a43 */(_vs/* sh5z */, _wM/* sh8U */, new T(function(){
        return E(E(_x0/* sh9H */).b);
      }), _/* EXTERNAL */);});
    }
  }
},
_x1/* f2 */ = new T(function(){
  return eval/* EXTERNAL */("(() => gameState)");
}),
_x2/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */("piece does not exist"));
}),
_x3/* lvl1 */ = new T(function(){
  return B(err/* EXTERNAL */(_x2/* LudoJS.lvl */));
}),
_x4/* lvl10 */ = new T2(1,_bo/* LudoJS.Green */,_hY/* LudoJS.lvl9 */),
_x5/* lvl2 */ = "((x, y, player) => posToField(x, y, player))",
_x6/* lvl42 */ = function(_x7/* shHV */){
  var _x8/* shHW */ = B(_5f/* System.Random.$w$crandomR12 */(_5c/* System.Random.Internal.$fRandomGenStdGen */, 1, 6, _x7/* shHV */));
  return new T2(0,E(_x8/* shHW */.b),_x8/* shHW */.a);
},
_x9/* lvl43 */ = function(_xa/* shIw */){
  var _xb/* shIx */ = B(_5f/* System.Random.$w$crandomR12 */(_5c/* System.Random.Internal.$fRandomGenStdGen */, 1, 4, _xa/* shIw */));
  return new T2(0,E(_xb/* shIx */.b),_xb/* shIx */.a);
},
_xc/* play9 */ = new T2(1,_bq/* LudoJS.Yellow */,_4/* GHC.Types.[] */),
_xd/* play8 */ = new T2(1,_bp/* LudoJS.Red */,_xc/* LudoJS.play9 */),
_xe/* play7 */ = new T2(1,_bo/* LudoJS.Green */,_xd/* LudoJS.play8 */),
_xf/* lvl16 */ = new T2(0,_hP/* LudoJS.lvl15 */,_hO/* LudoJS.Out */),
_xg/* lvl17 */ = new T2(1,_xf/* LudoJS.lvl16 */,_4/* GHC.Types.[] */),
_xh/* lvl18 */ = new T2(0,_b7/* LudoJS.play10 */,_hO/* LudoJS.Out */),
_xi/* lvl19 */ = new T2(1,_xh/* LudoJS.lvl18 */,_xg/* LudoJS.lvl17 */),
_xj/* lvl21 */ = new T2(0,_hS/* LudoJS.lvl20 */,_hO/* LudoJS.Out */),
_xk/* lvl22 */ = new T2(1,_xj/* LudoJS.lvl21 */,_xi/* LudoJS.lvl19 */),
_xl/* lvl23 */ = new T2(0,_oT/* LudoJS.lvl7 */,_hO/* LudoJS.Out */),
_xm/* lvl24 */ = new T2(1,_xl/* LudoJS.lvl23 */,_xk/* LudoJS.lvl22 */),
_xn/* go */ = function(_xo/* sgQU */){
  var _xp/* sgQV */ = E(_xo/* sgQU */);
  return (_xp/* sgQV */._==0) ? __Z/* EXTERNAL */ : new T2(1,new T2(0,_xp/* sgQV */.a,_xm/* LudoJS.lvl24 */),new T(function(){
    return B(_xn/* LudoJS.go */(_xp/* sgQV */.b));
  }));
},
_xq/* play_$sgo */ = function(_xr/* sgQQ */, _xs/* sgQR */){
  return new T2(1,new T2(0,_xr/* sgQQ */,_xm/* LudoJS.lvl24 */),new T(function(){
    return B(_xn/* LudoJS.go */(_xs/* sgQR */));
  }));
},
_xt/* play6 */ = new T(function(){
  return B(_xq/* LudoJS.play_$sgo */(_bn/* LudoJS.Blue */, _xe/* LudoJS.play7 */));
}),
_xu/* play5 */ = new T(function(){
  return B(_hy/* LudoJS.$sfromList */(_xt/* LudoJS.play6 */));
}),
_xv/* $fFractionalFixed1 */ = new T1(0,0),
_xw/* True */ = true,
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
  return B(_xT/* GHC.Integer.Type.mkInteger */(_xw/* GHC.Types.True */, _xZ/* s6TCl */));
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
  return B(_xT/* GHC.Integer.Type.mkInteger */(_xw/* GHC.Types.True */, _yw/* sltl */));
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
    if(!B(_yk/* GHC.Integer.Type.eqInteger */(_yx/* Data.Time.Clock.POSIX.getPOSIXTime2 */, _xv/* Data.Fixed.$fFractionalFixed1 */))){
      return B(_qJ/* GHC.Integer.Type.plusInteger */(B(_yA/* GHC.Integer.Type.timesInteger */(B(_yy/* GHC.Integer.Type.smallInteger */(E(_yL/* sltw */.a))), _y0/* Data.Fixed.$fHasResolutionE5 */)), B(_yc/* GHC.Integer.Type.divInteger */(B(_yA/* GHC.Integer.Type.timesInteger */(B(_yA/* GHC.Integer.Type.timesInteger */(B(_yy/* GHC.Integer.Type.smallInteger */(E(_yL/* sltw */.b))), _y0/* Data.Fixed.$fHasResolutionE5 */)), _y0/* Data.Fixed.$fHasResolutionE5 */)), _yx/* Data.Time.Clock.POSIX.getPOSIXTime2 */))));
    }else{
      return E(_3K/* GHC.Real.divZeroError */);
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
      return new T2(1,_5K/* GHC.Show.shows8 */,new T(function(){
        return B(_q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(B(_AA/* GHC.Integer.Type.integerToJSString */(_AJ/* sf7F */))), new T2(1,_5J/* GHC.Show.shows7 */,_AK/* sf7G */)));
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
_B3/* lvl */ = new T2(1,_5J/* GHC.Show.shows7 */,_4/* GHC.Types.[] */),
_B4/* $fShow(,)1 */ = function(_B5/* sfbb */, _B6/* sfbc */, _B7/* sfbd */){
  return new F(function(){return A1(_B5/* sfbb */,new T2(1,_x/* GHC.Show.showList__1 */,new T(function(){
    return B(A1(_B6/* sfbc */,_B7/* sfbd */));
  })));});
},
_B8/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */(": empty list"));
}),
_B9/* errorEmptyList */ = function(_Ba/* sbDG */){
  return new F(function(){return err/* EXTERNAL */(B(_q/* GHC.Base.++ */(_pF/* GHC.List.prel_list_str */, new T(function(){
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
  return new F(function(){return _5L/* GHC.Show.$wshowSignedInt */(0,  -2147483648, _Bk/* smzT */);});
},
_Bl/* lvl15 */ = function(_Bm/* smzU */){
  return new F(function(){return _5L/* GHC.Show.$wshowSignedInt */(0, 2147483647, _Bm/* smzU */);});
},
_Bn/* lvl16 */ = new T2(1,_Bl/* GHC.Enum.lvl15 */,_4/* GHC.Types.[] */),
_Bo/* lvl17 */ = new T2(1,_Bj/* GHC.Enum.lvl14 */,_Bn/* GHC.Enum.lvl16 */),
_Bp/* lvl18 */ = new T(function(){
  return B(_Bd/* GHC.List.foldr1 */(_B4/* GHC.Show.$fShow(,)1 */, _Bo/* GHC.Enum.lvl17 */));
}),
_Bq/* lvl19 */ = new T(function(){
  return B(A1(_Bp/* GHC.Enum.lvl18 */,_B3/* GHC.Enum.lvl */));
}),
_Br/* lvl20 */ = new T2(1,_5K/* GHC.Show.shows8 */,_Bq/* GHC.Enum.lvl19 */),
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
            return new T2(1,_5K/* GHC.Show.shows8 */,_Ch/* smnF */);
          });
          return B(unAppCStr/* EXTERNAL */(") is outside of bounds ", _Cf/* smnG */));
        },1);
        return B(_q/* GHC.Base.++ */(B(_5L/* GHC.Show.$wshowSignedInt */(0, E(_C8/* smns */), _4/* GHC.Types.[] */)), _Ce/* smnH */));
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
  return (_Cx/* s1RNk */==0) ? E(_3K/* GHC.Real.divZeroError */) : new T2(0,new T(function(){
    return quot/* EXTERNAL */(_Cw/* s1RNg */, _Cx/* s1RNk */);
  }),new T(function(){
    return _Cw/* s1RNg */%_Cx/* s1RNk */;
  }));
},
_Cy/* $fIntegralWord32_$cquot */ = function(_Cz/* s1RMM */, _CA/* s1RMN */){
  var _CB/* s1RMS */ = E(_CA/* s1RMN */);
  if(!_CB/* s1RMS */){
    return E(_3K/* GHC.Real.divZeroError */);
  }else{
    return new F(function(){return quot/* EXTERNAL */(E(_Cz/* s1RMM */), _CB/* s1RMS */);});
  }
},
_CC/* $fIntegralWord32_$cquotRem */ = function(_CD/* s1RN2 */, _CE/* s1RN3 */){
  var _CF/* s1RN8 */ = E(_CE/* s1RN3 */);
  if(!_CF/* s1RN8 */){
    return E(_3K/* GHC.Real.divZeroError */);
  }else{
    var _CG/* s1RN9 */ = quotRemI/* EXTERNAL */(E(_CD/* s1RN2 */), _CF/* s1RN8 */);
    return new T2(0,_CG/* s1RN9 */.a,_CG/* s1RN9 */.b);
  }
},
_CH/* $fIntegralWord32_$crem */ = function(_CI/* s1RMU */, _CJ/* s1RMV */){
  var _CK/* s1RN0 */ = E(_CJ/* s1RMV */);
  return (_CK/* s1RN0 */==0) ? E(_3K/* GHC.Real.divZeroError */) : E(_CI/* s1RMU */)%_CK/* s1RN0 */;
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
    return E(_3K/* GHC.Real.divZeroError */);
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
  return B(_3H/* GHC.Exception.$fExceptionArithException_$ctoException */(_Ed/* GHC.Exception.RatioZeroDenominator */));
}),
_Ef/* ratioZeroDenominatorError */ = new T(function(){
  return die/* EXTERNAL */(_Ee/* GHC.Exception.ratioZeroDenomException */);
}),
_Eg/* $w$sreduce */ = function(_Eh/* svlj */, _Ei/* svlk */){
  if(!B(_yk/* GHC.Integer.Type.eqInteger */(_Ei/* svlk */, _DJ/* GHC.Real.even1 */))){
    var _Ej/* svlm */ = B(_DV/* GHC.Real.$fEnumRatio_gcd' */(B(_E0/* GHC.Integer.Type.absInteger */(_Eh/* svlj */)), B(_E0/* GHC.Integer.Type.absInteger */(_Ei/* svlk */))));
    return (!B(_yk/* GHC.Integer.Type.eqInteger */(_Ej/* svlm */, _DJ/* GHC.Real.even1 */))) ? new T2(0,B(_E5/* GHC.Integer.Type.quotInteger */(_Eh/* svlj */, _Ej/* svlm */)),B(_E5/* GHC.Integer.Type.quotInteger */(_Ei/* svlk */, _Ej/* svlm */))) : E(_3K/* GHC.Real.divZeroError */);
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
      return E(_3K/* GHC.Real.divZeroError */);
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
      return E(_3K/* GHC.Real.divZeroError */);
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
      return E(_3K/* GHC.Real.divZeroError */);
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
      return E(_3K/* GHC.Real.divZeroError */);
    }
  });
},
_Fw/* mkSMGen */ = function(_Fx/* saZo */){
  var _Fy/* saZp */ = E(_Fx/* saZo */);
  return new T2(0,B(_2l/* System.Random.SplitMix.$wmix64 */(_Fy/* saZp */)),B(_4T/* System.Random.SplitMix.$wmixGamma */(B(_22/* GHC.Word.$w$c+ */(_Fy/* saZp */, new Long/* EXTERNAL */(2135587861, 2654435769, true))))));
},
_Fz/* lvl */ = function(_/* EXTERNAL */){
  var _FA/* szS3 */ = B(_yI/* Data.Time.Clock.POSIX.getPOSIXTime1 */(0)),
  _FB/* szS6 */ = B(_Fs/* System.Random.SplitMix.initSMGen2 */(_FA/* szS3 */, 0));
  return new F(function(){return nMV/* EXTERNAL */(new T(function(){
    return B(_Fw/* System.Random.SplitMix.mkSMGen */(_FB/* szS6 */));
  }));});
},
_FC/* theStdGen */ = new T(function(){
  return B(_1S/* GHC.IO.unsafeDupablePerformIO */(_Fz/* System.Random.lvl */));
}),
_FD/* $wlvl */ = function(_FE/* shID */, _FF/* shIE */, _FG/* shIF */, _FH/* shIG */, _/* EXTERNAL */){
  var _FI/* shII */ = E(_FG/* shIF */);
  if(!_FI/* shII */._){
    return _2s/* GHC.Tuple.() */;
  }else{
    if(!E(_FI/* shII */.a)){
      var _FJ/* shIL */ = E(_x1/* LudoJS.f2 */),
      _FK/* shIO */ = __app0/* EXTERNAL */(_FJ/* shIL */),
      _FL/* shIR */ = B(_oD/* LudoJS.$wa1 */(_FK/* shIO */, _/* EXTERNAL */)),
      _FM/* shJ8 */ = function(_FN/* shJ9 */){
        var _FO/* shJh */ = eval/* EXTERNAL */(E(_x5/* LudoJS.lvl2 */)),
        _FP/* shJp */ = __app3/* EXTERNAL */(E(_FO/* shJh */), E(_FE/* shID */), E(_FF/* shIE */), toJSStr/* EXTERNAL */(_FN/* shJ9 */)),
        _FQ/* shJv */ = __eq/* EXTERNAL */(_FP/* shJp */, E(_1W/* Haste.Prim.Any.jsNull */));
        if(!E(_FQ/* shJv */)){
          var _FR/* shJA */ = __isUndef/* EXTERNAL */(_FP/* shJp */);
          if(!E(_FR/* shJA */)){
            var _FS/* shJE */ = new T(function(){
              var _FT/* shJG */ = Number/* EXTERNAL */(_FP/* shJp */);
              return jsTrunc/* EXTERNAL */(_FT/* shJG */);
            }),
            _FU/* shJO */ = __app0/* EXTERNAL */(_FJ/* shIL */),
            _FV/* shJR */ = B(_oD/* LudoJS.$wa1 */(_FU/* shJO */, _/* EXTERNAL */)),
            _FW/* shJU */ = E(_FV/* shJR */),
            _FX/* shJW */ = _FW/* shJU */.b,
            _FY/* shJX */ = _FW/* shJU */.c,
            _FZ/* shJY */ = _FW/* shJU */.d,
            _G0/* shJZ */ = _FW/* shJU */.e,
            _G1/* shK0 */ = E(_FW/* shJU */.a);
            switch(_G1/* shK0 */._){
              case 0:
                if(E(_FS/* shJE */)==( -5)){
                  var _G2/* shK7 */ = mMV/* EXTERNAL */(E(_FC/* System.Random.theStdGen */), _x6/* LudoJS.lvl42 */),
                  _G3/* shKa */ = E(_G2/* shK7 */),
                  _G4/* shKc */ = B(_cn/* LudoJS.a42 */(_G3/* shKa */, _FW/* shJU */, _/* EXTERNAL */)),
                  _G5/* shKf */ = E(_G4/* shKc */),
                  _G6/* shKh */ = _G5/* shKf */.b;
                  if(!E(_G5/* shKf */.a)._){
                    var _G7/* shKj */ = E(_G6/* shKh */),
                    _G8/* shKs */ = B(_p0/* LudoJS.$wa13 */(_G7/* shKj */.a, _G7/* shKj */.b, E(_G7/* shKj */.c)-1|0, _G7/* shKj */.d, _G7/* shKj */.e, _/* EXTERNAL */)),
                    _G9/* shKy */ = E(E(_G8/* shKs */).b);
                    return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, new T1(0,new T1(1,_G3/* shKa */)), _G9/* shKy */.b, _G9/* shKy */.c, _G9/* shKy */.d, _G9/* shKy */.e);});
                  }else{
                    var _Ga/* shKI */ = E(_G6/* shKh */);
                    return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, new T1(1,_G3/* shKa */), _Ga/* shKI */.b, new T(function(){
                      if(E(_G3/* shKa */)==6){
                        return E(_oT/* LudoJS.lvl7 */);
                      }else{
                        return E(_bm/* LudoJS.$fShowStage2 */);
                      }
                    }), _Ga/* shKI */.d, _Ga/* shKI */.e);});
                  }
                }else{
                  return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G1/* shK0 */, _FX/* shJW */, _FY/* shJX */, _FZ/* shJY */, _G0/* shJZ */);});
                }
                break;
              case 1:
                var _Gb/* shKR */ = _G1/* shK0 */.a,
                _Gc/* shKS */ = E(_FS/* shJE */),
                _Gd/* shKU */ = function(_/* EXTERNAL */, _Ge/* shKW */, _Gf/* shKX */, _Gg/* shKY */, _Gh/* shKZ */, _Gi/* shL0 */, _Gj/* shL1 */){
                  var _Gk/* shL2 */ = E(_Ge/* shKW */);
                  if(!_Gk/* shL2 */._){
                    return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _Gf/* shKX */, _Gg/* shKY */, _Gh/* shKZ */, _Gi/* shL0 */, _Gj/* shL1 */);});
                  }else{
                    var _Gl/* shL4 */ = B(_bt/* LudoJS.$s!1 */(_Gg/* shKY */, _Gi/* shL0 */));
                    if(!_Gl/* shL4 */._){
                      return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _Gf/* shKX */, _Gg/* shKY */, _Gh/* shKZ */, _Gi/* shL0 */, _Gj/* shL1 */);});
                    }else{
                      var _Gm/* shLc */ = E(_Gk/* shL2 */.a);
                      if(E(E(_Gl/* shL4 */.a).a)!=_Gm/* shLc */){
                        var _Gn/* shLg */ = function(_Go/* shLh */){
                          while(1){
                            var _Gp/* shLi */ = E(_Go/* shLh */);
                            if(!_Gp/* shLi */._){
                              return false;
                            }else{
                              if(E(E(_Gp/* shLi */.a).a)!=_Gm/* shLc */){
                                _Go/* shLh */ = _Gp/* shLi */.b;
                                continue;
                              }else{
                                return true;
                              }
                            }
                          }
                        };
                        if(!B(_Gn/* shLg */(_Gl/* shL4 */.b))){
                          return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _Gf/* shKX */, _Gg/* shKY */, _Gh/* shKZ */, _Gi/* shL0 */, _Gj/* shL1 */);});
                        }else{
                          return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, new T2(2,_Gb/* shKR */,_Gm/* shLc */), _Gg/* shKY */, _Gh/* shKZ */, _Gi/* shL0 */, _Gj/* shL1 */);});
                        }
                      }else{
                        return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, new T2(2,_Gb/* shKR */,_Gm/* shLc */), _Gg/* shKY */, _Gh/* shKZ */, _Gi/* shL0 */, _Gj/* shL1 */);});
                      }
                    }
                  }
                };
                if(_Gc/* shKS */<( -4)){
                  if(_Gc/* shKS */<0){
                    return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G1/* shK0 */, _FX/* shJW */, _FY/* shJX */, _FZ/* shJY */, _G0/* shJZ */);});
                  }else{
                    if(_Gc/* shKS */>55){
                      return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G1/* shK0 */, _FX/* shJW */, _FY/* shJX */, _FZ/* shJY */, _G0/* shJZ */);});
                    }else{
                      var _Gq/* shLB */ = function(_Gr/* shLC */){
                        while(1){
                          var _Gs/* shLD */ = E(_Gr/* shLC */);
                          if(!_Gs/* shLD */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _Gt/* shLF */ = _Gs/* shLD */.b,
                            _Gu/* shLG */ = E(_Gs/* shLD */.a),
                            _Gv/* shLJ */ = E(_Gu/* shLG */.b);
                            if(!_Gv/* shLJ */._){
                              _Gr/* shLC */ = _Gt/* shLF */;
                              continue;
                            }else{
                              if(_Gc/* shKS */!=E(_Gv/* shLJ */.a)){
                                _Gr/* shLC */ = _Gt/* shLF */;
                                continue;
                              }else{
                                return new T1(1,_Gu/* shLG */.a);
                              }
                            }
                          }
                        }
                      };
                      return new F(function(){return _Gd/* shKU */(_/* EXTERNAL */, B(_Gq/* shLB */(B(_bt/* LudoJS.$s!1 */(_FX/* shJW */, _FZ/* shJY */)))), _G1/* shK0 */, _FX/* shJW */, _FY/* shJX */, _FZ/* shJY */, _G0/* shJZ */);});
                    }
                  }
                }else{
                  if(_Gc/* shKS */>( -1)){
                    if(_Gc/* shKS */<0){
                      return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G1/* shK0 */, _FX/* shJW */, _FY/* shJX */, _FZ/* shJY */, _G0/* shJZ */);});
                    }else{
                      if(_Gc/* shKS */>55){
                        return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G1/* shK0 */, _FX/* shJW */, _FY/* shJX */, _FZ/* shJY */, _G0/* shJZ */);});
                      }else{
                        var _Gw/* shLX */ = function(_Gx/* shLY */){
                          while(1){
                            var _Gy/* shLZ */ = E(_Gx/* shLY */);
                            if(!_Gy/* shLZ */._){
                              return __Z/* EXTERNAL */;
                            }else{
                              var _Gz/* shM1 */ = _Gy/* shLZ */.b,
                              _GA/* shM2 */ = E(_Gy/* shLZ */.a),
                              _GB/* shM5 */ = E(_GA/* shM2 */.b);
                              if(!_GB/* shM5 */._){
                                _Gx/* shLY */ = _Gz/* shM1 */;
                                continue;
                              }else{
                                if(_Gc/* shKS */!=E(_GB/* shM5 */.a)){
                                  _Gx/* shLY */ = _Gz/* shM1 */;
                                  continue;
                                }else{
                                  return new T1(1,_GA/* shM2 */.a);
                                }
                              }
                            }
                          }
                        };
                        return new F(function(){return _Gd/* shKU */(_/* EXTERNAL */, B(_Gw/* shLX */(B(_bt/* LudoJS.$s!1 */(_FX/* shJW */, _FZ/* shJY */)))), _G1/* shK0 */, _FX/* shJW */, _FY/* shJX */, _FZ/* shJY */, _G0/* shJZ */);});
                      }
                    }
                  }else{
                    var _GC/* shMd */ = _Gc/* shKS */+5|0,
                    _GD/* shMg */ = function(_GE/* shMh */){
                      while(1){
                        var _GF/* shMi */ = E(_GE/* shMh */);
                        if(!_GF/* shMi */._){
                          return __Z/* EXTERNAL */;
                        }else{
                          var _GG/* shMk */ = _GF/* shMi */.b,
                          _GH/* shMl */ = E(_GF/* shMi */.a);
                          if(E(_GH/* shMl */.a)!=_GC/* shMd */){
                            _GE/* shMh */ = _GG/* shMk */;
                            continue;
                          }else{
                            if(!E(_GH/* shMl */.b)._){
                              return E(new T1(1,_GC/* shMd */));
                            }else{
                              _GE/* shMh */ = _GG/* shMk */;
                              continue;
                            }
                          }
                        }
                      }
                    };
                    return new F(function(){return _Gd/* shKU */(_/* EXTERNAL */, B(_GD/* shMg */(B(_bt/* LudoJS.$s!1 */(_FX/* shJW */, _FZ/* shJY */)))), _G1/* shK0 */, _FX/* shJW */, _FY/* shJX */, _FZ/* shJY */, _G0/* shJZ */);});
                  }
                }
                break;
              case 2:
                var _GI/* shMw */ = _G1/* shK0 */.a,
                _GJ/* shMx */ = _G1/* shK0 */.b,
                _GK/* shMy */ = E(_FS/* shJE */);
                if(_GK/* shMy */>56){
                  return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G1/* shK0 */, _FX/* shJW */, _FY/* shJX */, _FZ/* shJY */, _G0/* shJZ */);});
                }else{
                  if(_GK/* shMy */<0){
                    return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _G1/* shK0 */, _FX/* shJW */, _FY/* shJX */, _FZ/* shJY */, _G0/* shJZ */);});
                  }else{
                    var _GL/* shME */ = B(_cn/* LudoJS.a42 */(_GI/* shMw */, _FW/* shJU */, _/* EXTERNAL */)),
                    _GM/* shMH */ = E(_GL/* shME */),
                    _GN/* shMJ */ = _GM/* shMH */.b,
                    _GO/* shMK */ = new T(function(){
                      var _GP/* shML */ = new T2(1,_GJ/* shMx */,_GK/* shMy */),
                      _GQ/* shMM */ = B(_bt/* LudoJS.$s!1 */(_FX/* shJW */, _FZ/* shJY */));
                      if(!_GQ/* shMM */._){
                        return E(_x3/* LudoJS.lvl1 */);
                      }else{
                        var _GR/* shMP */ = E(_GQ/* shMM */.a),
                        _GS/* shMS */ = E(_GR/* shMP */.a),
                        _GT/* shMU */ = E(_GJ/* shMx */);
                        if(_GS/* shMS */!=_GT/* shMU */){
                          var _GU/* shMY */ = function(_GV/* shMZ */){
                            while(1){
                              var _GW/* shN0 */ = E(_GV/* shMZ */);
                              if(!_GW/* shN0 */._){
                                return E(_x3/* LudoJS.lvl1 */);
                              }else{
                                var _GX/* shN3 */ = E(_GW/* shN0 */.a),
                                _GY/* shN6 */ = E(_GX/* shN3 */.a);
                                if(_GY/* shN6 */!=_GT/* shMU */){
                                  _GV/* shMZ */ = _GW/* shN0 */.b;
                                  continue;
                                }else{
                                  return (E(_GX/* shN3 */.b)._==0) ? new T1(0,_GY/* shN6 */) : E(_GP/* shML */);
                                }
                              }
                            }
                          };
                          return B(_GU/* shMY */(_GQ/* shMM */.b));
                        }else{
                          if(!E(_GR/* shMP */.b)._){
                            return new T1(0,_GS/* shMS */);
                          }else{
                            return E(_GP/* shML */);
                          }
                        }
                      }
                    });
                    if(!B(_uw/* GHC.List.elem */(_bl/* LudoJS.$fEqOption */, _GO/* shMK */, _GM/* shMH */.a))){
                      var _GZ/* shNf */ = E(_GN/* shMJ */);
                      return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, new T1(1,_GI/* shMw */), _GZ/* shNf */.b, _GZ/* shNf */.c, _GZ/* shNf */.d, _GZ/* shNf */.e);});
                    }else{
                      var _H0/* shNm */ = function(_/* EXTERNAL */, _H1/* shNo */, _H2/* shNp */, _H3/* shNq */, _H4/* shNr */, _H5/* shNs */){
                        if(!B(_bB/* GHC.List.$wlenAcc */(B(_bt/* LudoJS.$s!1 */(_H2/* shNp */, _H4/* shNr */)), 0))){
                          var _H6/* shNV */ = B(_q/* GHC.Base.++ */(_H5/* shNs */, new T2(1,_H2/* shNp */,_4/* GHC.Types.[] */)));
                          if(B(_bB/* GHC.List.$wlenAcc */(_H6/* shNV */, 0))==3){
                            var _H7/* shO8 */ = B(_c3/* LudoJS.$sa1 */(_bn/* LudoJS.Blue */, _x4/* LudoJS.lvl10 */, _H6/* shNV */, _H1/* shNo */, _H2/* shNp */, _H3/* shNq */, _H4/* shNr */, _H6/* shNV */, _/* EXTERNAL */)),
                            _H8/* shOf */ = B(_pa/* LudoJS.$wa14 */(_of/* LudoJS.GameFinished */, _H2/* shNp */, _H3/* shNq */, _H4/* shNr */, new T(function(){
                              return E(E(_H7/* shO8 */).a);
                            }), _/* EXTERNAL */));
                            return new T2(0,_2s/* GHC.Tuple.() */,new T(function(){
                              return E(E(_H8/* shOf */).b);
                            }));
                          }else{
                            var _H9/* shO0 */ = B(_pa/* LudoJS.$wa14 */(new T1(0,new T1(1,_GI/* shMw */)), _H2/* shNp */, _H3/* shNq */, _H4/* shNr */, _H6/* shNV */, _/* EXTERNAL */));
                            return new T2(0,_2s/* GHC.Tuple.() */,new T(function(){
                              return E(E(_H9/* shO0 */).b);
                            }));
                          }
                        }else{
                          if(B(_bB/* GHC.List.$wlenAcc */(_H5/* shNs */, 0))==3){
                            var _Ha/* shNG */ = B(_c3/* LudoJS.$sa1 */(_bn/* LudoJS.Blue */, _x4/* LudoJS.lvl10 */, _H5/* shNs */, _H1/* shNo */, _H2/* shNp */, _H3/* shNq */, _H4/* shNr */, _H5/* shNs */, _/* EXTERNAL */)),
                            _Hb/* shNN */ = B(_pa/* LudoJS.$wa14 */(_of/* LudoJS.GameFinished */, _H2/* shNp */, _H3/* shNq */, _H4/* shNr */, new T(function(){
                              return E(E(_Ha/* shNG */).a);
                            }), _/* EXTERNAL */));
                            return new T2(0,_2s/* GHC.Tuple.() */,new T(function(){
                              return E(E(_Hb/* shNN */).b);
                            }));
                          }else{
                            var _Hc/* shNy */ = B(_pa/* LudoJS.$wa14 */(new T1(0,new T1(1,_GI/* shMw */)), _H2/* shNp */, _H3/* shNq */, _H4/* shNr */, _H5/* shNs */, _/* EXTERNAL */));
                            return new T2(0,_2s/* GHC.Tuple.() */,new T(function(){
                              return E(E(_Hc/* shNy */).b);
                            }));
                          }
                        }
                      },
                      _Hd/* shOn */ = E(_GO/* shMK */);
                      if(!_Hd/* shOn */._){
                        var _He/* shOp */ = B(_vo/* LudoJS.$wa6 */(_Hd/* shOn */.a, 0, _GN/* shMJ */, _/* EXTERNAL */)),
                        _Hf/* shOv */ = E(E(_He/* shOp */).b),
                        _Hg/* shOB */ = B(_H0/* shNm */(_/* EXTERNAL */, _Hf/* shOv */.a, _Hf/* shOv */.b, _Hf/* shOv */.c, _Hf/* shOv */.d, _Hf/* shOv */.e)),
                        _Hh/* shOH */ = E(E(_Hg/* shOB */).b);
                        return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _Hh/* shOH */.a, _Hh/* shOH */.b, _Hh/* shOH */.c, _Hh/* shOH */.d, _Hh/* shOH */.e);});
                      }else{
                        var _Hi/* shOR */ = B(_vo/* LudoJS.$wa6 */(_Hd/* shOn */.a, E(_Hd/* shOn */.b), _GN/* shMJ */, _/* EXTERNAL */)),
                        _Hj/* shOX */ = E(E(_Hi/* shOR */).b),
                        _Hk/* shP3 */ = B(_H0/* shNm */(_/* EXTERNAL */, _Hj/* shOX */.a, _Hj/* shOX */.b, _Hj/* shOX */.c, _Hj/* shOX */.d, _Hj/* shOX */.e)),
                        _Hl/* shP9 */ = E(E(_Hk/* shP3 */).b);
                        return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _Hl/* shP9 */.a, _Hl/* shP9 */.b, _Hl/* shP9 */.c, _Hl/* shP9 */.d, _Hl/* shP9 */.e);});
                      }
                    }
                  }
                }
                break;
              default:
                var _Hm/* shPh */ = mMV/* EXTERNAL */(E(_FC/* System.Random.theStdGen */), _x9/* LudoJS.lvl43 */);
                return new F(function(){return _dm/* LudoJS.$w$j */(_/* EXTERNAL */, _b8/* LudoJS.play11 */, new T(function(){
                  switch(E(_Hm/* shPh */)){
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
                }), _b7/* LudoJS.play10 */, _xu/* LudoJS.play5 */, _4/* GHC.Types.[] */);});
            }
          }else{
            return _2s/* GHC.Tuple.() */;
          }
        }else{
          return _2s/* GHC.Tuple.() */;
        }
      };
      switch(E(E(_FL/* shIR */).b)){
        case 0:
          return new F(function(){return _FM/* shJ8 */(E(_1u/* LudoJS.$fFromAnyGameState14 */));});
          break;
        case 1:
          return new F(function(){return _FM/* shJ8 */(E(_1t/* LudoJS.$fFromAnyGameState13 */));});
          break;
        case 2:
          return new F(function(){return _FM/* shJ8 */(E(_1s/* LudoJS.$fFromAnyGameState12 */));});
          break;
        default:
          return new F(function(){return _FM/* shJ8 */(E(_1r/* LudoJS.$fFromAnyGameState11 */));});
      }
    }else{
      return _2s/* GHC.Tuple.() */;
    }
  }
},
_Hn/* play2 */ = function(_Ho/* shPt */, _/* EXTERNAL */){
  var _Hp/* shPv */ = E(_Ho/* shPt */),
  _Hq/* shPz */ = E(_Hp/* shPv */.a);
  return new F(function(){return _FD/* LudoJS.$wlvl */(_Hq/* shPz */.a, _Hq/* shPz */.b, _Hp/* shPv */.b, _Hp/* shPv */.c, _/* EXTERNAL */);});
},
_Hr/* play4 */ = new T(function(){
  return B(_ae/* GHC.Base.map */(_cl/* LudoJS.$fToAnyOption_$ctoAny */, _4/* GHC.Types.[] */));
}),
_Hs/* play1 */ = function(_Ht/* shPC */, _Hu/* shPD */, _/* EXTERNAL */){
  var _Hv/* shPG */ = B(_ai/* LudoJS.$w$ctoAny */(new T5(0,_b8/* LudoJS.play11 */,_Ht/* shPC */,_b7/* LudoJS.play10 */,_xu/* LudoJS.play5 */,_4/* GHC.Types.[] */))),
  _Hw/* shPK */ = __app1/* EXTERNAL */(E(_dl/* LudoJS.play_f1 */), _Hv/* shPG */),
  _Hx/* shPQ */ = __lst2arr/* EXTERNAL */(E(_Hr/* LudoJS.play4 */)),
  _Hy/* shPW */ = eval/* EXTERNAL */(E(_di/* LudoJS.play3 */)),
  _Hz/* shQ6 */ = __app3/* EXTERNAL */(E(_Hy/* shPW */), _Hv/* shPG */, _Hx/* shPQ */,  -1),
  _HA/* shQ9 */ = B(A(_aF/* Haste.Events.Core.onEvent */,[_93/* Haste.Events.Core.$fMonadEventIO */, _8j/* Haste.Events.Core.$fEventSourceElem1 */, _8i/* Haste.Events.MouseEvents.$fEventMouseEvent */, _Hu/* shPD */, _al/* Haste.Events.MouseEvents.Click */, _Hn/* LudoJS.play2 */, _/* EXTERNAL */]));
  return _2s/* GHC.Tuple.() */;
},
_HB/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */(" found!"));
}),
_HC/* withElem1 */ = function(_HD/* svSB */){
  return new F(function(){return err/* EXTERNAL */(B(unAppCStr/* EXTERNAL */("No element with ID ", new T(function(){
    return B(_q/* GHC.Base.++ */(fromJSStr/* EXTERNAL */(E(_HD/* svSB */)), _HB/* Haste.DOM.JSString.lvl */));
  }))));});
},
_HE/* main1 */ = function(_/* EXTERNAL */){
  var _HF/* ssQ7 */ = function(_HG/* ssPH */){
    var _HH/* ssPI */ = new T(function(){
      var _HI/* ssPJ */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_HG/* ssPH */, _/* EXTERNAL */));
      return E(_HI/* ssPJ */);
    }),
    _HJ/* ssQ6 */ = function(_HK/* ssPM */){
      var _HL/* ssPN */ = new T(function(){
        var _HM/* ssPR */ = Number/* EXTERNAL */(E(_HK/* ssPM */));
        return jsTrunc/* EXTERNAL */(_HM/* ssPR */);
      });
      return function(_HN/* ssPY */){
        var _HO/* ssQ1 */ = B(_1A/* LudoJS.$fFromAnyGameState9 */(_HN/* ssPY */, _/* EXTERNAL */));
        return new F(function(){return _1L/* LudoJS.$wconvertCell */(_HH/* ssPI */, E(_HL/* ssPN */), _HO/* ssQ1 */);});
      };
    };
    return E(_HJ/* ssQ6 */);
  },
  _HP/* ssQb */ = __createJSFunc/* EXTERNAL */(3, E(_HF/* ssQ7 */)),
  _HQ/* ssQj */ = __app2/* EXTERNAL */(E(_1Q/* Main.f2 */), "convertCell", _HP/* ssQb */),
  _HR/* ssQo */ = mMV/* EXTERNAL */(E(_FC/* System.Random.theStdGen */), _5x/* Main.lvl4 */),
  _HS/* ssQv */ = "canvas",
  _HT/* ssQB */ = __app1/* EXTERNAL */(E(_1P/* Haste.DOM.JSString.elemById_f1 */), _HS/* ssQv */),
  _HU/* ssQH */ = __eq/* EXTERNAL */(_HT/* ssQB */, E(_1W/* Haste.Prim.Any.jsNull */));
  if(!E(_HU/* ssQH */)){
    var _HV/* ssQN */ = __isUndef/* EXTERNAL */(_HT/* ssQB */);
    if(!E(_HV/* ssQN */)){
      return new F(function(){return _Hs/* LudoJS.play1 */(new T(function(){
        switch(E(_HR/* ssQo */)){
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
      }), _HT/* ssQB */, _/* EXTERNAL */);});
    }else{
      return new F(function(){return _HC/* Haste.DOM.JSString.withElem1 */(_HS/* ssQv */);});
    }
  }else{
    return new F(function(){return _HC/* Haste.DOM.JSString.withElem1 */(_HS/* ssQv */);});
  }
},
_HW/* main */ = function(_/* EXTERNAL */){
  return new F(function(){return _HE/* Main.main1 */(_/* EXTERNAL */);});
};

var hasteMain = function() {B(A(_HW, [0]));};onHasteStart(); hasteMain();