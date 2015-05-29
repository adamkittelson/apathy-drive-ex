(function(/*! Brunch !*/) {
  'use strict';

  var globals = typeof window !== 'undefined' ? window : global;
  if (typeof globals.require === 'function') return;

  var modules = {};
  var cache = {};

  var has = function(object, name) {
    return ({}).hasOwnProperty.call(object, name);
  };

  var expand = function(root, name) {
    var results = [], parts, part;
    if (/^\.\.?(\/|$)/.test(name)) {
      parts = [root, name].join('/').split('/');
    } else {
      parts = name.split('/');
    }
    for (var i = 0, length = parts.length; i < length; i++) {
      part = parts[i];
      if (part === '..') {
        results.pop();
      } else if (part !== '.' && part !== '') {
        results.push(part);
      }
    }
    return results.join('/');
  };

  var dirname = function(path) {
    return path.split('/').slice(0, -1).join('/');
  };

  var localRequire = function(path) {
    return function(name) {
      var dir = dirname(path);
      var absolute = expand(dir, name);
      return globals.require(absolute, path);
    };
  };

  var initModule = function(name, definition) {
    var module = {id: name, exports: {}};
    cache[name] = module;
    definition(module.exports, localRequire(name), module);
    return module.exports;
  };

  var require = function(name, loaderPath) {
    var path = expand(name, '.');
    if (loaderPath == null) loaderPath = '/';

    if (has(cache, path)) return cache[path].exports;
    if (has(modules, path)) return initModule(path, modules[path]);

    var dirIndex = expand(path, './index');
    if (has(cache, dirIndex)) return cache[dirIndex].exports;
    if (has(modules, dirIndex)) return initModule(dirIndex, modules[dirIndex]);

    throw new Error('Cannot find module "' + name + '" from '+ '"' + loaderPath + '"');
  };

  var define = function(bundle, fn) {
    if (typeof bundle === 'object') {
      for (var key in bundle) {
        if (has(bundle, key)) {
          modules[key] = bundle[key];
        }
      }
    } else {
      modules[bundle] = fn;
    }
  };

  var list = function() {
    var result = [];
    for (var item in modules) {
      if (has(modules, item)) {
        result.push(item);
      }
    }
    return result;
  };

  globals.require = require;
  globals.require.define = define;
  globals.require.register = define;
  globals.require.list = list;
  globals.require.brunch = true;
})();
"use strict";

/*! jQuery v1.10.2 | (c) 2005, 2013 jQuery Foundation, Inc. | jquery.org/license
//@ sourceMappingURL=jquery-1.10.2.min.map
*/
(function (e, t) {
  var n,
      r,
      i = typeof t,
      o = e.location,
      a = e.document,
      s = a.documentElement,
      l = e.jQuery,
      u = e.$,
      c = {},
      p = [],
      f = "1.10.2",
      d = p.concat,
      h = p.push,
      g = p.slice,
      m = p.indexOf,
      y = c.toString,
      v = c.hasOwnProperty,
      b = f.trim,
      x = (function (_x) {
    var _xWrapper = function x(_x2, _x3) {
      return _x.apply(this, arguments);
    };

    _xWrapper.toString = function () {
      return _x.toString();
    };

    return _xWrapper;
  })(function (e, t) {
    return new x.fn.init(e, t, r);
  }),
      w = /[+-]?(?:\d*\.|)\d+(?:[eE][+-]?\d+|)/.source,
      T = /\S+/g,
      C = /^[\s\uFEFF\xA0]+|[\s\uFEFF\xA0]+$/g,
      N = /^(?:\s*(<[\w\W]+>)[^>]*|#([\w-]*))$/,
      k = /^<(\w+)\s*\/?>(?:<\/\1>|)$/,
      E = /^[\],:{}\s]*$/,
      S = /(?:^|:|,)(?:\s*\[)+/g,
      A = /\\(?:["\\\/bfnrt]|u[\da-fA-F]{4})/g,
      j = /"[^"\\\r\n]*"|true|false|null|-?(?:\d+\.|)\d+(?:[eE][+-]?\d+|)/g,
      D = /^-ms-/,
      L = /-([\da-z])/gi,
      H = function H(e, t) {
    return t.toUpperCase();
  },
      q = function q(e) {
    (a.addEventListener || "load" === e.type || "complete" === a.readyState) && (_(), x.ready());
  },
      _ = function _() {
    a.addEventListener ? (a.removeEventListener("DOMContentLoaded", q, !1), e.removeEventListener("load", q, !1)) : (a.detachEvent("onreadystatechange", q), e.detachEvent("onload", q));
  };x.fn = x.prototype = { jquery: f, constructor: x, init: function init(e, n, r) {
      var i, o;if (!e) {
        return this;
      }if ("string" == typeof e) {
        if ((i = "<" === e.charAt(0) && ">" === e.charAt(e.length - 1) && e.length >= 3 ? [null, e, null] : N.exec(e), !i || !i[1] && n)) {
          return !n || n.jquery ? (n || r).find(e) : this.constructor(n).find(e);
        }if (i[1]) {
          if ((n = n instanceof x ? n[0] : n, x.merge(this, x.parseHTML(i[1], n && n.nodeType ? n.ownerDocument || n : a, !0)), k.test(i[1]) && x.isPlainObject(n))) for (i in n) x.isFunction(this[i]) ? this[i](n[i]) : this.attr(i, n[i]);return this;
        }if ((o = a.getElementById(i[2]), o && o.parentNode)) {
          if (o.id !== i[2]) {
            return r.find(e);
          }this.length = 1, this[0] = o;
        }return (this.context = a, this.selector = e, this);
      }return e.nodeType ? (this.context = this[0] = e, this.length = 1, this) : x.isFunction(e) ? r.ready(e) : (e.selector !== t && (this.selector = e.selector, this.context = e.context), x.makeArray(e, this));
    }, selector: "", length: 0, toArray: function toArray() {
      return g.call(this);
    }, get: function get(e) {
      return null == e ? this.toArray() : 0 > e ? this[this.length + e] : this[e];
    }, pushStack: function pushStack(e) {
      var t = x.merge(this.constructor(), e);return (t.prevObject = this, t.context = this.context, t);
    }, each: function each(e, t) {
      return x.each(this, e, t);
    }, ready: function ready(e) {
      return (x.ready.promise().done(e), this);
    }, slice: function slice() {
      return this.pushStack(g.apply(this, arguments));
    }, first: function first() {
      return this.eq(0);
    }, last: function last() {
      return this.eq(-1);
    }, eq: function eq(e) {
      var t = this.length,
          n = +e + (0 > e ? t : 0);return this.pushStack(n >= 0 && t > n ? [this[n]] : []);
    }, map: function map(e) {
      return this.pushStack(x.map(this, function (t, n) {
        return e.call(t, n, t);
      }));
    }, end: function end() {
      return this.prevObject || this.constructor(null);
    }, push: h, sort: [].sort, splice: [].splice }, x.fn.init.prototype = x.fn, x.extend = x.fn.extend = function () {
    var e,
        n,
        r,
        i,
        o,
        a,
        s = arguments[0] || {},
        l = 1,
        u = arguments.length,
        c = !1;for ("boolean" == typeof s && (c = s, s = arguments[1] || {}, l = 2), "object" == typeof s || x.isFunction(s) || (s = {}), u === l && (s = this, --l); u > l; l++) if (null != (o = arguments[l])) for (i in o) e = s[i], r = o[i], s !== r && (c && r && (x.isPlainObject(r) || (n = x.isArray(r))) ? (n ? (n = !1, a = e && x.isArray(e) ? e : []) : a = e && x.isPlainObject(e) ? e : {}, s[i] = x.extend(c, a, r)) : r !== t && (s[i] = r));return s;
  }, x.extend({ expando: "jQuery" + (f + Math.random()).replace(/\D/g, ""), noConflict: function noConflict(t) {
      return (e.$ === x && (e.$ = u), t && e.jQuery === x && (e.jQuery = l), x);
    }, isReady: !1, readyWait: 1, holdReady: function holdReady(e) {
      e ? x.readyWait++ : x.ready(!0);
    }, ready: function ready(e) {
      if (e === !0 ? ! --x.readyWait : !x.isReady) {
        if (!a.body) {
          return setTimeout(x.ready);
        }x.isReady = !0, e !== !0 && --x.readyWait > 0 || (n.resolveWith(a, [x]), x.fn.trigger && x(a).trigger("ready").off("ready"));
      }
    }, isFunction: function isFunction(e) {
      return "function" === x.type(e);
    }, isArray: Array.isArray || function (e) {
      return "array" === x.type(e);
    }, isWindow: function isWindow(e) {
      return null != e && e == e.window;
    }, isNumeric: function isNumeric(e) {
      return !isNaN(parseFloat(e)) && isFinite(e);
    }, type: function type(e) {
      return null == e ? e + "" : "object" == typeof e || "function" == typeof e ? c[y.call(e)] || "object" : typeof e;
    }, isPlainObject: function isPlainObject(e) {
      var n;if (!e || "object" !== x.type(e) || e.nodeType || x.isWindow(e)) {
        return !1;
      }try {
        if (e.constructor && !v.call(e, "constructor") && !v.call(e.constructor.prototype, "isPrototypeOf")) return !1;
      } catch (r) {
        return !1;
      }if (x.support.ownLast) for (n in e) return v.call(e, n);for (n in e);return n === t || v.call(e, n);
    }, isEmptyObject: function isEmptyObject(e) {
      var t;for (t in e) return !1;return !0;
    }, error: function error(e) {
      throw Error(e);
    }, parseHTML: function parseHTML(e, t, n) {
      if (!e || "string" != typeof e) {
        return null;
      }"boolean" == typeof t && (n = t, t = !1), t = t || a;var r = k.exec(e),
          i = !n && [];return r ? [t.createElement(r[1])] : (r = x.buildFragment([e], t, i), i && x(i).remove(), x.merge([], r.childNodes));
    }, parseJSON: function parseJSON(n) {
      return e.JSON && e.JSON.parse ? e.JSON.parse(n) : null === n ? n : "string" == typeof n && (n = x.trim(n), n && E.test(n.replace(A, "@").replace(j, "]").replace(S, ""))) ? Function("return " + n)() : (x.error("Invalid JSON: " + n), t);
    }, parseXML: function parseXML(n) {
      var r, i;if (!n || "string" != typeof n) {
        return null;
      }try {
        e.DOMParser ? (i = new DOMParser(), r = i.parseFromString(n, "text/xml")) : (r = new ActiveXObject("Microsoft.XMLDOM"), r.async = "false", r.loadXML(n));
      } catch (o) {
        r = t;
      }return (r && r.documentElement && !r.getElementsByTagName("parsererror").length || x.error("Invalid XML: " + n), r);
    }, noop: function noop() {}, globalEval: function globalEval(t) {
      t && x.trim(t) && (e.execScript || function (t) {
        e.eval.call(e, t);
      })(t);
    }, camelCase: function camelCase(e) {
      return e.replace(D, "ms-").replace(L, H);
    }, nodeName: function nodeName(e, t) {
      return e.nodeName && e.nodeName.toLowerCase() === t.toLowerCase();
    }, each: function each(e, t, n) {
      var r,
          i = 0,
          o = e.length,
          a = M(e);if (n) {
        if (a) {
          for (; o > i; i++) if ((r = t.apply(e[i], n), r === !1)) break;
        } else for (i in e) if ((r = t.apply(e[i], n), r === !1)) break;
      } else if (a) {
        for (; o > i; i++) if ((r = t.call(e[i], i, e[i]), r === !1)) break;
      } else for (i in e) if ((r = t.call(e[i], i, e[i]), r === !1)) break;return e;
    }, trim: b && !b.call("﻿ ") ? function (e) {
      return null == e ? "" : b.call(e);
    } : function (e) {
      return null == e ? "" : (e + "").replace(C, "");
    }, makeArray: function makeArray(e, t) {
      var n = t || [];return (null != e && (M(Object(e)) ? x.merge(n, "string" == typeof e ? [e] : e) : h.call(n, e)), n);
    }, inArray: function inArray(e, t, n) {
      var r;if (t) {
        if (m) {
          return m.call(t, e, n);
        }for (r = t.length, n = n ? 0 > n ? Math.max(0, r + n) : n : 0; r > n; n++) if (n in t && t[n] === e) {
          return n;
        }
      }return -1;
    }, merge: function merge(e, n) {
      var r = n.length,
          i = e.length,
          o = 0;if ("number" == typeof r) for (; r > o; o++) e[i++] = n[o];else while (n[o] !== t) e[i++] = n[o++];return (e.length = i, e);
    }, grep: function grep(e, t, n) {
      var r,
          i = [],
          o = 0,
          a = e.length;for (n = !!n; a > o; o++) r = !!t(e[o], o), n !== r && i.push(e[o]);return i;
    }, map: function map(e, t, n) {
      var r,
          i = 0,
          o = e.length,
          a = M(e),
          s = [];if (a) for (; o > i; i++) r = t(e[i], i, n), null != r && (s[s.length] = r);else for (i in e) r = t(e[i], i, n), null != r && (s[s.length] = r);return d.apply([], s);
    }, guid: 1, proxy: function proxy(e, n) {
      var r, i, o;return ("string" == typeof n && (o = e[n], n = e, e = o), x.isFunction(e) ? (r = g.call(arguments, 2), i = function () {
        return e.apply(n || this, r.concat(g.call(arguments)));
      }, i.guid = e.guid = e.guid || x.guid++, i) : t);
    }, access: function access(e, n, r, i, o, a, s) {
      var l = 0,
          u = e.length,
          c = null == r;if ("object" === x.type(r)) {
        o = !0;for (l in r) x.access(e, n, l, r[l], !0, a, s);
      } else if (i !== t && (o = !0, x.isFunction(i) || (s = !0), c && (s ? (n.call(e, i), n = null) : (c = n, n = function (e, t, n) {
        return c.call(x(e), n);
      })), n)) for (; u > l; l++) n(e[l], r, s ? i : i.call(e[l], l, n(e[l], r)));return o ? e : c ? n.call(e) : u ? n(e[0], r) : a;
    }, now: function now() {
      return new Date().getTime();
    }, swap: function swap(e, t, n, r) {
      var i,
          o,
          a = {};for (o in t) a[o] = e.style[o], e.style[o] = t[o];i = n.apply(e, r || []);for (o in t) e.style[o] = a[o];return i;
    } }), x.ready.promise = function (t) {
    if (!n) if ((n = x.Deferred(), "complete" === a.readyState)) setTimeout(x.ready);else if (a.addEventListener) a.addEventListener("DOMContentLoaded", q, !1), e.addEventListener("load", q, !1);else {
      a.attachEvent("onreadystatechange", q), e.attachEvent("onload", q);var r = !1;try {
        r = null == e.frameElement && a.documentElement;
      } catch (i) {}r && r.doScroll && (function o() {
        if (!x.isReady) {
          try {
            r.doScroll("left");
          } catch (e) {
            return setTimeout(o, 50);
          }_(), x.ready();
        }
      })();
    }return n.promise(t);
  }, x.each("Boolean Number String Function Array Date RegExp Object Error".split(" "), function (e, t) {
    c["[object " + t + "]"] = t.toLowerCase();
  });function M(e) {
    var t = e.length,
        n = x.type(e);return x.isWindow(e) ? !1 : 1 === e.nodeType && t ? !0 : "array" === n || "function" !== n && (0 === t || "number" == typeof t && t > 0 && t - 1 in e);
  }r = x(a), (function (e, t) {
    var n,
        r,
        i,
        o,
        a,
        s,
        l,
        u,
        c,
        p,
        f,
        d,
        h,
        g,
        m,
        y,
        v,
        b = "sizzle" + -new Date(),
        w = e.document,
        T = 0,
        C = 0,
        N = st(),
        k = st(),
        E = st(),
        S = !1,
        A = function A(e, t) {
      return e === t ? (S = !0, 0) : 0;
    },
        j = typeof t,
        D = 1 << 31,
        L = ({}).hasOwnProperty,
        H = [],
        q = H.pop,
        _ = H.push,
        M = H.push,
        O = H.slice,
        F = H.indexOf || function (e) {
      var t = 0,
          n = this.length;for (; n > t; t++) if (this[t] === e) return t;return -1;
    },
        B = "checked|selected|async|autofocus|autoplay|controls|defer|disabled|hidden|ismap|loop|multiple|open|readonly|required|scoped",
        P = "[\\x20\\t\\r\\n\\f]",
        R = "(?:\\\\.|[\\w-]|[^\\x00-\\xa0])+",
        W = R.replace("w", "w#"),
        $ = "\\[" + P + "*(" + R + ")" + P + "*(?:([*^$|!~]?=)" + P + "*(?:(['\"])((?:\\\\.|[^\\\\])*?)\\3|(" + W + ")|)|)" + P + "*\\]",
        I = ":(" + R + ")(?:\\(((['\"])((?:\\\\.|[^\\\\])*?)\\3|((?:\\\\.|[^\\\\()[\\]]|" + $.replace(3, 8) + ")*)|.*)\\)|)",
        z = RegExp("^" + P + "+|((?:^|[^\\\\])(?:\\\\.)*)" + P + "+$", "g"),
        X = RegExp("^" + P + "*," + P + "*"),
        U = RegExp("^" + P + "*([>+~]|" + P + ")" + P + "*"),
        V = RegExp(P + "*[+~]"),
        Y = RegExp("=" + P + "*([^\\]'\"]*)" + P + "*\\]", "g"),
        J = RegExp(I),
        G = RegExp("^" + W + "$"),
        Q = { ID: RegExp("^#(" + R + ")"), CLASS: RegExp("^\\.(" + R + ")"), TAG: RegExp("^(" + R.replace("w", "w*") + ")"), ATTR: RegExp("^" + $), PSEUDO: RegExp("^" + I), CHILD: RegExp("^:(only|first|last|nth|nth-last)-(child|of-type)(?:\\(" + P + "*(even|odd|(([+-]|)(\\d*)n|)" + P + "*(?:([+-]|)" + P + "*(\\d+)|))" + P + "*\\)|)", "i"), bool: RegExp("^(?:" + B + ")$", "i"), needsContext: RegExp("^" + P + "*[>+~]|:(even|odd|eq|gt|lt|nth|first|last)(?:\\(" + P + "*((?:-\\d)?\\d*)" + P + "*\\)|)(?=[^-]|$)", "i") },
        K = /^[^{]+\{\s*\[native \w/,
        Z = /^(?:#([\w-]+)|(\w+)|\.([\w-]+))$/,
        et = /^(?:input|select|textarea|button)$/i,
        tt = /^h\d$/i,
        nt = /'|\\/g,
        rt = RegExp("\\\\([\\da-f]{1,6}" + P + "?|(" + P + ")|.)", "ig"),
        it = function it(e, t, n) {
      var r = "0x" + t - 65536;return r !== r || n ? t : 0 > r ? String.fromCharCode(r + 65536) : String.fromCharCode(55296 | r >> 10, 56320 | 1023 & r);
    };try {
      M.apply(H = O.call(w.childNodes), w.childNodes), H[w.childNodes.length].nodeType;
    } catch (ot) {
      M = { apply: H.length ? function (e, t) {
          _.apply(e, O.call(t));
        } : function (e, t) {
          var n = e.length,
              r = 0;while (e[n++] = t[r++]);e.length = n - 1;
        } };
    }function at(e, t, n, i) {
      var o, a, s, l, u, c, d, m, y, x;if (((t ? t.ownerDocument || t : w) !== f && p(t), t = t || f, n = n || [], !e || "string" != typeof e)) {
        return n;
      }if (1 !== (l = t.nodeType) && 9 !== l) {
        return [];
      }if (h && !i) {
        if (o = Z.exec(e)) if (s = o[1]) {
          if (9 === l) {
            if ((a = t.getElementById(s), !a || !a.parentNode)) {
              return n;
            }if (a.id === s) {
              return (n.push(a), n);
            }
          } else if (t.ownerDocument && (a = t.ownerDocument.getElementById(s)) && v(t, a) && a.id === s) {
            return (n.push(a), n);
          }
        } else {
          if (o[2]) {
            return (M.apply(n, t.getElementsByTagName(e)), n);
          }if ((s = o[3]) && r.getElementsByClassName && t.getElementsByClassName) {
            return (M.apply(n, t.getElementsByClassName(s)), n);
          }
        }if (r.qsa && (!g || !g.test(e))) {
          if ((m = d = b, y = t, x = 9 === l && e, 1 === l && "object" !== t.nodeName.toLowerCase())) {
            c = mt(e), (d = t.getAttribute("id")) ? m = d.replace(nt, "\\$&") : t.setAttribute("id", m), m = "[id='" + m + "'] ", u = c.length;while (u--) c[u] = m + yt(c[u]);y = V.test(e) && t.parentNode || t, x = c.join(",");
          }if (x) try {
            return (M.apply(n, y.querySelectorAll(x)), n);
          } catch (T) {} finally {
            d || t.removeAttribute("id");
          }
        }
      }return kt(e.replace(z, "$1"), t, n, i);
    }function st() {
      var e = [];function t(n, r) {
        return (e.push(n += " ") > o.cacheLength && delete t[e.shift()], t[n] = r);
      }return t;
    }function lt(e) {
      return (e[b] = !0, e);
    }function ut(e) {
      var t = f.createElement("div");try {
        return !!e(t);
      } catch (n) {
        return !1;
      } finally {
        t.parentNode && t.parentNode.removeChild(t), t = null;
      }
    }function ct(e, t) {
      var n = e.split("|"),
          r = e.length;while (r--) o.attrHandle[n[r]] = t;
    }function pt(e, t) {
      var n = t && e,
          r = n && 1 === e.nodeType && 1 === t.nodeType && (~t.sourceIndex || D) - (~e.sourceIndex || D);if (r) {
        return r;
      }if (n) while (n = n.nextSibling) if (n === t) {
        return -1;
      }return e ? 1 : -1;
    }function ft(e) {
      return function (t) {
        var n = t.nodeName.toLowerCase();return "input" === n && t.type === e;
      };
    }function dt(e) {
      return function (t) {
        var n = t.nodeName.toLowerCase();return ("input" === n || "button" === n) && t.type === e;
      };
    }function ht(e) {
      return lt(function (t) {
        return (t = +t, lt(function (n, r) {
          var i,
              o = e([], n.length, t),
              a = o.length;while (a--) n[i = o[a]] && (n[i] = !(r[i] = n[i]));
        }));
      });
    }s = at.isXML = function (e) {
      var t = e && (e.ownerDocument || e).documentElement;return t ? "HTML" !== t.nodeName : !1;
    }, r = at.support = {}, p = at.setDocument = function (e) {
      var n = e ? e.ownerDocument || e : w,
          i = n.defaultView;return n !== f && 9 === n.nodeType && n.documentElement ? (f = n, d = n.documentElement, h = !s(n), i && i.attachEvent && i !== i.top && i.attachEvent("onbeforeunload", function () {
        p();
      }), r.attributes = ut(function (e) {
        return (e.className = "i", !e.getAttribute("className"));
      }), r.getElementsByTagName = ut(function (e) {
        return (e.appendChild(n.createComment("")), !e.getElementsByTagName("*").length);
      }), r.getElementsByClassName = ut(function (e) {
        return (e.innerHTML = "<div class='a'></div><div class='a i'></div>", e.firstChild.className = "i", 2 === e.getElementsByClassName("i").length);
      }), r.getById = ut(function (e) {
        return (d.appendChild(e).id = b, !n.getElementsByName || !n.getElementsByName(b).length);
      }), r.getById ? (o.find.ID = function (e, t) {
        if (typeof t.getElementById !== j && h) {
          var n = t.getElementById(e);return n && n.parentNode ? [n] : [];
        }
      }, o.filter.ID = function (e) {
        var t = e.replace(rt, it);return function (e) {
          return e.getAttribute("id") === t;
        };
      }) : (delete o.find.ID, o.filter.ID = function (e) {
        var t = e.replace(rt, it);return function (e) {
          var n = typeof e.getAttributeNode !== j && e.getAttributeNode("id");return n && n.value === t;
        };
      }), o.find.TAG = r.getElementsByTagName ? function (e, n) {
        return typeof n.getElementsByTagName !== j ? n.getElementsByTagName(e) : t;
      } : function (e, t) {
        var n,
            r = [],
            i = 0,
            o = t.getElementsByTagName(e);if ("*" === e) {
          while (n = o[i++]) 1 === n.nodeType && r.push(n);return r;
        }return o;
      }, o.find.CLASS = r.getElementsByClassName && function (e, n) {
        return typeof n.getElementsByClassName !== j && h ? n.getElementsByClassName(e) : t;
      }, m = [], g = [], (r.qsa = K.test(n.querySelectorAll)) && (ut(function (e) {
        e.innerHTML = "<select><option selected=''></option></select>", e.querySelectorAll("[selected]").length || g.push("\\[" + P + "*(?:value|" + B + ")"), e.querySelectorAll(":checked").length || g.push(":checked");
      }), ut(function (e) {
        var t = n.createElement("input");t.setAttribute("type", "hidden"), e.appendChild(t).setAttribute("t", ""), e.querySelectorAll("[t^='']").length && g.push("[*^$]=" + P + "*(?:''|\"\")"), e.querySelectorAll(":enabled").length || g.push(":enabled", ":disabled"), e.querySelectorAll("*,:x"), g.push(",.*:");
      })), (r.matchesSelector = K.test(y = d.webkitMatchesSelector || d.mozMatchesSelector || d.oMatchesSelector || d.msMatchesSelector)) && ut(function (e) {
        r.disconnectedMatch = y.call(e, "div"), y.call(e, "[s!='']:x"), m.push("!=", I);
      }), g = g.length && RegExp(g.join("|")), m = m.length && RegExp(m.join("|")), v = K.test(d.contains) || d.compareDocumentPosition ? function (e, t) {
        var n = 9 === e.nodeType ? e.documentElement : e,
            r = t && t.parentNode;return e === r || !(!r || 1 !== r.nodeType || !(n.contains ? n.contains(r) : e.compareDocumentPosition && 16 & e.compareDocumentPosition(r)));
      } : function (e, t) {
        if (t) while (t = t.parentNode) if (t === e) return !0;return !1;
      }, A = d.compareDocumentPosition ? function (e, t) {
        if (e === t) return (S = !0, 0);var i = t.compareDocumentPosition && e.compareDocumentPosition && e.compareDocumentPosition(t);return i ? 1 & i || !r.sortDetached && t.compareDocumentPosition(e) === i ? e === n || v(w, e) ? -1 : t === n || v(w, t) ? 1 : c ? F.call(c, e) - F.call(c, t) : 0 : 4 & i ? -1 : 1 : e.compareDocumentPosition ? -1 : 1;
      } : function (e, t) {
        var r,
            i = 0,
            o = e.parentNode,
            a = t.parentNode,
            s = [e],
            l = [t];if (e === t) return (S = !0, 0);if (!o || !a) return e === n ? -1 : t === n ? 1 : o ? -1 : a ? 1 : c ? F.call(c, e) - F.call(c, t) : 0;if (o === a) return pt(e, t);r = e;while (r = r.parentNode) s.unshift(r);r = t;while (r = r.parentNode) l.unshift(r);while (s[i] === l[i]) i++;return i ? pt(s[i], l[i]) : s[i] === w ? -1 : l[i] === w ? 1 : 0;
      }, n) : f;
    }, at.matches = function (e, t) {
      return at(e, null, null, t);
    }, at.matchesSelector = function (e, t) {
      if (((e.ownerDocument || e) !== f && p(e), t = t.replace(Y, "='$1']"), !(!r.matchesSelector || !h || m && m.test(t) || g && g.test(t)))) try {
        var n = y.call(e, t);if (n || r.disconnectedMatch || e.document && 11 !== e.document.nodeType) return n;
      } catch (i) {}return at(t, f, null, [e]).length > 0;
    }, at.contains = function (e, t) {
      return ((e.ownerDocument || e) !== f && p(e), v(e, t));
    }, at.attr = function (e, n) {
      (e.ownerDocument || e) !== f && p(e);var i = o.attrHandle[n.toLowerCase()],
          a = i && L.call(o.attrHandle, n.toLowerCase()) ? i(e, n, !h) : t;return a === t ? r.attributes || !h ? e.getAttribute(n) : (a = e.getAttributeNode(n)) && a.specified ? a.value : null : a;
    }, at.error = function (e) {
      throw Error("Syntax error, unrecognized expression: " + e);
    }, at.uniqueSort = function (e) {
      var t,
          n = [],
          i = 0,
          o = 0;if ((S = !r.detectDuplicates, c = !r.sortStable && e.slice(0), e.sort(A), S)) {
        while (t = e[o++]) t === e[o] && (i = n.push(o));while (i--) e.splice(n[i], 1);
      }return e;
    }, a = at.getText = function (e) {
      var t,
          n = "",
          r = 0,
          i = e.nodeType;if (i) {
        if (1 === i || 9 === i || 11 === i) {
          if ("string" == typeof e.textContent) return e.textContent;for (e = e.firstChild; e; e = e.nextSibling) n += a(e);
        } else if (3 === i || 4 === i) return e.nodeValue;
      } else for (; t = e[r]; r++) n += a(t);return n;
    }, o = at.selectors = { cacheLength: 50, createPseudo: lt, match: Q, attrHandle: {}, find: {}, relative: { ">": { dir: "parentNode", first: !0 }, " ": { dir: "parentNode" }, "+": { dir: "previousSibling", first: !0 }, "~": { dir: "previousSibling" } }, preFilter: { ATTR: function ATTR(e) {
          return (e[1] = e[1].replace(rt, it), e[3] = (e[4] || e[5] || "").replace(rt, it), "~=" === e[2] && (e[3] = " " + e[3] + " "), e.slice(0, 4));
        }, CHILD: function CHILD(e) {
          return (e[1] = e[1].toLowerCase(), "nth" === e[1].slice(0, 3) ? (e[3] || at.error(e[0]), e[4] = +(e[4] ? e[5] + (e[6] || 1) : 2 * ("even" === e[3] || "odd" === e[3])), e[5] = +(e[7] + e[8] || "odd" === e[3])) : e[3] && at.error(e[0]), e);
        }, PSEUDO: function PSEUDO(e) {
          var n,
              r = !e[5] && e[2];return Q.CHILD.test(e[0]) ? null : (e[3] && e[4] !== t ? e[2] = e[4] : r && J.test(r) && (n = mt(r, !0)) && (n = r.indexOf(")", r.length - n) - r.length) && (e[0] = e[0].slice(0, n), e[2] = r.slice(0, n)), e.slice(0, 3));
        } }, filter: { TAG: function TAG(e) {
          var t = e.replace(rt, it).toLowerCase();return "*" === e ? function () {
            return !0;
          } : function (e) {
            return e.nodeName && e.nodeName.toLowerCase() === t;
          };
        }, CLASS: function CLASS(e) {
          var t = N[e + " "];return t || (t = RegExp("(^|" + P + ")" + e + "(" + P + "|$)")) && N(e, function (e) {
            return t.test("string" == typeof e.className && e.className || typeof e.getAttribute !== j && e.getAttribute("class") || "");
          });
        }, ATTR: function ATTR(e, t, n) {
          return function (r) {
            var i = at.attr(r, e);return null == i ? "!=" === t : t ? (i += "", "=" === t ? i === n : "!=" === t ? i !== n : "^=" === t ? n && 0 === i.indexOf(n) : "*=" === t ? n && i.indexOf(n) > -1 : "$=" === t ? n && i.slice(-n.length) === n : "~=" === t ? (" " + i + " ").indexOf(n) > -1 : "|=" === t ? i === n || i.slice(0, n.length + 1) === n + "-" : !1) : !0;
          };
        }, CHILD: function CHILD(e, t, n, r, i) {
          var o = "nth" !== e.slice(0, 3),
              a = "last" !== e.slice(-4),
              s = "of-type" === t;return 1 === r && 0 === i ? function (e) {
            return !!e.parentNode;
          } : function (t, n, l) {
            var u,
                c,
                p,
                f,
                d,
                h,
                g = o !== a ? "nextSibling" : "previousSibling",
                m = t.parentNode,
                y = s && t.nodeName.toLowerCase(),
                v = !l && !s;if (m) {
              if (o) {
                while (g) {
                  p = t;while (p = p[g]) if (s ? p.nodeName.toLowerCase() === y : 1 === p.nodeType) return !1;h = g = "only" === e && !h && "nextSibling";
                }return !0;
              }if ((h = [a ? m.firstChild : m.lastChild], a && v)) {
                c = m[b] || (m[b] = {}), u = c[e] || [], d = u[0] === T && u[1], f = u[0] === T && u[2], p = d && m.childNodes[d];while (p = ++d && p && p[g] || (f = d = 0) || h.pop()) if (1 === p.nodeType && ++f && p === t) {
                  c[e] = [T, d, f];break;
                }
              } else if (v && (u = (t[b] || (t[b] = {}))[e]) && u[0] === T) f = u[1];else while (p = ++d && p && p[g] || (f = d = 0) || h.pop()) if ((s ? p.nodeName.toLowerCase() === y : 1 === p.nodeType) && ++f && (v && ((p[b] || (p[b] = {}))[e] = [T, f]), p === t)) break;return (f -= i, f === r || 0 === f % r && f / r >= 0);
            }
          };
        }, PSEUDO: function PSEUDO(e, t) {
          var n,
              r = o.pseudos[e] || o.setFilters[e.toLowerCase()] || at.error("unsupported pseudo: " + e);return r[b] ? r(t) : r.length > 1 ? (n = [e, e, "", t], o.setFilters.hasOwnProperty(e.toLowerCase()) ? lt(function (e, n) {
            var i,
                o = r(e, t),
                a = o.length;while (a--) i = F.call(e, o[a]), e[i] = !(n[i] = o[a]);
          }) : function (e) {
            return r(e, 0, n);
          }) : r;
        } }, pseudos: { not: lt(function (e) {
          var t = [],
              n = [],
              r = l(e.replace(z, "$1"));return r[b] ? lt(function (e, t, n, i) {
            var o,
                a = r(e, null, i, []),
                s = e.length;while (s--) (o = a[s]) && (e[s] = !(t[s] = o));
          }) : function (e, i, o) {
            return (t[0] = e, r(t, null, o, n), !n.pop());
          };
        }), has: lt(function (e) {
          return function (t) {
            return at(e, t).length > 0;
          };
        }), contains: lt(function (e) {
          return function (t) {
            return (t.textContent || t.innerText || a(t)).indexOf(e) > -1;
          };
        }), lang: lt(function (e) {
          return (G.test(e || "") || at.error("unsupported lang: " + e), e = e.replace(rt, it).toLowerCase(), function (t) {
            var n;do if (n = h ? t.lang : t.getAttribute("xml:lang") || t.getAttribute("lang")) return (n = n.toLowerCase(), n === e || 0 === n.indexOf(e + "-")); while ((t = t.parentNode) && 1 === t.nodeType);return !1;
          });
        }), target: function target(t) {
          var n = e.location && e.location.hash;return n && n.slice(1) === t.id;
        }, root: function root(e) {
          return e === d;
        }, focus: function focus(e) {
          return e === f.activeElement && (!f.hasFocus || f.hasFocus()) && !!(e.type || e.href || ~e.tabIndex);
        }, enabled: function enabled(e) {
          return e.disabled === !1;
        }, disabled: function disabled(e) {
          return e.disabled === !0;
        }, checked: function checked(e) {
          var t = e.nodeName.toLowerCase();return "input" === t && !!e.checked || "option" === t && !!e.selected;
        }, selected: function selected(e) {
          return (e.parentNode && e.parentNode.selectedIndex, e.selected === !0);
        }, empty: function empty(e) {
          for (e = e.firstChild; e; e = e.nextSibling) if (e.nodeName > "@" || 3 === e.nodeType || 4 === e.nodeType) {
            return !1;
          }return !0;
        }, parent: function parent(e) {
          return !o.pseudos.empty(e);
        }, header: function header(e) {
          return tt.test(e.nodeName);
        }, input: function input(e) {
          return et.test(e.nodeName);
        }, button: function button(e) {
          var t = e.nodeName.toLowerCase();return "input" === t && "button" === e.type || "button" === t;
        }, text: function text(e) {
          var t;return "input" === e.nodeName.toLowerCase() && "text" === e.type && (null == (t = e.getAttribute("type")) || t.toLowerCase() === e.type);
        }, first: ht(function () {
          return [0];
        }), last: ht(function (e, t) {
          return [t - 1];
        }), eq: ht(function (e, t, n) {
          return [0 > n ? n + t : n];
        }), even: ht(function (e, t) {
          var n = 0;for (; t > n; n += 2) e.push(n);return e;
        }), odd: ht(function (e, t) {
          var n = 1;for (; t > n; n += 2) e.push(n);return e;
        }), lt: ht(function (e, t, n) {
          var r = 0 > n ? n + t : n;for (; --r >= 0;) e.push(r);return e;
        }), gt: ht(function (e, t, n) {
          var r = 0 > n ? n + t : n;for (; t > ++r;) e.push(r);return e;
        }) } }, o.pseudos.nth = o.pseudos.eq;for (n in { radio: !0, checkbox: !0, file: !0, password: !0, image: !0 }) o.pseudos[n] = ft(n);for (n in { submit: !0, reset: !0 }) o.pseudos[n] = dt(n);function gt() {}gt.prototype = o.filters = o.pseudos, o.setFilters = new gt();function mt(e, t) {
      var n,
          r,
          i,
          a,
          s,
          l,
          u,
          c = k[e + " "];if (c) {
        return t ? 0 : c.slice(0);
      }s = e, l = [], u = o.preFilter;while (s) {
        (!n || (r = X.exec(s))) && (r && (s = s.slice(r[0].length) || s), l.push(i = [])), n = !1, (r = U.exec(s)) && (n = r.shift(), i.push({ value: n, type: r[0].replace(z, " ") }), s = s.slice(n.length));for (a in o.filter) !(r = Q[a].exec(s)) || u[a] && !(r = u[a](r)) || (n = r.shift(), i.push({ value: n, type: a, matches: r }), s = s.slice(n.length));if (!n) break;
      }return t ? s.length : s ? at.error(e) : k(e, l).slice(0);
    }function yt(e) {
      var t = 0,
          n = e.length,
          r = "";for (; n > t; t++) r += e[t].value;return r;
    }function vt(e, t, n) {
      var r = t.dir,
          o = n && "parentNode" === r,
          a = C++;return t.first ? function (t, n, i) {
        while (t = t[r]) if (1 === t.nodeType || o) return e(t, n, i);
      } : function (t, n, s) {
        var l,
            u,
            c,
            p = T + " " + a;if (s) {
          while (t = t[r]) if ((1 === t.nodeType || o) && e(t, n, s)) return !0;
        } else while (t = t[r]) if (1 === t.nodeType || o) if ((c = t[b] || (t[b] = {}), (u = c[r]) && u[0] === p)) {
          if ((l = u[1]) === !0 || l === i) return l === !0;
        } else if ((u = c[r] = [p], u[1] = e(t, n, s) || i, u[1] === !0)) return !0;
      };
    }function bt(e) {
      return e.length > 1 ? function (t, n, r) {
        var i = e.length;while (i--) if (!e[i](t, n, r)) return !1;return !0;
      } : e[0];
    }function xt(e, t, n, r, i) {
      var o,
          a = [],
          s = 0,
          l = e.length,
          u = null != t;for (; l > s; s++) (o = e[s]) && (!n || n(o, r, i)) && (a.push(o), u && t.push(s));return a;
    }function wt(e, t, n, r, i, o) {
      return (r && !r[b] && (r = wt(r)), i && !i[b] && (i = wt(i, o)), lt(function (o, a, s, l) {
        var u,
            c,
            p,
            f = [],
            d = [],
            h = a.length,
            g = o || Nt(t || "*", s.nodeType ? [s] : s, []),
            m = !e || !o && t ? g : xt(g, f, e, s, l),
            y = n ? i || (o ? e : h || r) ? [] : a : m;if ((n && n(m, y, s, l), r)) {
          u = xt(y, d), r(u, [], s, l), c = u.length;while (c--) (p = u[c]) && (y[d[c]] = !(m[d[c]] = p));
        }if (o) {
          if (i || e) {
            if (i) {
              u = [], c = y.length;while (c--) (p = y[c]) && u.push(m[c] = p);i(null, y = [], u, l);
            }c = y.length;while (c--) (p = y[c]) && (u = i ? F.call(o, p) : f[c]) > -1 && (o[u] = !(a[u] = p));
          }
        } else y = xt(y === a ? y.splice(h, y.length) : y), i ? i(null, a, y, l) : M.apply(a, y);
      }));
    }function Tt(e) {
      var t,
          n,
          r,
          i = e.length,
          a = o.relative[e[0].type],
          s = a || o.relative[" "],
          l = a ? 1 : 0,
          c = vt(function (e) {
        return e === t;
      }, s, !0),
          p = vt(function (e) {
        return F.call(t, e) > -1;
      }, s, !0),
          f = [function (e, n, r) {
        return !a && (r || n !== u) || ((t = n).nodeType ? c(e, n, r) : p(e, n, r));
      }];for (; i > l; l++) if (n = o.relative[e[l].type]) f = [vt(bt(f), n)];else {
        if ((n = o.filter[e[l].type].apply(null, e[l].matches), n[b])) {
          for (r = ++l; i > r; r++) if (o.relative[e[r].type]) break;return wt(l > 1 && bt(f), l > 1 && yt(e.slice(0, l - 1).concat({ value: " " === e[l - 2].type ? "*" : "" })).replace(z, "$1"), n, r > l && Tt(e.slice(l, r)), i > r && Tt(e = e.slice(r)), i > r && yt(e));
        }f.push(n);
      }return bt(f);
    }function Ct(e, t) {
      var n = 0,
          r = t.length > 0,
          a = e.length > 0,
          s = (function (_s) {
        var _sWrapper = function s(_x, _x2, _x3, _x4, _x5) {
          return _s.apply(this, arguments);
        };

        _sWrapper.toString = function () {
          return _s.toString();
        };

        return _sWrapper;
      })(function (s, l, c, p, d) {
        var h,
            g,
            m,
            y = [],
            v = 0,
            b = "0",
            x = s && [],
            w = null != d,
            C = u,
            N = s || a && o.find.TAG("*", d && l.parentNode || l),
            k = T += null == C ? 1 : Math.random() || 0.1;for (w && (u = l !== f && l, i = n); null != (h = N[b]); b++) {
          if (a && h) {
            g = 0;while (m = e[g++]) if (m(h, l, c)) {
              p.push(h);break;
            }w && (T = k, i = ++n);
          }r && ((h = !m && h) && v--, s && x.push(h));
        }if ((v += b, r && b !== v)) {
          g = 0;while (m = t[g++]) m(x, y, l, c);if (s) {
            if (v > 0) while (b--) x[b] || y[b] || (y[b] = q.call(p));y = xt(y);
          }M.apply(p, y), w && !s && y.length > 0 && v + t.length > 1 && at.uniqueSort(p);
        }return (w && (T = k, u = C), x);
      });return r ? lt(s) : s;
    }l = at.compile = function (e, t) {
      var n,
          r = [],
          i = [],
          o = E[e + " "];if (!o) {
        t || (t = mt(e)), n = t.length;while (n--) o = Tt(t[n]), o[b] ? r.push(o) : i.push(o);o = E(e, Ct(i, r));
      }return o;
    };function Nt(e, t, n) {
      var r = 0,
          i = t.length;for (; i > r; r++) at(e, t[r], n);return n;
    }function kt(e, t, n, i) {
      var a,
          s,
          u,
          c,
          p,
          f = mt(e);if (!i && 1 === f.length) {
        if ((s = f[0] = f[0].slice(0), s.length > 2 && "ID" === (u = s[0]).type && r.getById && 9 === t.nodeType && h && o.relative[s[1].type])) {
          if ((t = (o.find.ID(u.matches[0].replace(rt, it), t) || [])[0], !t)) {
            return n;
          }e = e.slice(s.shift().value.length);
        }a = Q.needsContext.test(e) ? 0 : s.length;while (a--) {
          if ((u = s[a], o.relative[c = u.type])) break;if ((p = o.find[c]) && (i = p(u.matches[0].replace(rt, it), V.test(s[0].type) && t.parentNode || t))) {
            if ((s.splice(a, 1), e = i.length && yt(s), !e)) {
              return (M.apply(n, i), n);
            }break;
          }
        }
      }return (l(e, f)(i, t, !h, n, V.test(e)), n);
    }r.sortStable = b.split("").sort(A).join("") === b, r.detectDuplicates = S, p(), r.sortDetached = ut(function (e) {
      return 1 & e.compareDocumentPosition(f.createElement("div"));
    }), ut(function (e) {
      return (e.innerHTML = "<a href='#'></a>", "#" === e.firstChild.getAttribute("href"));
    }) || ct("type|href|height|width", function (e, n, r) {
      return r ? t : e.getAttribute(n, "type" === n.toLowerCase() ? 1 : 2);
    }), r.attributes && ut(function (e) {
      return (e.innerHTML = "<input/>", e.firstChild.setAttribute("value", ""), "" === e.firstChild.getAttribute("value"));
    }) || ct("value", function (e, n, r) {
      return r || "input" !== e.nodeName.toLowerCase() ? t : e.defaultValue;
    }), ut(function (e) {
      return null == e.getAttribute("disabled");
    }) || ct(B, function (e, n, r) {
      var i;return r ? t : (i = e.getAttributeNode(n)) && i.specified ? i.value : e[n] === !0 ? n.toLowerCase() : null;
    }), x.find = at, x.expr = at.selectors, x.expr[":"] = x.expr.pseudos, x.unique = at.uniqueSort, x.text = at.getText, x.isXMLDoc = at.isXML, x.contains = at.contains;
  })(e);var O = {};function F(e) {
    var t = O[e] = {};return (x.each(e.match(T) || [], function (e, n) {
      t[n] = !0;
    }), t);
  }x.Callbacks = function (e) {
    e = "string" == typeof e ? O[e] || F(e) : x.extend({}, e);var n,
        r,
        i,
        o,
        a,
        s,
        l = [],
        u = !e.once && [],
        c = (function (_c) {
      var _cWrapper = function c(_x) {
        return _c.apply(this, arguments);
      };

      _cWrapper.toString = function () {
        return _c.toString();
      };

      return _cWrapper;
    })(function (t) {
      for (r = e.memory && t, i = !0, a = s || 0, s = 0, o = l.length, n = !0; l && o > a; a++) if (l[a].apply(t[0], t[1]) === !1 && e.stopOnFalse) {
        r = !1;break;
      }n = !1, l && (u ? u.length && c(u.shift()) : r ? l = [] : p.disable());
    }),
        p = { add: function add() {
        if (l) {
          var t = l.length;(function i(t) {
            x.each(t, function (t, n) {
              var r = x.type(n);"function" === r ? e.unique && p.has(n) || l.push(n) : n && n.length && "string" !== r && i(n);
            });
          })(arguments), n ? o = l.length : r && (s = t, c(r));
        }return this;
      }, remove: function remove() {
        return (l && x.each(arguments, function (e, t) {
          var r;while ((r = x.inArray(t, l, r)) > -1) l.splice(r, 1), n && (o >= r && o--, a >= r && a--);
        }), this);
      }, has: function has(e) {
        return e ? x.inArray(e, l) > -1 : !(!l || !l.length);
      }, empty: function empty() {
        return (l = [], o = 0, this);
      }, disable: function disable() {
        return (l = u = r = t, this);
      }, disabled: function disabled() {
        return !l;
      }, lock: function lock() {
        return (u = t, r || p.disable(), this);
      }, locked: function locked() {
        return !u;
      }, fireWith: function fireWith(e, t) {
        return (!l || i && !u || (t = t || [], t = [e, t.slice ? t.slice() : t], n ? u.push(t) : c(t)), this);
      }, fire: function fire() {
        return (p.fireWith(this, arguments), this);
      }, fired: function fired() {
        return !!i;
      } };return p;
  }, x.extend({ Deferred: function Deferred(e) {
      var t = [["resolve", "done", x.Callbacks("once memory"), "resolved"], ["reject", "fail", x.Callbacks("once memory"), "rejected"], ["notify", "progress", x.Callbacks("memory")]],
          n = "pending",
          r = { state: function state() {
          return n;
        }, always: function always() {
          return (i.done(arguments).fail(arguments), this);
        }, then: function then() {
          var e = arguments;return x.Deferred(function (n) {
            x.each(t, function (t, o) {
              var a = o[0],
                  s = x.isFunction(e[t]) && e[t];i[o[1]](function () {
                var e = s && s.apply(this, arguments);e && x.isFunction(e.promise) ? e.promise().done(n.resolve).fail(n.reject).progress(n.notify) : n[a + "With"](this === r ? n.promise() : this, s ? [e] : arguments);
              });
            }), e = null;
          }).promise();
        }, promise: function promise(e) {
          return null != e ? x.extend(e, r) : r;
        } },
          i = {};return (r.pipe = r.then, x.each(t, function (e, o) {
        var a = o[2],
            s = o[3];r[o[1]] = a.add, s && a.add(function () {
          n = s;
        }, t[1 ^ e][2].disable, t[2][2].lock), i[o[0]] = function () {
          return (i[o[0] + "With"](this === i ? r : this, arguments), this);
        }, i[o[0] + "With"] = a.fireWith;
      }), r.promise(i), e && e.call(i, i), i);
    }, when: function when(e) {
      var t = 0,
          n = g.call(arguments),
          r = n.length,
          i = 1 !== r || e && x.isFunction(e.promise) ? r : 0,
          o = 1 === i ? e : x.Deferred(),
          a = function a(e, t, n) {
        return function (r) {
          t[e] = this, n[e] = arguments.length > 1 ? g.call(arguments) : r, n === s ? o.notifyWith(t, n) : --i || o.resolveWith(t, n);
        };
      },
          s,
          l,
          u;if (r > 1) for (s = Array(r), l = Array(r), u = Array(r); r > t; t++) n[t] && x.isFunction(n[t].promise) ? n[t].promise().done(a(t, u, n)).fail(o.reject).progress(a(t, l, s)) : --i;return (i || o.resolveWith(u, n), o.promise());
    } }), x.support = (function (t) {
    var n,
        r,
        o,
        s,
        l,
        u,
        c,
        p,
        f,
        d = a.createElement("div");if ((d.setAttribute("className", "t"), d.innerHTML = "  <link/><table></table><a href='/a'>a</a><input type='checkbox'/>", n = d.getElementsByTagName("*") || [], r = d.getElementsByTagName("a")[0], !r || !r.style || !n.length)) return t;s = a.createElement("select"), u = s.appendChild(a.createElement("option")), o = d.getElementsByTagName("input")[0], r.style.cssText = "top:1px;float:left;opacity:.5", t.getSetAttribute = "t" !== d.className, t.leadingWhitespace = 3 === d.firstChild.nodeType, t.tbody = !d.getElementsByTagName("tbody").length, t.htmlSerialize = !!d.getElementsByTagName("link").length, t.style = /top/.test(r.getAttribute("style")), t.hrefNormalized = "/a" === r.getAttribute("href"), t.opacity = /^0.5/.test(r.style.opacity), t.cssFloat = !!r.style.cssFloat, t.checkOn = !!o.value, t.optSelected = u.selected, t.enctype = !!a.createElement("form").enctype, t.html5Clone = "<:nav></:nav>" !== a.createElement("nav").cloneNode(!0).outerHTML, t.inlineBlockNeedsLayout = !1, t.shrinkWrapBlocks = !1, t.pixelPosition = !1, t.deleteExpando = !0, t.noCloneEvent = !0, t.reliableMarginRight = !0, t.boxSizingReliable = !0, o.checked = !0, t.noCloneChecked = o.cloneNode(!0).checked, s.disabled = !0, t.optDisabled = !u.disabled;try {
      delete d.test;
    } catch (h) {
      t.deleteExpando = !1;
    }o = a.createElement("input"), o.setAttribute("value", ""), t.input = "" === o.getAttribute("value"), o.value = "t", o.setAttribute("type", "radio"), t.radioValue = "t" === o.value, o.setAttribute("checked", "t"), o.setAttribute("name", "t"), l = a.createDocumentFragment(), l.appendChild(o), t.appendChecked = o.checked, t.checkClone = l.cloneNode(!0).cloneNode(!0).lastChild.checked, d.attachEvent && (d.attachEvent("onclick", function () {
      t.noCloneEvent = !1;
    }), d.cloneNode(!0).click());for (f in { submit: !0, change: !0, focusin: !0 }) d.setAttribute(c = "on" + f, "t"), t[f + "Bubbles"] = c in e || d.attributes[c].expando === !1;d.style.backgroundClip = "content-box", d.cloneNode(!0).style.backgroundClip = "", t.clearCloneStyle = "content-box" === d.style.backgroundClip;for (f in x(t)) break;return (t.ownLast = "0" !== f, x(function () {
      var n,
          r,
          o,
          s = "padding:0;margin:0;border:0;display:block;box-sizing:content-box;-moz-box-sizing:content-box;-webkit-box-sizing:content-box;",
          l = a.getElementsByTagName("body")[0];l && (n = a.createElement("div"), n.style.cssText = "border:0;width:0;height:0;position:absolute;top:0;left:-9999px;margin-top:1px", l.appendChild(n).appendChild(d), d.innerHTML = "<table><tr><td></td><td>t</td></tr></table>", o = d.getElementsByTagName("td"), o[0].style.cssText = "padding:0;margin:0;border:0;display:none", p = 0 === o[0].offsetHeight, o[0].style.display = "", o[1].style.display = "none", t.reliableHiddenOffsets = p && 0 === o[0].offsetHeight, d.innerHTML = "", d.style.cssText = "box-sizing:border-box;-moz-box-sizing:border-box;-webkit-box-sizing:border-box;padding:1px;border:1px;display:block;width:4px;margin-top:1%;position:absolute;top:1%;", x.swap(l, null != l.style.zoom ? { zoom: 1 } : {}, function () {
        t.boxSizing = 4 === d.offsetWidth;
      }), e.getComputedStyle && (t.pixelPosition = "1%" !== (e.getComputedStyle(d, null) || {}).top, t.boxSizingReliable = "4px" === (e.getComputedStyle(d, null) || { width: "4px" }).width, r = d.appendChild(a.createElement("div")), r.style.cssText = d.style.cssText = s, r.style.marginRight = r.style.width = "0", d.style.width = "1px", t.reliableMarginRight = !parseFloat((e.getComputedStyle(r, null) || {}).marginRight)), typeof d.style.zoom !== i && (d.innerHTML = "", d.style.cssText = s + "width:1px;padding:1px;display:inline;zoom:1", t.inlineBlockNeedsLayout = 3 === d.offsetWidth, d.style.display = "block", d.innerHTML = "<div></div>", d.firstChild.style.width = "5px", t.shrinkWrapBlocks = 3 !== d.offsetWidth, t.inlineBlockNeedsLayout && (l.style.zoom = 1)), l.removeChild(n), n = d = o = r = null);
    }), n = s = l = u = r = o = null, t);
  })({});var B = /(?:\{[\s\S]*\}|\[[\s\S]*\])$/,
      P = /([A-Z])/g;function R(e, n, r, i) {
    if (x.acceptData(e)) {
      var o,
          a,
          s = x.expando,
          l = e.nodeType,
          u = l ? x.cache : e,
          c = l ? e[s] : e[s] && s;if (c && u[c] && (i || u[c].data) || r !== t || "string" != typeof n) {
        return (c || (c = l ? e[s] = p.pop() || x.guid++ : s), u[c] || (u[c] = l ? {} : { toJSON: x.noop }), ("object" == typeof n || "function" == typeof n) && (i ? u[c] = x.extend(u[c], n) : u[c].data = x.extend(u[c].data, n)), a = u[c], i || (a.data || (a.data = {}), a = a.data), r !== t && (a[x.camelCase(n)] = r), "string" == typeof n ? (o = a[n], null == o && (o = a[x.camelCase(n)])) : o = a, o);
      }
    }
  }function W(e, t, n) {
    if (x.acceptData(e)) {
      var r,
          i,
          o = e.nodeType,
          a = o ? x.cache : e,
          s = o ? e[x.expando] : x.expando;if (a[s]) {
        if (t && (r = n ? a[s] : a[s].data)) {
          x.isArray(t) ? t = t.concat(x.map(t, x.camelCase)) : t in r ? t = [t] : (t = x.camelCase(t), t = t in r ? [t] : t.split(" ")), i = t.length;while (i--) delete r[t[i]];if (n ? !I(r) : !x.isEmptyObject(r)) {
            return;
          }
        }(n || (delete a[s].data, I(a[s]))) && (o ? x.cleanData([e], !0) : x.support.deleteExpando || a != a.window ? delete a[s] : a[s] = null);
      }
    }
  }x.extend({ cache: {}, noData: { applet: !0, embed: !0, object: "clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" }, hasData: function hasData(e) {
      return (e = e.nodeType ? x.cache[e[x.expando]] : e[x.expando], !!e && !I(e));
    }, data: function data(e, t, n) {
      return R(e, t, n);
    }, removeData: function removeData(e, t) {
      return W(e, t);
    }, _data: function _data(e, t, n) {
      return R(e, t, n, !0);
    }, _removeData: function _removeData(e, t) {
      return W(e, t, !0);
    }, acceptData: function acceptData(e) {
      if (e.nodeType && 1 !== e.nodeType && 9 !== e.nodeType) {
        return !1;
      }var t = e.nodeName && x.noData[e.nodeName.toLowerCase()];return !t || t !== !0 && e.getAttribute("classid") === t;
    } }), x.fn.extend({ data: function data(e, n) {
      var r,
          i,
          o = null,
          a = 0,
          s = this[0];if (e === t) {
        if (this.length && (o = x.data(s), 1 === s.nodeType && !x._data(s, "parsedAttrs"))) {
          for (r = s.attributes; r.length > a; a++) i = r[a].name, 0 === i.indexOf("data-") && (i = x.camelCase(i.slice(5)), $(s, i, o[i]));x._data(s, "parsedAttrs", !0);
        }return o;
      }return "object" == typeof e ? this.each(function () {
        x.data(this, e);
      }) : arguments.length > 1 ? this.each(function () {
        x.data(this, e, n);
      }) : s ? $(s, e, x.data(s, e)) : null;
    }, removeData: function removeData(e) {
      return this.each(function () {
        x.removeData(this, e);
      });
    } });function $(e, n, r) {
    if (r === t && 1 === e.nodeType) {
      var i = "data-" + n.replace(P, "-$1").toLowerCase();if ((r = e.getAttribute(i), "string" == typeof r)) {
        try {
          r = "true" === r ? !0 : "false" === r ? !1 : "null" === r ? null : +r + "" === r ? +r : B.test(r) ? x.parseJSON(r) : r;
        } catch (o) {}x.data(e, n, r);
      } else r = t;
    }return r;
  }function I(e) {
    var t;for (t in e) if (("data" !== t || !x.isEmptyObject(e[t])) && "toJSON" !== t) {
      return !1;
    }return !0;
  }x.extend({ queue: function queue(e, n, r) {
      var i;return e ? (n = (n || "fx") + "queue", i = x._data(e, n), r && (!i || x.isArray(r) ? i = x._data(e, n, x.makeArray(r)) : i.push(r)), i || []) : t;
    }, dequeue: function dequeue(e, t) {
      t = t || "fx";var n = x.queue(e, t),
          r = n.length,
          i = n.shift(),
          o = x._queueHooks(e, t),
          a = function a() {
        x.dequeue(e, t);
      };"inprogress" === i && (i = n.shift(), r--), i && ("fx" === t && n.unshift("inprogress"), delete o.stop, i.call(e, a, o)), !r && o && o.empty.fire();
    }, _queueHooks: function _queueHooks(e, t) {
      var n = t + "queueHooks";return x._data(e, n) || x._data(e, n, { empty: x.Callbacks("once memory").add(function () {
          x._removeData(e, t + "queue"), x._removeData(e, n);
        }) });
    } }), x.fn.extend({ queue: function queue(e, n) {
      var r = 2;return ("string" != typeof e && (n = e, e = "fx", r--), r > arguments.length ? x.queue(this[0], e) : n === t ? this : this.each(function () {
        var t = x.queue(this, e, n);x._queueHooks(this, e), "fx" === e && "inprogress" !== t[0] && x.dequeue(this, e);
      }));
    }, dequeue: function dequeue(e) {
      return this.each(function () {
        x.dequeue(this, e);
      });
    }, delay: function delay(e, t) {
      return (e = x.fx ? x.fx.speeds[e] || e : e, t = t || "fx", this.queue(t, function (t, n) {
        var r = setTimeout(t, e);n.stop = function () {
          clearTimeout(r);
        };
      }));
    }, clearQueue: function clearQueue(e) {
      return this.queue(e || "fx", []);
    }, promise: function promise(e, n) {
      var r,
          i = 1,
          o = x.Deferred(),
          a = this,
          s = this.length,
          l = function l() {
        --i || o.resolveWith(a, [a]);
      };"string" != typeof e && (n = e, e = t), e = e || "fx";while (s--) r = x._data(a[s], e + "queueHooks"), r && r.empty && (i++, r.empty.add(l));return (l(), o.promise(n));
    } });var z,
      X,
      U = /[\t\r\n\f]/g,
      V = /\r/g,
      Y = /^(?:input|select|textarea|button|object)$/i,
      J = /^(?:a|area)$/i,
      G = /^(?:checked|selected)$/i,
      Q = x.support.getSetAttribute,
      K = x.support.input;x.fn.extend({ attr: function attr(e, t) {
      return x.access(this, x.attr, e, t, arguments.length > 1);
    }, removeAttr: function removeAttr(e) {
      return this.each(function () {
        x.removeAttr(this, e);
      });
    }, prop: function prop(e, t) {
      return x.access(this, x.prop, e, t, arguments.length > 1);
    }, removeProp: function removeProp(e) {
      return (e = x.propFix[e] || e, this.each(function () {
        try {
          this[e] = t, delete this[e];
        } catch (n) {}
      }));
    }, addClass: function addClass(e) {
      var t,
          n,
          r,
          i,
          o,
          a = 0,
          s = this.length,
          l = "string" == typeof e && e;if (x.isFunction(e)) {
        return this.each(function (t) {
          x(this).addClass(e.call(this, t, this.className));
        });
      }if (l) for (t = (e || "").match(T) || []; s > a; a++) if ((n = this[a], r = 1 === n.nodeType && (n.className ? (" " + n.className + " ").replace(U, " ") : " "))) {
        o = 0;while (i = t[o++]) 0 > r.indexOf(" " + i + " ") && (r += i + " ");n.className = x.trim(r);
      }return this;
    }, removeClass: function removeClass(e) {
      var t,
          n,
          r,
          i,
          o,
          a = 0,
          s = this.length,
          l = 0 === arguments.length || "string" == typeof e && e;if (x.isFunction(e)) {
        return this.each(function (t) {
          x(this).removeClass(e.call(this, t, this.className));
        });
      }if (l) for (t = (e || "").match(T) || []; s > a; a++) if ((n = this[a], r = 1 === n.nodeType && (n.className ? (" " + n.className + " ").replace(U, " ") : ""))) {
        o = 0;while (i = t[o++]) while (r.indexOf(" " + i + " ") >= 0) r = r.replace(" " + i + " ", " ");n.className = e ? x.trim(r) : "";
      }return this;
    }, toggleClass: function toggleClass(e, t) {
      var n = typeof e;return "boolean" == typeof t && "string" === n ? t ? this.addClass(e) : this.removeClass(e) : x.isFunction(e) ? this.each(function (n) {
        x(this).toggleClass(e.call(this, n, this.className, t), t);
      }) : this.each(function () {
        if ("string" === n) {
          var t,
              r = 0,
              o = x(this),
              a = e.match(T) || [];while (t = a[r++]) o.hasClass(t) ? o.removeClass(t) : o.addClass(t);
        } else (n === i || "boolean" === n) && (this.className && x._data(this, "__className__", this.className), this.className = this.className || e === !1 ? "" : x._data(this, "__className__") || "");
      });
    }, hasClass: function hasClass(e) {
      var t = " " + e + " ",
          n = 0,
          r = this.length;for (; r > n; n++) if (1 === this[n].nodeType && (" " + this[n].className + " ").replace(U, " ").indexOf(t) >= 0) {
        return !0;
      }return !1;
    }, val: function val(e) {
      var n,
          r,
          i,
          o = this[0];{
        if (arguments.length) {
          return (i = x.isFunction(e), this.each(function (n) {
            var o;1 === this.nodeType && (o = i ? e.call(this, n, x(this).val()) : e, null == o ? o = "" : "number" == typeof o ? o += "" : x.isArray(o) && (o = x.map(o, function (e) {
              return null == e ? "" : e + "";
            })), r = x.valHooks[this.type] || x.valHooks[this.nodeName.toLowerCase()], r && "set" in r && r.set(this, o, "value") !== t || (this.value = o));
          }));
        }if (o) {
          return (r = x.valHooks[o.type] || x.valHooks[o.nodeName.toLowerCase()], r && "get" in r && (n = r.get(o, "value")) !== t ? n : (n = o.value, "string" == typeof n ? n.replace(V, "") : null == n ? "" : n));
        }
      }
    } }), x.extend({ valHooks: { option: { get: function get(e) {
          var t = x.find.attr(e, "value");return null != t ? t : e.text;
        } }, select: { get: function get(e) {
          var t,
              n,
              r = e.options,
              i = e.selectedIndex,
              o = "select-one" === e.type || 0 > i,
              a = o ? null : [],
              s = o ? i + 1 : r.length,
              l = 0 > i ? s : o ? i : 0;for (; s > l; l++) if ((n = r[l], !(!n.selected && l !== i || (x.support.optDisabled ? n.disabled : null !== n.getAttribute("disabled")) || n.parentNode.disabled && x.nodeName(n.parentNode, "optgroup")))) {
            if ((t = x(n).val(), o)) {
              return t;
            }a.push(t);
          }return a;
        }, set: function set(e, t) {
          var n,
              r,
              i = e.options,
              o = x.makeArray(t),
              a = i.length;while (a--) r = i[a], (r.selected = x.inArray(x(r).val(), o) >= 0) && (n = !0);return (n || (e.selectedIndex = -1), o);
        } } }, attr: function attr(e, n, r) {
      var o,
          a,
          s = e.nodeType;if (e && 3 !== s && 8 !== s && 2 !== s) {
        return typeof e.getAttribute === i ? x.prop(e, n, r) : (1 === s && x.isXMLDoc(e) || (n = n.toLowerCase(), o = x.attrHooks[n] || (x.expr.match.bool.test(n) ? X : z)), r === t ? o && "get" in o && null !== (a = o.get(e, n)) ? a : (a = x.find.attr(e, n), null == a ? t : a) : null !== r ? o && "set" in o && (a = o.set(e, r, n)) !== t ? a : (e.setAttribute(n, r + ""), r) : (x.removeAttr(e, n), t));
      }
    }, removeAttr: function removeAttr(e, t) {
      var n,
          r,
          i = 0,
          o = t && t.match(T);if (o && 1 === e.nodeType) while (n = o[i++]) r = x.propFix[n] || n, x.expr.match.bool.test(n) ? K && Q || !G.test(n) ? e[r] = !1 : e[x.camelCase("default-" + n)] = e[r] = !1 : x.attr(e, n, ""), e.removeAttribute(Q ? n : r);
    }, attrHooks: { type: { set: function set(e, t) {
          if (!x.support.radioValue && "radio" === t && x.nodeName(e, "input")) {
            var n = e.value;return (e.setAttribute("type", t), n && (e.value = n), t);
          }
        } } }, propFix: { "for": "htmlFor", "class": "className" }, prop: function prop(e, n, r) {
      var i,
          o,
          a,
          s = e.nodeType;if (e && 3 !== s && 8 !== s && 2 !== s) {
        return (a = 1 !== s || !x.isXMLDoc(e), a && (n = x.propFix[n] || n, o = x.propHooks[n]), r !== t ? o && "set" in o && (i = o.set(e, r, n)) !== t ? i : e[n] = r : o && "get" in o && null !== (i = o.get(e, n)) ? i : e[n]);
      }
    }, propHooks: { tabIndex: { get: function get(e) {
          var t = x.find.attr(e, "tabindex");return t ? parseInt(t, 10) : Y.test(e.nodeName) || J.test(e.nodeName) && e.href ? 0 : -1;
        } } } }), X = { set: function set(e, t, n) {
      return (t === !1 ? x.removeAttr(e, n) : K && Q || !G.test(n) ? e.setAttribute(!Q && x.propFix[n] || n, n) : e[x.camelCase("default-" + n)] = e[n] = !0, n);
    } }, x.each(x.expr.match.bool.source.match(/\w+/g), function (e, n) {
    var r = x.expr.attrHandle[n] || x.find.attr;x.expr.attrHandle[n] = K && Q || !G.test(n) ? function (e, n, i) {
      var o = x.expr.attrHandle[n],
          a = i ? t : (x.expr.attrHandle[n] = t) != r(e, n, i) ? n.toLowerCase() : null;return (x.expr.attrHandle[n] = o, a);
    } : function (e, n, r) {
      return r ? t : e[x.camelCase("default-" + n)] ? n.toLowerCase() : null;
    };
  }), K && Q || (x.attrHooks.value = { set: function set(e, n, r) {
      return x.nodeName(e, "input") ? (e.defaultValue = n, t) : z && z.set(e, n, r);
    } }), Q || (z = { set: function set(e, n, r) {
      var i = e.getAttributeNode(r);return (i || e.setAttributeNode(i = e.ownerDocument.createAttribute(r)), i.value = n += "", "value" === r || n === e.getAttribute(r) ? n : t);
    } }, x.expr.attrHandle.id = x.expr.attrHandle.name = x.expr.attrHandle.coords = function (e, n, r) {
    var i;return r ? t : (i = e.getAttributeNode(n)) && "" !== i.value ? i.value : null;
  }, x.valHooks.button = { get: function get(e, n) {
      var r = e.getAttributeNode(n);return r && r.specified ? r.value : t;
    }, set: z.set }, x.attrHooks.contenteditable = { set: function set(e, t, n) {
      z.set(e, "" === t ? !1 : t, n);
    } }, x.each(["width", "height"], function (e, n) {
    x.attrHooks[n] = { set: function set(e, r) {
        return "" === r ? (e.setAttribute(n, "auto"), r) : t;
      } };
  })), x.support.hrefNormalized || x.each(["href", "src"], function (e, t) {
    x.propHooks[t] = { get: function get(e) {
        return e.getAttribute(t, 4);
      } };
  }), x.support.style || (x.attrHooks.style = { get: function get(e) {
      return e.style.cssText || t;
    }, set: function set(e, t) {
      return e.style.cssText = t + "";
    } }), x.support.optSelected || (x.propHooks.selected = { get: function get(e) {
      var t = e.parentNode;return (t && (t.selectedIndex, t.parentNode && t.parentNode.selectedIndex), null);
    } }), x.each(["tabIndex", "readOnly", "maxLength", "cellSpacing", "cellPadding", "rowSpan", "colSpan", "useMap", "frameBorder", "contentEditable"], function () {
    x.propFix[this.toLowerCase()] = this;
  }), x.support.enctype || (x.propFix.enctype = "encoding"), x.each(["radio", "checkbox"], function () {
    x.valHooks[this] = { set: function set(e, n) {
        return x.isArray(n) ? e.checked = x.inArray(x(e).val(), n) >= 0 : t;
      } }, x.support.checkOn || (x.valHooks[this].get = function (e) {
      return null === e.getAttribute("value") ? "on" : e.value;
    });
  });var Z = /^(?:input|select|textarea)$/i,
      et = /^key/,
      tt = /^(?:mouse|contextmenu)|click/,
      nt = /^(?:focusinfocus|focusoutblur)$/,
      rt = /^([^.]*)(?:\.(.+)|)$/;function it() {
    return !0;
  }function ot() {
    return !1;
  }function at() {
    try {
      return a.activeElement;
    } catch (e) {}
  }x.event = { global: {}, add: function add(e, n, r, o, a) {
      var s,
          l,
          u,
          c,
          p,
          f,
          d,
          h,
          g,
          m,
          y,
          v = x._data(e);if (v) {
        r.handler && (c = r, r = c.handler, a = c.selector), r.guid || (r.guid = x.guid++), (l = v.events) || (l = v.events = {}), (f = v.handle) || (f = v.handle = function (e) {
          return typeof x === i || e && x.event.triggered === e.type ? t : x.event.dispatch.apply(f.elem, arguments);
        }, f.elem = e), n = (n || "").match(T) || [""], u = n.length;while (u--) s = rt.exec(n[u]) || [], g = y = s[1], m = (s[2] || "").split(".").sort(), g && (p = x.event.special[g] || {}, g = (a ? p.delegateType : p.bindType) || g, p = x.event.special[g] || {}, d = x.extend({ type: g, origType: y, data: o, handler: r, guid: r.guid, selector: a, needsContext: a && x.expr.match.needsContext.test(a), namespace: m.join(".") }, c), (h = l[g]) || (h = l[g] = [], h.delegateCount = 0, p.setup && p.setup.call(e, o, m, f) !== !1 || (e.addEventListener ? e.addEventListener(g, f, !1) : e.attachEvent && e.attachEvent("on" + g, f))), p.add && (p.add.call(e, d), d.handler.guid || (d.handler.guid = r.guid)), a ? h.splice(h.delegateCount++, 0, d) : h.push(d), x.event.global[g] = !0);e = null;
      }
    }, remove: function remove(e, t, n, r, i) {
      var o,
          a,
          s,
          l,
          u,
          c,
          p,
          f,
          d,
          h,
          g,
          m = x.hasData(e) && x._data(e);if (m && (c = m.events)) {
        t = (t || "").match(T) || [""], u = t.length;while (u--) if ((s = rt.exec(t[u]) || [], d = g = s[1], h = (s[2] || "").split(".").sort(), d)) {
          p = x.event.special[d] || {}, d = (r ? p.delegateType : p.bindType) || d, f = c[d] || [], s = s[2] && RegExp("(^|\\.)" + h.join("\\.(?:.*\\.|)") + "(\\.|$)"), l = o = f.length;while (o--) a = f[o], !i && g !== a.origType || n && n.guid !== a.guid || s && !s.test(a.namespace) || r && r !== a.selector && ("**" !== r || !a.selector) || (f.splice(o, 1), a.selector && f.delegateCount--, p.remove && p.remove.call(e, a));l && !f.length && (p.teardown && p.teardown.call(e, h, m.handle) !== !1 || x.removeEvent(e, d, m.handle), delete c[d]);
        } else for (d in c) x.event.remove(e, d + t[u], n, r, !0);x.isEmptyObject(c) && (delete m.handle, x._removeData(e, "events"));
      }
    }, trigger: function trigger(n, r, i, o) {
      var s,
          l,
          u,
          c,
          p,
          f,
          d,
          h = [i || a],
          g = v.call(n, "type") ? n.type : n,
          m = v.call(n, "namespace") ? n.namespace.split(".") : [];if ((u = f = i = i || a, 3 !== i.nodeType && 8 !== i.nodeType && !nt.test(g + x.event.triggered) && (g.indexOf(".") >= 0 && (m = g.split("."), g = m.shift(), m.sort()), l = 0 > g.indexOf(":") && "on" + g, n = n[x.expando] ? n : new x.Event(g, "object" == typeof n && n), n.isTrigger = o ? 2 : 3, n.namespace = m.join("."), n.namespace_re = n.namespace ? RegExp("(^|\\.)" + m.join("\\.(?:.*\\.|)") + "(\\.|$)") : null, n.result = t, n.target || (n.target = i), r = null == r ? [n] : x.makeArray(r, [n]), p = x.event.special[g] || {}, o || !p.trigger || p.trigger.apply(i, r) !== !1))) {
        if (!o && !p.noBubble && !x.isWindow(i)) {
          for (c = p.delegateType || g, nt.test(c + g) || (u = u.parentNode); u; u = u.parentNode) h.push(u), f = u;f === (i.ownerDocument || a) && h.push(f.defaultView || f.parentWindow || e);
        }d = 0;while ((u = h[d++]) && !n.isPropagationStopped()) n.type = d > 1 ? c : p.bindType || g, s = (x._data(u, "events") || {})[n.type] && x._data(u, "handle"), s && s.apply(u, r), s = l && u[l], s && x.acceptData(u) && s.apply && s.apply(u, r) === !1 && n.preventDefault();if ((n.type = g, !o && !n.isDefaultPrevented() && (!p._default || p._default.apply(h.pop(), r) === !1) && x.acceptData(i) && l && i[g] && !x.isWindow(i))) {
          f = i[l], f && (i[l] = null), x.event.triggered = g;try {
            i[g]();
          } catch (y) {}x.event.triggered = t, f && (i[l] = f);
        }return n.result;
      }
    }, dispatch: function dispatch(e) {
      e = x.event.fix(e);var n,
          r,
          i,
          o,
          a,
          s = [],
          l = g.call(arguments),
          u = (x._data(this, "events") || {})[e.type] || [],
          c = x.event.special[e.type] || {};if ((l[0] = e, e.delegateTarget = this, !c.preDispatch || c.preDispatch.call(this, e) !== !1)) {
        s = x.event.handlers.call(this, e, u), n = 0;while ((o = s[n++]) && !e.isPropagationStopped()) {
          e.currentTarget = o.elem, a = 0;while ((i = o.handlers[a++]) && !e.isImmediatePropagationStopped()) (!e.namespace_re || e.namespace_re.test(i.namespace)) && (e.handleObj = i, e.data = i.data, r = ((x.event.special[i.origType] || {}).handle || i.handler).apply(o.elem, l), r !== t && (e.result = r) === !1 && (e.preventDefault(), e.stopPropagation()));
        }return (c.postDispatch && c.postDispatch.call(this, e), e.result);
      }
    }, handlers: function handlers(e, n) {
      var r,
          i,
          o,
          a,
          s = [],
          l = n.delegateCount,
          u = e.target;if (l && u.nodeType && (!e.button || "click" !== e.type)) for (; u != this; u = u.parentNode || this) if (1 === u.nodeType && (u.disabled !== !0 || "click" !== e.type)) {
        for (o = [], a = 0; l > a; a++) i = n[a], r = i.selector + " ", o[r] === t && (o[r] = i.needsContext ? x(r, this).index(u) >= 0 : x.find(r, this, null, [u]).length), o[r] && o.push(i);o.length && s.push({ elem: u, handlers: o });
      }return (n.length > l && s.push({ elem: this, handlers: n.slice(l) }), s);
    }, fix: function fix(e) {
      if (e[x.expando]) {
        return e;
      }var t,
          n,
          r,
          i = e.type,
          o = e,
          s = this.fixHooks[i];s || (this.fixHooks[i] = s = tt.test(i) ? this.mouseHooks : et.test(i) ? this.keyHooks : {}), r = s.props ? this.props.concat(s.props) : this.props, e = new x.Event(o), t = r.length;while (t--) n = r[t], e[n] = o[n];return (e.target || (e.target = o.srcElement || a), 3 === e.target.nodeType && (e.target = e.target.parentNode), e.metaKey = !!e.metaKey, s.filter ? s.filter(e, o) : e);
    }, props: "altKey bubbles cancelable ctrlKey currentTarget eventPhase metaKey relatedTarget shiftKey target timeStamp view which".split(" "), fixHooks: {}, keyHooks: { props: "char charCode key keyCode".split(" "), filter: function filter(e, t) {
        return (null == e.which && (e.which = null != t.charCode ? t.charCode : t.keyCode), e);
      } }, mouseHooks: { props: "button buttons clientX clientY fromElement offsetX offsetY pageX pageY screenX screenY toElement".split(" "), filter: function filter(e, n) {
        var r,
            i,
            o,
            s = n.button,
            l = n.fromElement;return (null == e.pageX && null != n.clientX && (i = e.target.ownerDocument || a, o = i.documentElement, r = i.body, e.pageX = n.clientX + (o && o.scrollLeft || r && r.scrollLeft || 0) - (o && o.clientLeft || r && r.clientLeft || 0), e.pageY = n.clientY + (o && o.scrollTop || r && r.scrollTop || 0) - (o && o.clientTop || r && r.clientTop || 0)), !e.relatedTarget && l && (e.relatedTarget = l === e.target ? n.toElement : l), e.which || s === t || (e.which = 1 & s ? 1 : 2 & s ? 3 : 4 & s ? 2 : 0), e);
      } }, special: { load: { noBubble: !0 }, focus: { trigger: function trigger() {
          if (this !== at() && this.focus) try {
            return (this.focus(), !1);
          } catch (e) {}
        }, delegateType: "focusin" }, blur: { trigger: function trigger() {
          return this === at() && this.blur ? (this.blur(), !1) : t;
        }, delegateType: "focusout" }, click: { trigger: function trigger() {
          return x.nodeName(this, "input") && "checkbox" === this.type && this.click ? (this.click(), !1) : t;
        }, _default: function _default(e) {
          return x.nodeName(e.target, "a");
        } }, beforeunload: { postDispatch: function postDispatch(e) {
          e.result !== t && (e.originalEvent.returnValue = e.result);
        } } }, simulate: function simulate(e, t, n, r) {
      var i = x.extend(new x.Event(), n, { type: e, isSimulated: !0, originalEvent: {} });r ? x.event.trigger(i, null, t) : x.event.dispatch.call(t, i), i.isDefaultPrevented() && n.preventDefault();
    } }, x.removeEvent = a.removeEventListener ? function (e, t, n) {
    e.removeEventListener && e.removeEventListener(t, n, !1);
  } : function (e, t, n) {
    var r = "on" + t;e.detachEvent && (typeof e[r] === i && (e[r] = null), e.detachEvent(r, n));
  }, x.Event = function (e, n) {
    return this instanceof x.Event ? (e && e.type ? (this.originalEvent = e, this.type = e.type, this.isDefaultPrevented = e.defaultPrevented || e.returnValue === !1 || e.getPreventDefault && e.getPreventDefault() ? it : ot) : this.type = e, n && x.extend(this, n), this.timeStamp = e && e.timeStamp || x.now(), this[x.expando] = !0, t) : new x.Event(e, n);
  }, x.Event.prototype = { isDefaultPrevented: ot, isPropagationStopped: ot, isImmediatePropagationStopped: ot, preventDefault: function preventDefault() {
      var e = this.originalEvent;this.isDefaultPrevented = it, e && (e.preventDefault ? e.preventDefault() : e.returnValue = !1);
    }, stopPropagation: function stopPropagation() {
      var e = this.originalEvent;this.isPropagationStopped = it, e && (e.stopPropagation && e.stopPropagation(), e.cancelBubble = !0);
    }, stopImmediatePropagation: function stopImmediatePropagation() {
      this.isImmediatePropagationStopped = it, this.stopPropagation();
    } }, x.each({ mouseenter: "mouseover", mouseleave: "mouseout" }, function (e, t) {
    x.event.special[e] = { delegateType: t, bindType: t, handle: function handle(e) {
        var n,
            r = this,
            i = e.relatedTarget,
            o = e.handleObj;return ((!i || i !== r && !x.contains(r, i)) && (e.type = o.origType, n = o.handler.apply(this, arguments), e.type = t), n);
      } };
  }), x.support.submitBubbles || (x.event.special.submit = { setup: function setup() {
      return x.nodeName(this, "form") ? !1 : (x.event.add(this, "click._submit keypress._submit", function (e) {
        var n = e.target,
            r = x.nodeName(n, "input") || x.nodeName(n, "button") ? n.form : t;r && !x._data(r, "submitBubbles") && (x.event.add(r, "submit._submit", function (e) {
          e._submit_bubble = !0;
        }), x._data(r, "submitBubbles", !0));
      }), t);
    }, postDispatch: function postDispatch(e) {
      e._submit_bubble && (delete e._submit_bubble, this.parentNode && !e.isTrigger && x.event.simulate("submit", this.parentNode, e, !0));
    }, teardown: function teardown() {
      return x.nodeName(this, "form") ? !1 : (x.event.remove(this, "._submit"), t);
    } }), x.support.changeBubbles || (x.event.special.change = { setup: function setup() {
      return Z.test(this.nodeName) ? (("checkbox" === this.type || "radio" === this.type) && (x.event.add(this, "propertychange._change", function (e) {
        "checked" === e.originalEvent.propertyName && (this._just_changed = !0);
      }), x.event.add(this, "click._change", function (e) {
        this._just_changed && !e.isTrigger && (this._just_changed = !1), x.event.simulate("change", this, e, !0);
      })), !1) : (x.event.add(this, "beforeactivate._change", function (e) {
        var t = e.target;Z.test(t.nodeName) && !x._data(t, "changeBubbles") && (x.event.add(t, "change._change", function (e) {
          !this.parentNode || e.isSimulated || e.isTrigger || x.event.simulate("change", this.parentNode, e, !0);
        }), x._data(t, "changeBubbles", !0));
      }), t);
    }, handle: function handle(e) {
      var n = e.target;return this !== n || e.isSimulated || e.isTrigger || "radio" !== n.type && "checkbox" !== n.type ? e.handleObj.handler.apply(this, arguments) : t;
    }, teardown: function teardown() {
      return (x.event.remove(this, "._change"), !Z.test(this.nodeName));
    } }), x.support.focusinBubbles || x.each({ focus: "focusin", blur: "focusout" }, function (e, t) {
    var n = 0,
        r = function r(e) {
      x.event.simulate(t, e.target, x.event.fix(e), !0);
    };x.event.special[t] = { setup: function setup() {
        0 === n++ && a.addEventListener(e, r, !0);
      }, teardown: function teardown() {
        0 === --n && a.removeEventListener(e, r, !0);
      } };
  }), x.fn.extend({ on: function on(e, n, r, i, o) {
      var a, s;if ("object" == typeof e) {
        "string" != typeof n && (r = r || n, n = t);for (a in e) this.on(a, n, r, e[a], o);return this;
      }if ((null == r && null == i ? (i = n, r = n = t) : null == i && ("string" == typeof n ? (i = r, r = t) : (i = r, r = n, n = t)), i === !1)) i = ot;else if (!i) {
        return this;
      }return (1 === o && (s = i, i = function (e) {
        return (x().off(e), s.apply(this, arguments));
      }, i.guid = s.guid || (s.guid = x.guid++)), this.each(function () {
        x.event.add(this, e, i, r, n);
      }));
    }, one: function one(e, t, n, r) {
      return this.on(e, t, n, r, 1);
    }, off: function off(e, n, r) {
      var i, o;if (e && e.preventDefault && e.handleObj) {
        return (i = e.handleObj, x(e.delegateTarget).off(i.namespace ? i.origType + "." + i.namespace : i.origType, i.selector, i.handler), this);
      }if ("object" == typeof e) {
        for (o in e) this.off(o, n, e[o]);return this;
      }return ((n === !1 || "function" == typeof n) && (r = n, n = t), r === !1 && (r = ot), this.each(function () {
        x.event.remove(this, e, r, n);
      }));
    }, trigger: function trigger(e, t) {
      return this.each(function () {
        x.event.trigger(e, t, this);
      });
    }, triggerHandler: function triggerHandler(e, n) {
      var r = this[0];return r ? x.event.trigger(e, n, r, !0) : t;
    } });var st = /^.[^:#\[\.,]*$/,
      lt = /^(?:parents|prev(?:Until|All))/,
      ut = x.expr.match.needsContext,
      ct = { children: !0, contents: !0, next: !0, prev: !0 };x.fn.extend({ find: function find(e) {
      var t,
          n = [],
          r = this,
          i = r.length;if ("string" != typeof e) {
        return this.pushStack(x(e).filter(function () {
          for (t = 0; i > t; t++) if (x.contains(r[t], this)) return !0;
        }));
      }for (t = 0; i > t; t++) x.find(e, r[t], n);return (n = this.pushStack(i > 1 ? x.unique(n) : n), n.selector = this.selector ? this.selector + " " + e : e, n);
    }, has: function has(e) {
      var t,
          n = x(e, this),
          r = n.length;return this.filter(function () {
        for (t = 0; r > t; t++) if (x.contains(this, n[t])) return !0;
      });
    }, not: function not(e) {
      return this.pushStack(ft(this, e || [], !0));
    }, filter: function filter(e) {
      return this.pushStack(ft(this, e || [], !1));
    }, is: function is(e) {
      return !!ft(this, "string" == typeof e && ut.test(e) ? x(e) : e || [], !1).length;
    }, closest: function closest(e, t) {
      var n,
          r = 0,
          i = this.length,
          o = [],
          a = ut.test(e) || "string" != typeof e ? x(e, t || this.context) : 0;for (; i > r; r++) for (n = this[r]; n && n !== t; n = n.parentNode) if (11 > n.nodeType && (a ? a.index(n) > -1 : 1 === n.nodeType && x.find.matchesSelector(n, e))) {
        n = o.push(n);break;
      }return this.pushStack(o.length > 1 ? x.unique(o) : o);
    }, index: function index(e) {
      return e ? "string" == typeof e ? x.inArray(this[0], x(e)) : x.inArray(e.jquery ? e[0] : e, this) : this[0] && this[0].parentNode ? this.first().prevAll().length : -1;
    }, add: function add(e, t) {
      var n = "string" == typeof e ? x(e, t) : x.makeArray(e && e.nodeType ? [e] : e),
          r = x.merge(this.get(), n);return this.pushStack(x.unique(r));
    }, addBack: function addBack(e) {
      return this.add(null == e ? this.prevObject : this.prevObject.filter(e));
    } });function pt(e, t) {
    do e = e[t]; while (e && 1 !== e.nodeType);return e;
  }x.each({ parent: function parent(e) {
      var t = e.parentNode;return t && 11 !== t.nodeType ? t : null;
    }, parents: function parents(e) {
      return x.dir(e, "parentNode");
    }, parentsUntil: function parentsUntil(e, t, n) {
      return x.dir(e, "parentNode", n);
    }, next: function next(e) {
      return pt(e, "nextSibling");
    }, prev: function prev(e) {
      return pt(e, "previousSibling");
    }, nextAll: function nextAll(e) {
      return x.dir(e, "nextSibling");
    }, prevAll: function prevAll(e) {
      return x.dir(e, "previousSibling");
    }, nextUntil: function nextUntil(e, t, n) {
      return x.dir(e, "nextSibling", n);
    }, prevUntil: function prevUntil(e, t, n) {
      return x.dir(e, "previousSibling", n);
    }, siblings: function siblings(e) {
      return x.sibling((e.parentNode || {}).firstChild, e);
    }, children: function children(e) {
      return x.sibling(e.firstChild);
    }, contents: function contents(e) {
      return x.nodeName(e, "iframe") ? e.contentDocument || e.contentWindow.document : x.merge([], e.childNodes);
    } }, function (e, t) {
    x.fn[e] = function (n, r) {
      var i = x.map(this, t, n);return ("Until" !== e.slice(-5) && (r = n), r && "string" == typeof r && (i = x.filter(r, i)), this.length > 1 && (ct[e] || (i = x.unique(i)), lt.test(e) && (i = i.reverse())), this.pushStack(i));
    };
  }), x.extend({ filter: function filter(e, t, n) {
      var r = t[0];return (n && (e = ":not(" + e + ")"), 1 === t.length && 1 === r.nodeType ? x.find.matchesSelector(r, e) ? [r] : [] : x.find.matches(e, x.grep(t, function (e) {
        return 1 === e.nodeType;
      })));
    }, dir: function dir(e, n, r) {
      var i = [],
          o = e[n];while (o && 9 !== o.nodeType && (r === t || 1 !== o.nodeType || !x(o).is(r))) 1 === o.nodeType && i.push(o), o = o[n];return i;
    }, sibling: function sibling(e, t) {
      var n = [];for (; e; e = e.nextSibling) 1 === e.nodeType && e !== t && n.push(e);return n;
    } });function ft(e, t, n) {
    if (x.isFunction(t)) {
      return x.grep(e, function (e, r) {
        return !!t.call(e, r, e) !== n;
      });
    }if (t.nodeType) {
      return x.grep(e, function (e) {
        return e === t !== n;
      });
    }if ("string" == typeof t) {
      if (st.test(t)) {
        return x.filter(t, e, n);
      }t = x.filter(t, e);
    }return x.grep(e, function (e) {
      return x.inArray(e, t) >= 0 !== n;
    });
  }function dt(e) {
    var t = ht.split("|"),
        n = e.createDocumentFragment();if (n.createElement) while (t.length) n.createElement(t.pop());return n;
  }var ht = "abbr|article|aside|audio|bdi|canvas|data|datalist|details|figcaption|figure|footer|header|hgroup|mark|meter|nav|output|progress|section|summary|time|video",
      gt = / jQuery\d+="(?:null|\d+)"/g,
      mt = RegExp("<(?:" + ht + ")[\\s/>]", "i"),
      yt = /^\s+/,
      vt = /<(?!area|br|col|embed|hr|img|input|link|meta|param)(([\w:]+)[^>]*)\/>/gi,
      bt = /<([\w:]+)/,
      xt = /<tbody/i,
      wt = /<|&#?\w+;/,
      Tt = /<(?:script|style|link)/i,
      Ct = /^(?:checkbox|radio)$/i,
      Nt = /checked\s*(?:[^=]|=\s*.checked.)/i,
      kt = /^$|\/(?:java|ecma)script/i,
      Et = /^true\/(.*)/,
      St = /^\s*<!(?:\[CDATA\[|--)|(?:\]\]|--)>\s*$/g,
      At = { option: [1, "<select multiple='multiple'>", "</select>"], legend: [1, "<fieldset>", "</fieldset>"], area: [1, "<map>", "</map>"], param: [1, "<object>", "</object>"], thead: [1, "<table>", "</table>"], tr: [2, "<table><tbody>", "</tbody></table>"], col: [2, "<table><tbody></tbody><colgroup>", "</colgroup></table>"], td: [3, "<table><tbody><tr>", "</tr></tbody></table>"], _default: x.support.htmlSerialize ? [0, "", ""] : [1, "X<div>", "</div>"] },
      jt = dt(a),
      Dt = jt.appendChild(a.createElement("div"));At.optgroup = At.option, At.tbody = At.tfoot = At.colgroup = At.caption = At.thead, At.th = At.td, x.fn.extend({ text: function text(e) {
      return x.access(this, function (e) {
        return e === t ? x.text(this) : this.empty().append((this[0] && this[0].ownerDocument || a).createTextNode(e));
      }, null, e, arguments.length);
    }, append: function append() {
      return this.domManip(arguments, function (e) {
        if (1 === this.nodeType || 11 === this.nodeType || 9 === this.nodeType) {
          var t = Lt(this, e);t.appendChild(e);
        }
      });
    }, prepend: function prepend() {
      return this.domManip(arguments, function (e) {
        if (1 === this.nodeType || 11 === this.nodeType || 9 === this.nodeType) {
          var t = Lt(this, e);t.insertBefore(e, t.firstChild);
        }
      });
    }, before: function before() {
      return this.domManip(arguments, function (e) {
        this.parentNode && this.parentNode.insertBefore(e, this);
      });
    }, after: function after() {
      return this.domManip(arguments, function (e) {
        this.parentNode && this.parentNode.insertBefore(e, this.nextSibling);
      });
    }, remove: function remove(e, t) {
      var n,
          r = e ? x.filter(e, this) : this,
          i = 0;for (; null != (n = r[i]); i++) t || 1 !== n.nodeType || x.cleanData(Ft(n)), n.parentNode && (t && x.contains(n.ownerDocument, n) && _t(Ft(n, "script")), n.parentNode.removeChild(n));return this;
    }, empty: function empty() {
      var e,
          t = 0;for (; null != (e = this[t]); t++) {
        1 === e.nodeType && x.cleanData(Ft(e, !1));while (e.firstChild) e.removeChild(e.firstChild);e.options && x.nodeName(e, "select") && (e.options.length = 0);
      }return this;
    }, clone: function clone(e, t) {
      return (e = null == e ? !1 : e, t = null == t ? e : t, this.map(function () {
        return x.clone(this, e, t);
      }));
    }, html: function html(e) {
      return x.access(this, function (e) {
        var n = this[0] || {},
            r = 0,
            i = this.length;if (e === t) return 1 === n.nodeType ? n.innerHTML.replace(gt, "") : t;if (!("string" != typeof e || Tt.test(e) || !x.support.htmlSerialize && mt.test(e) || !x.support.leadingWhitespace && yt.test(e) || At[(bt.exec(e) || ["", ""])[1].toLowerCase()])) {
          e = e.replace(vt, "<$1></$2>");try {
            for (; i > r; r++) n = this[r] || {}, 1 === n.nodeType && (x.cleanData(Ft(n, !1)), n.innerHTML = e);n = 0;
          } catch (o) {}
        }n && this.empty().append(e);
      }, null, e, arguments.length);
    }, replaceWith: function replaceWith() {
      var e = x.map(this, function (e) {
        return [e.nextSibling, e.parentNode];
      }),
          t = 0;return (this.domManip(arguments, function (n) {
        var r = e[t++],
            i = e[t++];i && (r && r.parentNode !== i && (r = this.nextSibling), x(this).remove(), i.insertBefore(n, r));
      }, !0), t ? this : this.remove());
    }, detach: function detach(e) {
      return this.remove(e, !0);
    }, domManip: function domManip(e, t, n) {
      e = d.apply([], e);var r,
          i,
          o,
          a,
          s,
          l,
          u = 0,
          c = this.length,
          p = this,
          f = c - 1,
          h = e[0],
          g = x.isFunction(h);if (g || !(1 >= c || "string" != typeof h || x.support.checkClone) && Nt.test(h)) {
        return this.each(function (r) {
          var i = p.eq(r);g && (e[0] = h.call(this, r, i.html())), i.domManip(e, t, n);
        });
      }if (c && (l = x.buildFragment(e, this[0].ownerDocument, !1, !n && this), r = l.firstChild, 1 === l.childNodes.length && (l = r), r)) {
        for (a = x.map(Ft(l, "script"), Ht), o = a.length; c > u; u++) i = l, u !== f && (i = x.clone(i, !0, !0), o && x.merge(a, Ft(i, "script"))), t.call(this[u], i, u);if (o) for (s = a[a.length - 1].ownerDocument, x.map(a, qt), u = 0; o > u; u++) i = a[u], kt.test(i.type || "") && !x._data(i, "globalEval") && x.contains(s, i) && (i.src ? x._evalUrl(i.src) : x.globalEval((i.text || i.textContent || i.innerHTML || "").replace(St, "")));l = r = null;
      }return this;
    } });function Lt(e, t) {
    return x.nodeName(e, "table") && x.nodeName(1 === t.nodeType ? t : t.firstChild, "tr") ? e.getElementsByTagName("tbody")[0] || e.appendChild(e.ownerDocument.createElement("tbody")) : e;
  }function Ht(e) {
    return (e.type = (null !== x.find.attr(e, "type")) + "/" + e.type, e);
  }function qt(e) {
    var t = Et.exec(e.type);return (t ? e.type = t[1] : e.removeAttribute("type"), e);
  }function _t(e, t) {
    var n,
        r = 0;for (; null != (n = e[r]); r++) x._data(n, "globalEval", !t || x._data(t[r], "globalEval"));
  }function Mt(e, t) {
    if (1 === t.nodeType && x.hasData(e)) {
      var n,
          r,
          i,
          o = x._data(e),
          a = x._data(t, o),
          s = o.events;if (s) {
        delete a.handle, a.events = {};for (n in s) for (r = 0, i = s[n].length; i > r; r++) x.event.add(t, n, s[n][r]);
      }a.data && (a.data = x.extend({}, a.data));
    }
  }function Ot(e, t) {
    var n, r, i;if (1 === t.nodeType) {
      if ((n = t.nodeName.toLowerCase(), !x.support.noCloneEvent && t[x.expando])) {
        i = x._data(t);for (r in i.events) x.removeEvent(t, r, i.handle);t.removeAttribute(x.expando);
      }"script" === n && t.text !== e.text ? (Ht(t).text = e.text, qt(t)) : "object" === n ? (t.parentNode && (t.outerHTML = e.outerHTML), x.support.html5Clone && e.innerHTML && !x.trim(t.innerHTML) && (t.innerHTML = e.innerHTML)) : "input" === n && Ct.test(e.type) ? (t.defaultChecked = t.checked = e.checked, t.value !== e.value && (t.value = e.value)) : "option" === n ? t.defaultSelected = t.selected = e.defaultSelected : ("input" === n || "textarea" === n) && (t.defaultValue = e.defaultValue);
    }
  }x.each({ appendTo: "append", prependTo: "prepend", insertBefore: "before", insertAfter: "after", replaceAll: "replaceWith" }, function (e, t) {
    x.fn[e] = function (e) {
      var n,
          r = 0,
          i = [],
          o = x(e),
          a = o.length - 1;for (; a >= r; r++) n = r === a ? this : this.clone(!0), x(o[r])[t](n), h.apply(i, n.get());return this.pushStack(i);
    };
  });function Ft(e, n) {
    var r,
        o,
        a = 0,
        s = typeof e.getElementsByTagName !== i ? e.getElementsByTagName(n || "*") : typeof e.querySelectorAll !== i ? e.querySelectorAll(n || "*") : t;if (!s) for (s = [], r = e.childNodes || e; null != (o = r[a]); a++) !n || x.nodeName(o, n) ? s.push(o) : x.merge(s, Ft(o, n));return n === t || n && x.nodeName(e, n) ? x.merge([e], s) : s;
  }function Bt(e) {
    Ct.test(e.type) && (e.defaultChecked = e.checked);
  }x.extend({ clone: function clone(e, t, n) {
      var r,
          i,
          o,
          a,
          s,
          l = x.contains(e.ownerDocument, e);if ((x.support.html5Clone || x.isXMLDoc(e) || !mt.test("<" + e.nodeName + ">") ? o = e.cloneNode(!0) : (Dt.innerHTML = e.outerHTML, Dt.removeChild(o = Dt.firstChild)), !(x.support.noCloneEvent && x.support.noCloneChecked || 1 !== e.nodeType && 11 !== e.nodeType || x.isXMLDoc(e)))) for (r = Ft(o), s = Ft(e), a = 0; null != (i = s[a]); ++a) r[a] && Ot(i, r[a]);if (t) if (n) for (s = s || Ft(e), r = r || Ft(o), a = 0; null != (i = s[a]); a++) Mt(i, r[a]);else Mt(e, o);return (r = Ft(o, "script"), r.length > 0 && _t(r, !l && Ft(e, "script")), r = s = i = null, o);
    }, buildFragment: function buildFragment(e, t, n, r) {
      var i,
          o,
          a,
          s,
          l,
          u,
          c,
          p = e.length,
          f = dt(t),
          d = [],
          h = 0;for (; p > h; h++) if ((o = e[h], o || 0 === o)) if ("object" === x.type(o)) x.merge(d, o.nodeType ? [o] : o);else if (wt.test(o)) {
        s = s || f.appendChild(t.createElement("div")), l = (bt.exec(o) || ["", ""])[1].toLowerCase(), c = At[l] || At._default, s.innerHTML = c[1] + o.replace(vt, "<$1></$2>") + c[2], i = c[0];while (i--) s = s.lastChild;if ((!x.support.leadingWhitespace && yt.test(o) && d.push(t.createTextNode(yt.exec(o)[0])), !x.support.tbody)) {
          o = "table" !== l || xt.test(o) ? "<table>" !== c[1] || xt.test(o) ? 0 : s : s.firstChild, i = o && o.childNodes.length;while (i--) x.nodeName(u = o.childNodes[i], "tbody") && !u.childNodes.length && o.removeChild(u);
        }x.merge(d, s.childNodes), s.textContent = "";while (s.firstChild) s.removeChild(s.firstChild);s = f.lastChild;
      } else d.push(t.createTextNode(o));s && f.removeChild(s), x.support.appendChecked || x.grep(Ft(d, "input"), Bt), h = 0;while (o = d[h++]) if ((!r || -1 === x.inArray(o, r)) && (a = x.contains(o.ownerDocument, o), s = Ft(f.appendChild(o), "script"), a && _t(s), n)) {
        i = 0;while (o = s[i++]) kt.test(o.type || "") && n.push(o);
      }return (s = null, f);
    }, cleanData: function cleanData(e, t) {
      var n,
          r,
          o,
          a,
          s = 0,
          l = x.expando,
          u = x.cache,
          c = x.support.deleteExpando,
          f = x.event.special;for (; null != (n = e[s]); s++) if ((t || x.acceptData(n)) && (o = n[l], a = o && u[o])) {
        if (a.events) for (r in a.events) f[r] ? x.event.remove(n, r) : x.removeEvent(n, r, a.handle);
        u[o] && (delete u[o], c ? delete n[l] : typeof n.removeAttribute !== i ? n.removeAttribute(l) : n[l] = null, p.push(o));
      }
    }, _evalUrl: function _evalUrl(e) {
      return x.ajax({ url: e, type: "GET", dataType: "script", async: !1, global: !1, throws: !0 });
    } }), x.fn.extend({ wrapAll: function wrapAll(e) {
      if (x.isFunction(e)) {
        return this.each(function (t) {
          x(this).wrapAll(e.call(this, t));
        });
      }if (this[0]) {
        var t = x(e, this[0].ownerDocument).eq(0).clone(!0);this[0].parentNode && t.insertBefore(this[0]), t.map(function () {
          var e = this;while (e.firstChild && 1 === e.firstChild.nodeType) e = e.firstChild;return e;
        }).append(this);
      }return this;
    }, wrapInner: function wrapInner(e) {
      return x.isFunction(e) ? this.each(function (t) {
        x(this).wrapInner(e.call(this, t));
      }) : this.each(function () {
        var t = x(this),
            n = t.contents();n.length ? n.wrapAll(e) : t.append(e);
      });
    }, wrap: function wrap(e) {
      var t = x.isFunction(e);return this.each(function (n) {
        x(this).wrapAll(t ? e.call(this, n) : e);
      });
    }, unwrap: function unwrap() {
      return this.parent().each(function () {
        x.nodeName(this, "body") || x(this).replaceWith(this.childNodes);
      }).end();
    } });var Pt,
      Rt,
      Wt,
      $t = /alpha\([^)]*\)/i,
      It = /opacity\s*=\s*([^)]*)/,
      zt = /^(top|right|bottom|left)$/,
      Xt = /^(none|table(?!-c[ea]).+)/,
      Ut = /^margin/,
      Vt = RegExp("^(" + w + ")(.*)$", "i"),
      Yt = RegExp("^(" + w + ")(?!px)[a-z%]+$", "i"),
      Jt = RegExp("^([+-])=(" + w + ")", "i"),
      Gt = { BODY: "block" },
      Qt = { position: "absolute", visibility: "hidden", display: "block" },
      Kt = { letterSpacing: 0, fontWeight: 400 },
      Zt = ["Top", "Right", "Bottom", "Left"],
      en = ["Webkit", "O", "Moz", "ms"];function tn(e, t) {
    if (t in e) {
      return t;
    }var n = t.charAt(0).toUpperCase() + t.slice(1),
        r = t,
        i = en.length;while (i--) if ((t = en[i] + n, t in e)) {
      return t;
    }return r;
  }function nn(e, t) {
    return (e = t || e, "none" === x.css(e, "display") || !x.contains(e.ownerDocument, e));
  }function rn(e, t) {
    var n,
        r,
        i,
        o = [],
        a = 0,
        s = e.length;for (; s > a; a++) r = e[a], r.style && (o[a] = x._data(r, "olddisplay"), n = r.style.display, t ? (o[a] || "none" !== n || (r.style.display = ""), "" === r.style.display && nn(r) && (o[a] = x._data(r, "olddisplay", ln(r.nodeName)))) : o[a] || (i = nn(r), (n && "none" !== n || !i) && x._data(r, "olddisplay", i ? n : x.css(r, "display"))));for (a = 0; s > a; a++) r = e[a], r.style && (t && "none" !== r.style.display && "" !== r.style.display || (r.style.display = t ? o[a] || "" : "none"));return e;
  }x.fn.extend({ css: function css(e, n) {
      return x.access(this, function (e, n, r) {
        var i,
            o,
            a = {},
            s = 0;if (x.isArray(n)) {
          for (o = Rt(e), i = n.length; i > s; s++) a[n[s]] = x.css(e, n[s], !1, o);return a;
        }return r !== t ? x.style(e, n, r) : x.css(e, n);
      }, e, n, arguments.length > 1);
    }, show: function show() {
      return rn(this, !0);
    }, hide: function hide() {
      return rn(this);
    }, toggle: function toggle(e) {
      return "boolean" == typeof e ? e ? this.show() : this.hide() : this.each(function () {
        nn(this) ? x(this).show() : x(this).hide();
      });
    } }), x.extend({ cssHooks: { opacity: { get: function get(e, t) {
          if (t) {
            var n = Wt(e, "opacity");return "" === n ? "1" : n;
          }
        } } }, cssNumber: { columnCount: !0, fillOpacity: !0, fontWeight: !0, lineHeight: !0, opacity: !0, order: !0, orphans: !0, widows: !0, zIndex: !0, zoom: !0 }, cssProps: { float: x.support.cssFloat ? "cssFloat" : "styleFloat" }, style: function style(e, n, r, i) {
      if (e && 3 !== e.nodeType && 8 !== e.nodeType && e.style) {
        var o,
            a,
            s,
            l = x.camelCase(n),
            u = e.style;if ((n = x.cssProps[l] || (x.cssProps[l] = tn(u, l)), s = x.cssHooks[n] || x.cssHooks[l], r === t)) {
          return s && "get" in s && (o = s.get(e, !1, i)) !== t ? o : u[n];
        }if ((a = typeof r, "string" === a && (o = Jt.exec(r)) && (r = (o[1] + 1) * o[2] + parseFloat(x.css(e, n)), a = "number"), !(null == r || "number" === a && isNaN(r) || ("number" !== a || x.cssNumber[l] || (r += "px"), x.support.clearCloneStyle || "" !== r || 0 !== n.indexOf("background") || (u[n] = "inherit"), s && "set" in s && (r = s.set(e, r, i)) === t)))) try {
          u[n] = r;
        } catch (c) {}
      }
    }, css: function css(e, n, r, i) {
      var o,
          a,
          s,
          l = x.camelCase(n);return (n = x.cssProps[l] || (x.cssProps[l] = tn(e.style, l)), s = x.cssHooks[n] || x.cssHooks[l], s && "get" in s && (a = s.get(e, !0, r)), a === t && (a = Wt(e, n, i)), "normal" === a && n in Kt && (a = Kt[n]), "" === r || r ? (o = parseFloat(a), r === !0 || x.isNumeric(o) ? o || 0 : a) : a);
    } }), e.getComputedStyle ? (Rt = function (t) {
    return e.getComputedStyle(t, null);
  }, Wt = function (e, n, r) {
    var i,
        o,
        a,
        s = r || Rt(e),
        l = s ? s.getPropertyValue(n) || s[n] : t,
        u = e.style;return (s && ("" !== l || x.contains(e.ownerDocument, e) || (l = x.style(e, n)), Yt.test(l) && Ut.test(n) && (i = u.width, o = u.minWidth, a = u.maxWidth, u.minWidth = u.maxWidth = u.width = l, l = s.width, u.width = i, u.minWidth = o, u.maxWidth = a)), l);
  }) : a.documentElement.currentStyle && (Rt = function (e) {
    return e.currentStyle;
  }, Wt = function (e, n, r) {
    var i,
        o,
        a,
        s = r || Rt(e),
        l = s ? s[n] : t,
        u = e.style;return (null == l && u && u[n] && (l = u[n]), Yt.test(l) && !zt.test(n) && (i = u.left, o = e.runtimeStyle, a = o && o.left, a && (o.left = e.currentStyle.left), u.left = "fontSize" === n ? "1em" : l, l = u.pixelLeft + "px", u.left = i, a && (o.left = a)), "" === l ? "auto" : l);
  });function on(e, t, n) {
    var r = Vt.exec(t);return r ? Math.max(0, r[1] - (n || 0)) + (r[2] || "px") : t;
  }function an(e, t, n, r, i) {
    var o = n === (r ? "border" : "content") ? 4 : "width" === t ? 1 : 0,
        a = 0;for (; 4 > o; o += 2) "margin" === n && (a += x.css(e, n + Zt[o], !0, i)), r ? ("content" === n && (a -= x.css(e, "padding" + Zt[o], !0, i)), "margin" !== n && (a -= x.css(e, "border" + Zt[o] + "Width", !0, i))) : (a += x.css(e, "padding" + Zt[o], !0, i), "padding" !== n && (a += x.css(e, "border" + Zt[o] + "Width", !0, i)));return a;
  }function sn(e, t, n) {
    var r = !0,
        i = "width" === t ? e.offsetWidth : e.offsetHeight,
        o = Rt(e),
        a = x.support.boxSizing && "border-box" === x.css(e, "boxSizing", !1, o);if (0 >= i || null == i) {
      if ((i = Wt(e, t, o), (0 > i || null == i) && (i = e.style[t]), Yt.test(i))) {
        return i;
      }r = a && (x.support.boxSizingReliable || i === e.style[t]), i = parseFloat(i) || 0;
    }return i + an(e, t, n || (a ? "border" : "content"), r, o) + "px";
  }function ln(e) {
    var t = a,
        n = Gt[e];return (n || (n = un(e, t), "none" !== n && n || (Pt = (Pt || x("<iframe frameborder='0' width='0' height='0'/>").css("cssText", "display:block !important")).appendTo(t.documentElement), t = (Pt[0].contentWindow || Pt[0].contentDocument).document, t.write("<!doctype html><html><body>"), t.close(), n = un(e, t), Pt.detach()), Gt[e] = n), n);
  }function un(e, t) {
    var n = x(t.createElement(e)).appendTo(t.body),
        r = x.css(n[0], "display");return (n.remove(), r);
  }x.each(["height", "width"], function (e, n) {
    x.cssHooks[n] = { get: function get(e, r, i) {
        return r ? 0 === e.offsetWidth && Xt.test(x.css(e, "display")) ? x.swap(e, Qt, function () {
          return sn(e, n, i);
        }) : sn(e, n, i) : t;
      }, set: function set(e, t, r) {
        var i = r && Rt(e);return on(e, t, r ? an(e, n, r, x.support.boxSizing && "border-box" === x.css(e, "boxSizing", !1, i), i) : 0);
      } };
  }), x.support.opacity || (x.cssHooks.opacity = { get: function get(e, t) {
      return It.test((t && e.currentStyle ? e.currentStyle.filter : e.style.filter) || "") ? 0.01 * parseFloat(RegExp.$1) + "" : t ? "1" : "";
    }, set: function set(e, t) {
      var n = e.style,
          r = e.currentStyle,
          i = x.isNumeric(t) ? "alpha(opacity=" + 100 * t + ")" : "",
          o = r && r.filter || n.filter || "";n.zoom = 1, (t >= 1 || "" === t) && "" === x.trim(o.replace($t, "")) && n.removeAttribute && (n.removeAttribute("filter"), "" === t || r && !r.filter) || (n.filter = $t.test(o) ? o.replace($t, i) : o + " " + i);
    } }), x(function () {
    x.support.reliableMarginRight || (x.cssHooks.marginRight = { get: function get(e, n) {
        return n ? x.swap(e, { display: "inline-block" }, Wt, [e, "marginRight"]) : t;
      } }), !x.support.pixelPosition && x.fn.position && x.each(["top", "left"], function (e, n) {
      x.cssHooks[n] = { get: function get(e, r) {
          return r ? (r = Wt(e, n), Yt.test(r) ? x(e).position()[n] + "px" : r) : t;
        } };
    });
  }), x.expr && x.expr.filters && (x.expr.filters.hidden = function (e) {
    return 0 >= e.offsetWidth && 0 >= e.offsetHeight || !x.support.reliableHiddenOffsets && "none" === (e.style && e.style.display || x.css(e, "display"));
  }, x.expr.filters.visible = function (e) {
    return !x.expr.filters.hidden(e);
  }), x.each({ margin: "", padding: "", border: "Width" }, function (e, t) {
    x.cssHooks[e + t] = { expand: function expand(n) {
        var r = 0,
            i = {},
            o = "string" == typeof n ? n.split(" ") : [n];for (; 4 > r; r++) i[e + Zt[r] + t] = o[r] || o[r - 2] || o[0];return i;
      } }, Ut.test(e) || (x.cssHooks[e + t].set = on);
  });var cn = /%20/g,
      pn = /\[\]$/,
      fn = /\r?\n/g,
      dn = /^(?:submit|button|image|reset|file)$/i,
      hn = /^(?:input|select|textarea|keygen)/i;x.fn.extend({ serialize: function serialize() {
      return x.param(this.serializeArray());
    }, serializeArray: function serializeArray() {
      return this.map(function () {
        var e = x.prop(this, "elements");return e ? x.makeArray(e) : this;
      }).filter(function () {
        var e = this.type;return this.name && !x(this).is(":disabled") && hn.test(this.nodeName) && !dn.test(e) && (this.checked || !Ct.test(e));
      }).map(function (e, t) {
        var n = x(this).val();return null == n ? null : x.isArray(n) ? x.map(n, function (e) {
          return { name: t.name, value: e.replace(fn, "\r\n") };
        }) : { name: t.name, value: n.replace(fn, "\r\n") };
      }).get();
    } }), x.param = function (e, n) {
    var r,
        i = [],
        o = function o(e, t) {
      t = x.isFunction(t) ? t() : null == t ? "" : t, i[i.length] = encodeURIComponent(e) + "=" + encodeURIComponent(t);
    };if ((n === t && (n = x.ajaxSettings && x.ajaxSettings.traditional), x.isArray(e) || e.jquery && !x.isPlainObject(e))) x.each(e, function () {
      o(this.name, this.value);
    });else for (r in e) gn(r, e[r], n, o);return i.join("&").replace(cn, "+");
  };function gn(e, t, n, r) {
    var i;if (x.isArray(t)) x.each(t, function (t, i) {
      n || pn.test(e) ? r(e, i) : gn(e + "[" + ("object" == typeof i ? t : "") + "]", i, n, r);
    });else if (n || "object" !== x.type(t)) r(e, t);else for (i in t) gn(e + "[" + i + "]", t[i], n, r);
  }x.each("blur focus focusin focusout load resize scroll unload click dblclick mousedown mouseup mousemove mouseover mouseout mouseenter mouseleave change select submit keydown keypress keyup error contextmenu".split(" "), function (e, t) {
    x.fn[t] = function (e, n) {
      return arguments.length > 0 ? this.on(t, null, e, n) : this.trigger(t);
    };
  }), x.fn.extend({ hover: function hover(e, t) {
      return this.mouseenter(e).mouseleave(t || e);
    }, bind: function bind(e, t, n) {
      return this.on(e, null, t, n);
    }, unbind: function unbind(e, t) {
      return this.off(e, null, t);
    }, delegate: function delegate(e, t, n, r) {
      return this.on(t, e, n, r);
    }, undelegate: function undelegate(e, t, n) {
      return 1 === arguments.length ? this.off(e, "**") : this.off(t, e || "**", n);
    } });var mn,
      yn,
      vn = x.now(),
      bn = /\?/,
      xn = /#.*$/,
      wn = /([?&])_=[^&]*/,
      Tn = /^(.*?):[ \t]*([^\r\n]*)\r?$/gm,
      Cn = /^(?:about|app|app-storage|.+-extension|file|res|widget):$/,
      Nn = /^(?:GET|HEAD)$/,
      kn = /^\/\//,
      En = /^([\w.+-]+:)(?:\/\/([^\/?#:]*)(?::(\d+)|)|)/,
      Sn = x.fn.load,
      An = {},
      jn = {},
      Dn = "*/".concat("*");try {
    yn = o.href;
  } catch (Ln) {
    yn = a.createElement("a"), yn.href = "", yn = yn.href;
  }mn = En.exec(yn.toLowerCase()) || [];function Hn(e) {
    return function (t, n) {
      "string" != typeof t && (n = t, t = "*");var r,
          i = 0,
          o = t.toLowerCase().match(T) || [];if (x.isFunction(n)) while (r = o[i++]) "+" === r[0] ? (r = r.slice(1) || "*", (e[r] = e[r] || []).unshift(n)) : (e[r] = e[r] || []).push(n);
    };
  }function qn(e, n, r, i) {
    var o = {},
        a = e === jn;function s(l) {
      var u;return (o[l] = !0, x.each(e[l] || [], function (e, l) {
        var c = l(n, r, i);return "string" != typeof c || a || o[c] ? a ? !(u = c) : t : (n.dataTypes.unshift(c), s(c), !1);
      }), u);
    }return s(n.dataTypes[0]) || !o["*"] && s("*");
  }function _n(e, n) {
    var r,
        i,
        o = x.ajaxSettings.flatOptions || {};for (i in n) n[i] !== t && ((o[i] ? e : r || (r = {}))[i] = n[i]);return (r && x.extend(!0, e, r), e);
  }x.fn.load = function (e, n, r) {
    if ("string" != typeof e && Sn) return Sn.apply(this, arguments);var i,
        o,
        a,
        s = this,
        l = e.indexOf(" ");return (l >= 0 && (i = e.slice(l, e.length), e = e.slice(0, l)), x.isFunction(n) ? (r = n, n = t) : n && "object" == typeof n && (a = "POST"), s.length > 0 && x.ajax({ url: e, type: a, dataType: "html", data: n }).done(function (e) {
      o = arguments, s.html(i ? x("<div>").append(x.parseHTML(e)).find(i) : e);
    }).complete(r && function (e, t) {
      s.each(r, o || [e.responseText, t, e]);
    }), this);
  }, x.each(["ajaxStart", "ajaxStop", "ajaxComplete", "ajaxError", "ajaxSuccess", "ajaxSend"], function (e, t) {
    x.fn[t] = function (e) {
      return this.on(t, e);
    };
  }), x.extend({ active: 0, lastModified: {}, etag: {}, ajaxSettings: { url: yn, type: "GET", isLocal: Cn.test(mn[1]), global: !0, processData: !0, async: !0, contentType: "application/x-www-form-urlencoded; charset=UTF-8", accepts: { "*": Dn, text: "text/plain", html: "text/html", xml: "application/xml, text/xml", json: "application/json, text/javascript" }, contents: { xml: /xml/, html: /html/, json: /json/ }, responseFields: { xml: "responseXML", text: "responseText", json: "responseJSON" }, converters: { "* text": String, "text html": !0, "text json": x.parseJSON, "text xml": x.parseXML }, flatOptions: { url: !0, context: !0 } }, ajaxSetup: function ajaxSetup(e, t) {
      return t ? _n(_n(e, x.ajaxSettings), t) : _n(x.ajaxSettings, e);
    }, ajaxPrefilter: Hn(An), ajaxTransport: Hn(jn), ajax: function ajax(e, n) {
      "object" == typeof e && (n = e, e = t), n = n || {};var r,
          i,
          o,
          a,
          s,
          l,
          u,
          c,
          p = x.ajaxSetup({}, n),
          f = p.context || p,
          d = p.context && (f.nodeType || f.jquery) ? x(f) : x.event,
          h = x.Deferred(),
          g = x.Callbacks("once memory"),
          m = p.statusCode || {},
          y = {},
          v = {},
          b = 0,
          w = "canceled",
          C = { readyState: 0, getResponseHeader: function getResponseHeader(e) {
          var t;if (2 === b) {
            if (!c) {
              c = {};while (t = Tn.exec(a)) c[t[1].toLowerCase()] = t[2];
            }t = c[e.toLowerCase()];
          }return null == t ? null : t;
        }, getAllResponseHeaders: function getAllResponseHeaders() {
          return 2 === b ? a : null;
        }, setRequestHeader: function setRequestHeader(e, t) {
          var n = e.toLowerCase();return (b || (e = v[n] = v[n] || e, y[e] = t), this);
        }, overrideMimeType: function overrideMimeType(e) {
          return (b || (p.mimeType = e), this);
        }, statusCode: function statusCode(e) {
          var t;if (e) if (2 > b) for (t in e) m[t] = [m[t], e[t]];else C.always(e[C.status]);return this;
        }, abort: function abort(e) {
          var t = e || w;return (u && u.abort(t), k(0, t), this);
        } };if ((h.promise(C).complete = g.add, C.success = C.done, C.error = C.fail, p.url = ((e || p.url || yn) + "").replace(xn, "").replace(kn, mn[1] + "//"), p.type = n.method || n.type || p.method || p.type, p.dataTypes = x.trim(p.dataType || "*").toLowerCase().match(T) || [""], null == p.crossDomain && (r = En.exec(p.url.toLowerCase()), p.crossDomain = !(!r || r[1] === mn[1] && r[2] === mn[2] && (r[3] || ("http:" === r[1] ? "80" : "443")) === (mn[3] || ("http:" === mn[1] ? "80" : "443")))), p.data && p.processData && "string" != typeof p.data && (p.data = x.param(p.data, p.traditional)), qn(An, p, n, C), 2 === b)) {
        return C;
      }l = p.global, l && 0 === x.active++ && x.event.trigger("ajaxStart"), p.type = p.type.toUpperCase(), p.hasContent = !Nn.test(p.type), o = p.url, p.hasContent || (p.data && (o = p.url += (bn.test(o) ? "&" : "?") + p.data, delete p.data), p.cache === !1 && (p.url = wn.test(o) ? o.replace(wn, "$1_=" + vn++) : o + (bn.test(o) ? "&" : "?") + "_=" + vn++)), p.ifModified && (x.lastModified[o] && C.setRequestHeader("If-Modified-Since", x.lastModified[o]), x.etag[o] && C.setRequestHeader("If-None-Match", x.etag[o])), (p.data && p.hasContent && p.contentType !== !1 || n.contentType) && C.setRequestHeader("Content-Type", p.contentType), C.setRequestHeader("Accept", p.dataTypes[0] && p.accepts[p.dataTypes[0]] ? p.accepts[p.dataTypes[0]] + ("*" !== p.dataTypes[0] ? ", " + Dn + "; q=0.01" : "") : p.accepts["*"]);for (i in p.headers) C.setRequestHeader(i, p.headers[i]);if (p.beforeSend && (p.beforeSend.call(f, C, p) === !1 || 2 === b)) {
        return C.abort();
      }w = "abort";for (i in { success: 1, error: 1, complete: 1 }) C[i](p[i]);if (u = qn(jn, p, n, C)) {
        C.readyState = 1, l && d.trigger("ajaxSend", [C, p]), p.async && p.timeout > 0 && (s = setTimeout(function () {
          C.abort("timeout");
        }, p.timeout));try {
          b = 1, u.send(y, k);
        } catch (N) {
          if (!(2 > b)) throw N;k(-1, N);
        }
      } else k(-1, "No Transport");function k(e, n, r, i) {
        var c,
            y,
            v,
            w,
            T,
            N = n;2 !== b && (b = 2, s && clearTimeout(s), u = t, a = i || "", C.readyState = e > 0 ? 4 : 0, c = e >= 200 && 300 > e || 304 === e, r && (w = Mn(p, C, r)), w = On(p, w, C, c), c ? (p.ifModified && (T = C.getResponseHeader("Last-Modified"), T && (x.lastModified[o] = T), T = C.getResponseHeader("etag"), T && (x.etag[o] = T)), 204 === e || "HEAD" === p.type ? N = "nocontent" : 304 === e ? N = "notmodified" : (N = w.state, y = w.data, v = w.error, c = !v)) : (v = N, (e || !N) && (N = "error", 0 > e && (e = 0))), C.status = e, C.statusText = (n || N) + "", c ? h.resolveWith(f, [y, N, C]) : h.rejectWith(f, [C, N, v]), C.statusCode(m), m = t, l && d.trigger(c ? "ajaxSuccess" : "ajaxError", [C, p, c ? y : v]), g.fireWith(f, [C, N]), l && (d.trigger("ajaxComplete", [C, p]), --x.active || x.event.trigger("ajaxStop")));
      }return C;
    }, getJSON: function getJSON(e, t, n) {
      return x.get(e, t, n, "json");
    }, getScript: function getScript(e, n) {
      return x.get(e, t, n, "script");
    } }), x.each(["get", "post"], function (e, n) {
    x[n] = function (e, r, i, o) {
      return (x.isFunction(r) && (o = o || i, i = r, r = t), x.ajax({ url: e, type: n, dataType: o, data: r, success: i }));
    };
  });function Mn(e, n, r) {
    var i,
        o,
        a,
        s,
        l = e.contents,
        u = e.dataTypes;while ("*" === u[0]) u.shift(), o === t && (o = e.mimeType || n.getResponseHeader("Content-Type"));if (o) for (s in l) if (l[s] && l[s].test(o)) {
      u.unshift(s);break;
    }if (u[0] in r) a = u[0];else {
      for (s in r) {
        if (!u[0] || e.converters[s + " " + u[0]]) {
          a = s;break;
        }i || (i = s);
      }a = a || i;
    }return a ? (a !== u[0] && u.unshift(a), r[a]) : t;
  }function On(e, t, n, r) {
    var i,
        o,
        a,
        s,
        l,
        u = {},
        c = e.dataTypes.slice();if (c[1]) for (a in e.converters) u[a.toLowerCase()] = e.converters[a];o = c.shift();while (o) if ((e.responseFields[o] && (n[e.responseFields[o]] = t), !l && r && e.dataFilter && (t = e.dataFilter(t, e.dataType)), l = o, o = c.shift())) if ("*" === o) o = l;else if ("*" !== l && l !== o) {
      if ((a = u[l + " " + o] || u["* " + o], !a)) for (i in u) if ((s = i.split(" "), s[1] === o && (a = u[l + " " + s[0]] || u["* " + s[0]]))) {
        a === !0 ? a = u[i] : u[i] !== !0 && (o = s[0], c.unshift(s[1]));break;
      }if (a !== !0) if (a && e.throws) t = a(t);else try {
        t = a(t);
      } catch (p) {
        return { state: "parsererror", error: a ? p : "No conversion from " + l + " to " + o };
      }
    }return { state: "success", data: t };
  }x.ajaxSetup({ accepts: { script: "text/javascript, application/javascript, application/ecmascript, application/x-ecmascript" }, contents: { script: /(?:java|ecma)script/ }, converters: { "text script": function (e) {
        return (x.globalEval(e), e);
      } } }), x.ajaxPrefilter("script", function (e) {
    e.cache === t && (e.cache = !1), e.crossDomain && (e.type = "GET", e.global = !1);
  }), x.ajaxTransport("script", function (e) {
    if (e.crossDomain) {
      var n,
          r = a.head || x("head")[0] || a.documentElement;return { send: function send(t, i) {
          n = a.createElement("script"), n.async = !0, e.scriptCharset && (n.charset = e.scriptCharset), n.src = e.url, n.onload = n.onreadystatechange = function (e, t) {
            (t || !n.readyState || /loaded|complete/.test(n.readyState)) && (n.onload = n.onreadystatechange = null, n.parentNode && n.parentNode.removeChild(n), n = null, t || i(200, "success"));
          }, r.insertBefore(n, r.firstChild);
        }, abort: function abort() {
          n && n.onload(t, !0);
        } };
    }
  });var Fn = [],
      Bn = /(=)\?(?=&|$)|\?\?/;x.ajaxSetup({ jsonp: "callback", jsonpCallback: function jsonpCallback() {
      var e = Fn.pop() || x.expando + "_" + vn++;return (this[e] = !0, e);
    } }), x.ajaxPrefilter("json jsonp", function (n, r, i) {
    var o,
        a,
        s,
        l = n.jsonp !== !1 && (Bn.test(n.url) ? "url" : "string" == typeof n.data && !(n.contentType || "").indexOf("application/x-www-form-urlencoded") && Bn.test(n.data) && "data");return l || "jsonp" === n.dataTypes[0] ? (o = n.jsonpCallback = x.isFunction(n.jsonpCallback) ? n.jsonpCallback() : n.jsonpCallback, l ? n[l] = n[l].replace(Bn, "$1" + o) : n.jsonp !== !1 && (n.url += (bn.test(n.url) ? "&" : "?") + n.jsonp + "=" + o), n.converters["script json"] = function () {
      return (s || x.error(o + " was not called"), s[0]);
    }, n.dataTypes[0] = "json", a = e[o], e[o] = function () {
      s = arguments;
    }, i.always(function () {
      e[o] = a, n[o] && (n.jsonpCallback = r.jsonpCallback, Fn.push(o)), s && x.isFunction(a) && a(s[0]), s = a = t;
    }), "script") : t;
  });var Pn,
      Rn,
      Wn = 0,
      $n = e.ActiveXObject && function () {
    var e;for (e in Pn) Pn[e](t, !0);
  };function In() {
    try {
      return new e.XMLHttpRequest();
    } catch (t) {}
  }function zn() {
    try {
      return new e.ActiveXObject("Microsoft.XMLHTTP");
    } catch (t) {}
  }x.ajaxSettings.xhr = e.ActiveXObject ? function () {
    return !this.isLocal && In() || zn();
  } : In, Rn = x.ajaxSettings.xhr(), x.support.cors = !!Rn && "withCredentials" in Rn, Rn = x.support.ajax = !!Rn, Rn && x.ajaxTransport(function (n) {
    if (!n.crossDomain || x.support.cors) {
      var r;return { send: function send(i, o) {
          var a,
              s,
              l = n.xhr();if ((n.username ? l.open(n.type, n.url, n.async, n.username, n.password) : l.open(n.type, n.url, n.async), n.xhrFields)) for (s in n.xhrFields) l[s] = n.xhrFields[s];n.mimeType && l.overrideMimeType && l.overrideMimeType(n.mimeType), n.crossDomain || i["X-Requested-With"] || (i["X-Requested-With"] = "XMLHttpRequest");try {
            for (s in i) l.setRequestHeader(s, i[s]);
          } catch (u) {}l.send(n.hasContent && n.data || null), r = function (e, i) {
            var s, u, c, p;try {
              if (r && (i || 4 === l.readyState)) if ((r = t, a && (l.onreadystatechange = x.noop, $n && delete Pn[a]), i)) 4 !== l.readyState && l.abort();else {
                p = {}, s = l.status, u = l.getAllResponseHeaders(), "string" == typeof l.responseText && (p.text = l.responseText);try {
                  c = l.statusText;
                } catch (f) {
                  c = "";
                }s || !n.isLocal || n.crossDomain ? 1223 === s && (s = 204) : s = p.text ? 200 : 404;
              }
            } catch (d) {
              i || o(-1, d);
            }p && o(s, c, p, u);
          }, n.async ? 4 === l.readyState ? setTimeout(r) : (a = ++Wn, $n && (Pn || (Pn = {}, x(e).unload($n)), Pn[a] = r), l.onreadystatechange = r) : r();
        }, abort: function abort() {
          r && r(t, !0);
        } };
    }
  });var Xn,
      Un,
      Vn = /^(?:toggle|show|hide)$/,
      Yn = RegExp("^(?:([+-])=|)(" + w + ")([a-z%]*)$", "i"),
      Jn = /queueHooks$/,
      Gn = [nr],
      Qn = { "*": [function (e, t) {
      var n = this.createTween(e, t),
          r = n.cur(),
          i = Yn.exec(t),
          o = i && i[3] || (x.cssNumber[e] ? "" : "px"),
          a = (x.cssNumber[e] || "px" !== o && +r) && Yn.exec(x.css(n.elem, e)),
          s = 1,
          l = 20;if (a && a[3] !== o) {
        o = o || a[3], i = i || [], a = +r || 1;do s = s || ".5", a /= s, x.style(n.elem, e, a + o); while (s !== (s = n.cur() / r) && 1 !== s && --l);
      }return (i && (a = n.start = +a || +r || 0, n.unit = o, n.end = i[1] ? a + (i[1] + 1) * i[2] : +i[2]), n);
    }] };function Kn() {
    return (setTimeout(function () {
      Xn = t;
    }), Xn = x.now());
  }function Zn(e, t, n) {
    var r,
        i = (Qn[t] || []).concat(Qn["*"]),
        o = 0,
        a = i.length;for (; a > o; o++) if (r = i[o].call(n, t, e)) {
      return r;
    }
  }function er(e, t, n) {
    var r,
        i,
        o = 0,
        a = Gn.length,
        s = x.Deferred().always(function () {
      delete l.elem;
    }),
        l = (function (_l) {
      var _lWrapper = function l() {
        return _l.apply(this, arguments);
      };

      _lWrapper.toString = function () {
        return _l.toString();
      };

      return _lWrapper;
    })(function () {
      if (i) return !1;var t = Xn || Kn(),
          n = Math.max(0, u.startTime + u.duration - t),
          r = n / u.duration || 0,
          o = 1 - r,
          a = 0,
          l = u.tweens.length;for (; l > a; a++) u.tweens[a].run(o);return (s.notifyWith(e, [u, o, n]), 1 > o && l ? n : (s.resolveWith(e, [u]), !1));
    }),
        u = s.promise({ elem: e, props: x.extend({}, t), opts: x.extend(!0, { specialEasing: {} }, n), originalProperties: t, originalOptions: n, startTime: Xn || Kn(), duration: n.duration, tweens: [], createTween: function createTween(t, n) {
        var r = x.Tween(e, u.opts, t, n, u.opts.specialEasing[t] || u.opts.easing);return (u.tweens.push(r), r);
      }, stop: function stop(t) {
        var n = 0,
            r = t ? u.tweens.length : 0;if (i) {
          return this;
        }for (i = !0; r > n; n++) u.tweens[n].run(1);return (t ? s.resolveWith(e, [u, t]) : s.rejectWith(e, [u, t]), this);
      } }),
        c = u.props;for (tr(c, u.opts.specialEasing); a > o; o++) if (r = Gn[o].call(u, e, c, u.opts)) {
      return r;
    }return (x.map(c, Zn, u), x.isFunction(u.opts.start) && u.opts.start.call(e, u), x.fx.timer(x.extend(l, { elem: e, anim: u, queue: u.opts.queue })), u.progress(u.opts.progress).done(u.opts.done, u.opts.complete).fail(u.opts.fail).always(u.opts.always));
  }function tr(e, t) {
    var n, r, i, o, a;for (n in e) if ((r = x.camelCase(n), i = t[r], o = e[n], x.isArray(o) && (i = o[1], o = e[n] = o[0]), n !== r && (e[r] = o, delete e[n]), a = x.cssHooks[r], a && "expand" in a)) {
      o = a.expand(o), delete e[r];for (n in o) n in e || (e[n] = o[n], t[n] = i);
    } else t[r] = i;
  }x.Animation = x.extend(er, { tweener: function tweener(e, t) {
      x.isFunction(e) ? (t = e, e = ["*"]) : e = e.split(" ");var n,
          r = 0,
          i = e.length;for (; i > r; r++) n = e[r], Qn[n] = Qn[n] || [], Qn[n].unshift(t);
    }, prefilter: function prefilter(e, t) {
      t ? Gn.unshift(e) : Gn.push(e);
    } });function nr(e, t, n) {
    var r,
        i,
        o,
        a,
        s,
        l,
        u = this,
        c = {},
        p = e.style,
        f = e.nodeType && nn(e),
        d = x._data(e, "fxshow");n.queue || (s = x._queueHooks(e, "fx"), null == s.unqueued && (s.unqueued = 0, l = s.empty.fire, s.empty.fire = function () {
      s.unqueued || l();
    }), s.unqueued++, u.always(function () {
      u.always(function () {
        s.unqueued--, x.queue(e, "fx").length || s.empty.fire();
      });
    })), 1 === e.nodeType && ("height" in t || "width" in t) && (n.overflow = [p.overflow, p.overflowX, p.overflowY], "inline" === x.css(e, "display") && "none" === x.css(e, "float") && (x.support.inlineBlockNeedsLayout && "inline" !== ln(e.nodeName) ? p.zoom = 1 : p.display = "inline-block")), n.overflow && (p.overflow = "hidden", x.support.shrinkWrapBlocks || u.always(function () {
      p.overflow = n.overflow[0], p.overflowX = n.overflow[1], p.overflowY = n.overflow[2];
    }));for (r in t) if ((i = t[r], Vn.exec(i))) {
      if ((delete t[r], o = o || "toggle" === i, i === (f ? "hide" : "show"))) continue;c[r] = d && d[r] || x.style(e, r);
    }if (!x.isEmptyObject(c)) {
      d ? "hidden" in d && (f = d.hidden) : d = x._data(e, "fxshow", {}), o && (d.hidden = !f), f ? x(e).show() : u.done(function () {
        x(e).hide();
      }), u.done(function () {
        var t;x._removeData(e, "fxshow");for (t in c) x.style(e, t, c[t]);
      });for (r in c) a = Zn(f ? d[r] : 0, r, u), r in d || (d[r] = a.start, f && (a.end = a.start, a.start = "width" === r || "height" === r ? 1 : 0));
    }
  }function rr(e, t, n, r, i) {
    return new rr.prototype.init(e, t, n, r, i);
  }x.Tween = rr, rr.prototype = { constructor: rr, init: function init(e, t, n, r, i, o) {
      this.elem = e, this.prop = n, this.easing = i || "swing", this.options = t, this.start = this.now = this.cur(), this.end = r, this.unit = o || (x.cssNumber[n] ? "" : "px");
    }, cur: function cur() {
      var e = rr.propHooks[this.prop];return e && e.get ? e.get(this) : rr.propHooks._default.get(this);
    }, run: function run(e) {
      var t,
          n = rr.propHooks[this.prop];return (this.pos = t = this.options.duration ? x.easing[this.easing](e, this.options.duration * e, 0, 1, this.options.duration) : e, this.now = (this.end - this.start) * t + this.start, this.options.step && this.options.step.call(this.elem, this.now, this), n && n.set ? n.set(this) : rr.propHooks._default.set(this), this);
    } }, rr.prototype.init.prototype = rr.prototype, rr.propHooks = { _default: { get: function get(e) {
        var t;return null == e.elem[e.prop] || e.elem.style && null != e.elem.style[e.prop] ? (t = x.css(e.elem, e.prop, ""), t && "auto" !== t ? t : 0) : e.elem[e.prop];
      }, set: function set(e) {
        x.fx.step[e.prop] ? x.fx.step[e.prop](e) : e.elem.style && (null != e.elem.style[x.cssProps[e.prop]] || x.cssHooks[e.prop]) ? x.style(e.elem, e.prop, e.now + e.unit) : e.elem[e.prop] = e.now;
      } } }, rr.propHooks.scrollTop = rr.propHooks.scrollLeft = { set: function set(e) {
      e.elem.nodeType && e.elem.parentNode && (e.elem[e.prop] = e.now);
    } }, x.each(["toggle", "show", "hide"], function (e, t) {
    var n = x.fn[t];x.fn[t] = function (e, r, i) {
      return null == e || "boolean" == typeof e ? n.apply(this, arguments) : this.animate(ir(t, !0), e, r, i);
    };
  }), x.fn.extend({ fadeTo: function fadeTo(e, t, n, r) {
      return this.filter(nn).css("opacity", 0).show().end().animate({ opacity: t }, e, n, r);
    }, animate: function animate(e, t, n, r) {
      var i = x.isEmptyObject(e),
          o = x.speed(t, n, r),
          a = function a() {
        var t = er(this, x.extend({}, e), o);(i || x._data(this, "finish")) && t.stop(!0);
      };return (a.finish = a, i || o.queue === !1 ? this.each(a) : this.queue(o.queue, a));
    }, stop: function stop(e, n, r) {
      var i = function i(e) {
        var t = e.stop;delete e.stop, t(r);
      };return ("string" != typeof e && (r = n, n = e, e = t), n && e !== !1 && this.queue(e || "fx", []), this.each(function () {
        var t = !0,
            n = null != e && e + "queueHooks",
            o = x.timers,
            a = x._data(this);if (n) a[n] && a[n].stop && i(a[n]);else for (n in a) a[n] && a[n].stop && Jn.test(n) && i(a[n]);for (n = o.length; n--;) o[n].elem !== this || null != e && o[n].queue !== e || (o[n].anim.stop(r), t = !1, o.splice(n, 1));(t || !r) && x.dequeue(this, e);
      }));
    }, finish: function finish(e) {
      return (e !== !1 && (e = e || "fx"), this.each(function () {
        var t,
            n = x._data(this),
            r = n[e + "queue"],
            i = n[e + "queueHooks"],
            o = x.timers,
            a = r ? r.length : 0;for (n.finish = !0, x.queue(this, e, []), i && i.stop && i.stop.call(this, !0), t = o.length; t--;) o[t].elem === this && o[t].queue === e && (o[t].anim.stop(!0), o.splice(t, 1));for (t = 0; a > t; t++) r[t] && r[t].finish && r[t].finish.call(this);delete n.finish;
      }));
    } });function ir(e, t) {
    var n,
        r = { height: e },
        i = 0;for (t = t ? 1 : 0; 4 > i; i += 2 - t) n = Zt[i], r["margin" + n] = r["padding" + n] = e;return (t && (r.opacity = r.width = e), r);
  }x.each({ slideDown: ir("show"), slideUp: ir("hide"), slideToggle: ir("toggle"), fadeIn: { opacity: "show" }, fadeOut: { opacity: "hide" }, fadeToggle: { opacity: "toggle" } }, function (e, t) {
    x.fn[e] = function (e, n, r) {
      return this.animate(t, e, n, r);
    };
  }), x.speed = function (e, t, n) {
    var r = e && "object" == typeof e ? x.extend({}, e) : { complete: n || !n && t || x.isFunction(e) && e, duration: e, easing: n && t || t && !x.isFunction(t) && t };return (r.duration = x.fx.off ? 0 : "number" == typeof r.duration ? r.duration : r.duration in x.fx.speeds ? x.fx.speeds[r.duration] : x.fx.speeds._default, (null == r.queue || r.queue === !0) && (r.queue = "fx"), r.old = r.complete, r.complete = function () {
      x.isFunction(r.old) && r.old.call(this), r.queue && x.dequeue(this, r.queue);
    }, r);
  }, x.easing = { linear: function linear(e) {
      return e;
    }, swing: function swing(e) {
      return 0.5 - Math.cos(e * Math.PI) / 2;
    } }, x.timers = [], x.fx = rr.prototype.init, x.fx.tick = function () {
    var e,
        n = x.timers,
        r = 0;for (Xn = x.now(); n.length > r; r++) e = n[r], e() || n[r] !== e || n.splice(r--, 1);n.length || x.fx.stop(), Xn = t;
  }, x.fx.timer = function (e) {
    e() && x.timers.push(e) && x.fx.start();
  }, x.fx.interval = 13, x.fx.start = function () {
    Un || (Un = setInterval(x.fx.tick, x.fx.interval));
  }, x.fx.stop = function () {
    clearInterval(Un), Un = null;
  }, x.fx.speeds = { slow: 600, fast: 200, _default: 400 }, x.fx.step = {}, x.expr && x.expr.filters && (x.expr.filters.animated = function (e) {
    return x.grep(x.timers, function (t) {
      return e === t.elem;
    }).length;
  }), x.fn.offset = function (e) {
    if (arguments.length) return e === t ? this : this.each(function (t) {
      x.offset.setOffset(this, e, t);
    });var n,
        r,
        o = { top: 0, left: 0 },
        a = this[0],
        s = a && a.ownerDocument;if (s) return (n = s.documentElement, x.contains(n, a) ? (typeof a.getBoundingClientRect !== i && (o = a.getBoundingClientRect()), r = or(s), { top: o.top + (r.pageYOffset || n.scrollTop) - (n.clientTop || 0), left: o.left + (r.pageXOffset || n.scrollLeft) - (n.clientLeft || 0) }) : o);
  }, x.offset = { setOffset: function setOffset(e, t, n) {
      var r = x.css(e, "position");"static" === r && (e.style.position = "relative");var i = x(e),
          o = i.offset(),
          a = x.css(e, "top"),
          s = x.css(e, "left"),
          l = ("absolute" === r || "fixed" === r) && x.inArray("auto", [a, s]) > -1,
          u = {},
          c = {},
          p,
          f;l ? (c = i.position(), p = c.top, f = c.left) : (p = parseFloat(a) || 0, f = parseFloat(s) || 0), x.isFunction(t) && (t = t.call(e, n, o)), null != t.top && (u.top = t.top - o.top + p), null != t.left && (u.left = t.left - o.left + f), "using" in t ? t.using.call(e, u) : i.css(u);
    } }, x.fn.extend({ position: function position() {
      if (this[0]) {
        var e,
            t,
            n = { top: 0, left: 0 },
            r = this[0];return ("fixed" === x.css(r, "position") ? t = r.getBoundingClientRect() : (e = this.offsetParent(), t = this.offset(), x.nodeName(e[0], "html") || (n = e.offset()), n.top += x.css(e[0], "borderTopWidth", !0), n.left += x.css(e[0], "borderLeftWidth", !0)), { top: t.top - n.top - x.css(r, "marginTop", !0), left: t.left - n.left - x.css(r, "marginLeft", !0) });
      }
    }, offsetParent: function offsetParent() {
      return this.map(function () {
        var e = this.offsetParent || s;while (e && !x.nodeName(e, "html") && "static" === x.css(e, "position")) e = e.offsetParent;return e || s;
      });
    } }), x.each({ scrollLeft: "pageXOffset", scrollTop: "pageYOffset" }, function (e, n) {
    var r = /Y/.test(n);x.fn[e] = function (i) {
      return x.access(this, function (e, i, o) {
        var a = or(e);return o === t ? a ? n in a ? a[n] : a.document.documentElement[i] : e[i] : (a ? a.scrollTo(r ? x(a).scrollLeft() : o, r ? o : x(a).scrollTop()) : e[i] = o, t);
      }, e, i, arguments.length, null);
    };
  });function or(e) {
    return x.isWindow(e) ? e : 9 === e.nodeType ? e.defaultView || e.parentWindow : !1;
  }x.each({ Height: "height", Width: "width" }, function (e, n) {
    x.each({ padding: "inner" + e, content: n, "": "outer" + e }, function (r, i) {
      x.fn[i] = function (i, o) {
        var a = arguments.length && (r || "boolean" != typeof i),
            s = r || (i === !0 || o === !0 ? "margin" : "border");return x.access(this, function (n, r, i) {
          var o;return x.isWindow(n) ? n.document.documentElement["client" + e] : 9 === n.nodeType ? (o = n.documentElement, Math.max(n.body["scroll" + e], o["scroll" + e], n.body["offset" + e], o["offset" + e], o["client" + e])) : i === t ? x.css(n, r, s) : x.style(n, r, i, s);
        }, n, a ? i : t, a, null);
      };
    });
  }), x.fn.size = function () {
    return this.length;
  }, x.fn.andSelf = x.fn.addBack, "object" == typeof module && module && "object" == typeof module.exports ? module.exports = x : (e.jQuery = e.$ = x, "function" == typeof define && define.amd && define("jquery", [], function () {
    return x;
  }));
})(window);"use strict";(function(mod){if(typeof exports == "object" && typeof module == "object")module.exports = mod();else if(typeof define == "function" && define.amd)return define([], mod);else this.CodeMirror = mod();})(function(){"use strict";var gecko=/gecko\/\d/i.test(navigator.userAgent);var ie_upto10=/MSIE \d/.test(navigator.userAgent);var ie_11up=/Trident\/(?:[7-9]|\d{2,})\..*rv:(\d+)/.exec(navigator.userAgent);var ie=ie_upto10 || ie_11up;var ie_version=ie && (ie_upto10?document.documentMode || 6:ie_11up[1]);var webkit=/WebKit\//.test(navigator.userAgent);var qtwebkit=webkit && /Qt\/\d+\.\d+/.test(navigator.userAgent);var chrome=/Chrome\//.test(navigator.userAgent);var presto=/Opera\//.test(navigator.userAgent);var safari=/Apple Computer/.test(navigator.vendor);var mac_geMountainLion=/Mac OS X 1\d\D([8-9]|\d\d)\D/.test(navigator.userAgent);var phantom=/PhantomJS/.test(navigator.userAgent);var ios=/AppleWebKit/.test(navigator.userAgent) && /Mobile\/\w+/.test(navigator.userAgent);var mobile=ios || /Android|webOS|BlackBerry|Opera Mini|Opera Mobi|IEMobile/i.test(navigator.userAgent);var mac=ios || /Mac/.test(navigator.platform);var windows=/win/i.test(navigator.platform);var presto_version=presto && navigator.userAgent.match(/Version\/(\d*\.\d*)/);if(presto_version)presto_version = Number(presto_version[1]);if(presto_version && presto_version >= 15){presto = false;webkit = true;}var flipCtrlCmd=mac && (qtwebkit || presto && (presto_version == null || presto_version < 12.11));var captureRightClick=gecko || ie && ie_version >= 9;var sawReadOnlySpans=false, sawCollapsedSpans=false;function CodeMirror(place, options){if(!(this instanceof CodeMirror)){return new CodeMirror(place, options);}this.options = options = options?copyObj(options):{};copyObj(defaults, options, false);setGuttersForLineNumbers(options);var doc=options.value;if(typeof doc == "string")doc = new Doc(doc, options.mode);this.doc = doc;var input=new CodeMirror.inputStyles[options.inputStyle](this);var display=this.display = new Display(place, doc, input);display.wrapper.CodeMirror = this;updateGutters(this);themeChanged(this);if(options.lineWrapping)this.display.wrapper.className += " CodeMirror-wrap";if(options.autofocus && !mobile)display.input.focus();initScrollbars(this);this.state = {keyMaps:[], overlays:[], modeGen:0, overwrite:false, delayingBlurEvent:false, focused:false, suppressEdits:false, pasteIncoming:false, cutIncoming:false, draggingText:false, highlight:new Delayed(), keySeq:null, specialChars:null};var cm=this;if(ie && ie_version < 11)setTimeout(function(){cm.display.input.reset(true);}, 20);registerEventHandlers(this);ensureGlobalHandlers();startOperation(this);this.curOp.forceUpdate = true;attachDoc(this, doc);if(options.autofocus && !mobile || cm.hasFocus())setTimeout(bind(onFocus, this), 20);else onBlur(this);for(var opt in optionHandlers) if(optionHandlers.hasOwnProperty(opt))optionHandlers[opt](this, options[opt], Init);maybeUpdateLineNumberWidth(this);if(options.finishInit)options.finishInit(this);for(var i=0; i < initHooks.length; ++i) initHooks[i](this);endOperation(this);if(webkit && options.lineWrapping && getComputedStyle(display.lineDiv).textRendering == "optimizelegibility")display.lineDiv.style.textRendering = "auto";}function Display(place, doc, input){var d=this;this.input = input;d.scrollbarFiller = elt("div", null, "CodeMirror-scrollbar-filler");d.scrollbarFiller.setAttribute("cm-not-content", "true");d.gutterFiller = elt("div", null, "CodeMirror-gutter-filler");d.gutterFiller.setAttribute("cm-not-content", "true");d.lineDiv = elt("div", null, "CodeMirror-code");d.selectionDiv = elt("div", null, null, "position: relative; z-index: 1");d.cursorDiv = elt("div", null, "CodeMirror-cursors");d.measure = elt("div", null, "CodeMirror-measure");d.lineMeasure = elt("div", null, "CodeMirror-measure");d.lineSpace = elt("div", [d.measure, d.lineMeasure, d.selectionDiv, d.cursorDiv, d.lineDiv], null, "position: relative; outline: none");d.mover = elt("div", [elt("div", [d.lineSpace], "CodeMirror-lines")], null, "position: relative");d.sizer = elt("div", [d.mover], "CodeMirror-sizer");d.sizerWidth = null;d.heightForcer = elt("div", null, null, "position: absolute; height: " + scrollerGap + "px; width: 1px;");d.gutters = elt("div", null, "CodeMirror-gutters");d.lineGutter = null;d.scroller = elt("div", [d.sizer, d.heightForcer, d.gutters], "CodeMirror-scroll");d.scroller.setAttribute("tabIndex", "-1");d.wrapper = elt("div", [d.scrollbarFiller, d.gutterFiller, d.scroller], "CodeMirror");if(ie && ie_version < 8){d.gutters.style.zIndex = -1;d.scroller.style.paddingRight = 0;}if(!webkit && !(gecko && mobile))d.scroller.draggable = true;if(place){if(place.appendChild)place.appendChild(d.wrapper);else place(d.wrapper);}d.viewFrom = d.viewTo = doc.first;d.reportedViewFrom = d.reportedViewTo = doc.first;d.view = [];d.renderedView = null;d.externalMeasured = null;d.viewOffset = 0;d.lastWrapHeight = d.lastWrapWidth = 0;d.updateLineNumbers = null;d.nativeBarWidth = d.barHeight = d.barWidth = 0;d.scrollbarsClipped = false;d.lineNumWidth = d.lineNumInnerWidth = d.lineNumChars = null;d.alignWidgets = false;d.cachedCharWidth = d.cachedTextHeight = d.cachedPaddingH = null;d.maxLine = null;d.maxLineLength = 0;d.maxLineChanged = false;d.wheelDX = d.wheelDY = d.wheelStartX = d.wheelStartY = null;d.shift = false;d.selForContextMenu = null;d.activeTouch = null;input.init(d);}function loadMode(cm){cm.doc.mode = CodeMirror.getMode(cm.options, cm.doc.modeOption);resetModeState(cm);}function resetModeState(cm){cm.doc.iter(function(line){if(line.stateAfter)line.stateAfter = null;if(line.styles)line.styles = null;});cm.doc.frontier = cm.doc.first;startWorker(cm, 100);cm.state.modeGen++;if(cm.curOp)regChange(cm);}function wrappingChanged(cm){if(cm.options.lineWrapping){addClass(cm.display.wrapper, "CodeMirror-wrap");cm.display.sizer.style.minWidth = "";cm.display.sizerWidth = null;}else {rmClass(cm.display.wrapper, "CodeMirror-wrap");findMaxLine(cm);}estimateLineHeights(cm);regChange(cm);clearCaches(cm);setTimeout(function(){updateScrollbars(cm);}, 100);}function estimateHeight(cm){var th=textHeight(cm.display), wrapping=cm.options.lineWrapping;var perLine=wrapping && Math.max(5, cm.display.scroller.clientWidth / charWidth(cm.display) - 3);return function(line){if(lineIsHidden(cm.doc, line))return 0;var widgetsHeight=0;if(line.widgets)for(var i=0; i < line.widgets.length; i++) {if(line.widgets[i].height)widgetsHeight += line.widgets[i].height;}if(wrapping)return widgetsHeight + (Math.ceil(line.text.length / perLine) || 1) * th;else return widgetsHeight + th;};}function estimateLineHeights(cm){var doc=cm.doc, est=estimateHeight(cm);doc.iter(function(line){var estHeight=est(line);if(estHeight != line.height)updateLineHeight(line, estHeight);});}function themeChanged(cm){cm.display.wrapper.className = cm.display.wrapper.className.replace(/\s*cm-s-\S+/g, "") + cm.options.theme.replace(/(^|\s)\s*/g, " cm-s-");clearCaches(cm);}function guttersChanged(cm){updateGutters(cm);regChange(cm);setTimeout(function(){alignHorizontally(cm);}, 20);}function updateGutters(cm){var gutters=cm.display.gutters, specs=cm.options.gutters;removeChildren(gutters);for(var i=0; i < specs.length; ++i) {var gutterClass=specs[i];var gElt=gutters.appendChild(elt("div", null, "CodeMirror-gutter " + gutterClass));if(gutterClass == "CodeMirror-linenumbers"){cm.display.lineGutter = gElt;gElt.style.width = (cm.display.lineNumWidth || 1) + "px";}}gutters.style.display = i?"":"none";updateGutterSpace(cm);}function updateGutterSpace(cm){var width=cm.display.gutters.offsetWidth;cm.display.sizer.style.marginLeft = width + "px";}function lineLength(line){if(line.height == 0){return 0;}var len=line.text.length, merged, cur=line;while(merged = collapsedSpanAtStart(cur)) {var found=merged.find(0, true);cur = found.from.line;len += found.from.ch - found.to.ch;}cur = line;while(merged = collapsedSpanAtEnd(cur)) {var found=merged.find(0, true);len -= cur.text.length - found.from.ch;cur = found.to.line;len += cur.text.length - found.to.ch;}return len;}function findMaxLine(cm){var d=cm.display, doc=cm.doc;d.maxLine = getLine(doc, doc.first);d.maxLineLength = lineLength(d.maxLine);d.maxLineChanged = true;doc.iter(function(line){var len=lineLength(line);if(len > d.maxLineLength){d.maxLineLength = len;d.maxLine = line;}});}function setGuttersForLineNumbers(options){var found=indexOf(options.gutters, "CodeMirror-linenumbers");if(found == -1 && options.lineNumbers){options.gutters = options.gutters.concat(["CodeMirror-linenumbers"]);}else if(found > -1 && !options.lineNumbers){options.gutters = options.gutters.slice(0);options.gutters.splice(found, 1);}}function measureForScrollbars(cm){var d=cm.display, gutterW=d.gutters.offsetWidth;var docH=Math.round(cm.doc.height + paddingVert(cm.display));return {clientHeight:d.scroller.clientHeight, viewHeight:d.wrapper.clientHeight, scrollWidth:d.scroller.scrollWidth, clientWidth:d.scroller.clientWidth, viewWidth:d.wrapper.clientWidth, barLeft:cm.options.fixedGutter?gutterW:0, docHeight:docH, scrollHeight:docH + scrollGap(cm) + d.barHeight, nativeBarWidth:d.nativeBarWidth, gutterWidth:gutterW};}function NativeScrollbars(place, scroll, cm){this.cm = cm;var vert=this.vert = elt("div", [elt("div", null, null, "min-width: 1px")], "CodeMirror-vscrollbar");var horiz=this.horiz = elt("div", [elt("div", null, null, "height: 100%; min-height: 1px")], "CodeMirror-hscrollbar");place(vert);place(horiz);on(vert, "scroll", function(){if(vert.clientHeight)scroll(vert.scrollTop, "vertical");});on(horiz, "scroll", function(){if(horiz.clientWidth)scroll(horiz.scrollLeft, "horizontal");});this.checkedOverlay = false;if(ie && ie_version < 8)this.horiz.style.minHeight = this.vert.style.minWidth = "18px";}NativeScrollbars.prototype = copyObj({update:function update(measure){var needsH=measure.scrollWidth > measure.clientWidth + 1;var needsV=measure.scrollHeight > measure.clientHeight + 1;var sWidth=measure.nativeBarWidth;if(needsV){this.vert.style.display = "block";this.vert.style.bottom = needsH?sWidth + "px":"0";var totalHeight=measure.viewHeight - (needsH?sWidth:0);this.vert.firstChild.style.height = Math.max(0, measure.scrollHeight - measure.clientHeight + totalHeight) + "px";}else {this.vert.style.display = "";this.vert.firstChild.style.height = "0";}if(needsH){this.horiz.style.display = "block";this.horiz.style.right = needsV?sWidth + "px":"0";this.horiz.style.left = measure.barLeft + "px";var totalWidth=measure.viewWidth - measure.barLeft - (needsV?sWidth:0);this.horiz.firstChild.style.width = measure.scrollWidth - measure.clientWidth + totalWidth + "px";}else {this.horiz.style.display = "";this.horiz.firstChild.style.width = "0";}if(!this.checkedOverlay && measure.clientHeight > 0){if(sWidth == 0)this.overlayHack();this.checkedOverlay = true;}return {right:needsV?sWidth:0, bottom:needsH?sWidth:0};}, setScrollLeft:function setScrollLeft(pos){if(this.horiz.scrollLeft != pos)this.horiz.scrollLeft = pos;}, setScrollTop:function setScrollTop(pos){if(this.vert.scrollTop != pos)this.vert.scrollTop = pos;}, overlayHack:function overlayHack(){var w=mac && !mac_geMountainLion?"12px":"18px";this.horiz.style.minHeight = this.vert.style.minWidth = w;var self=this;var barMouseDown=function barMouseDown(e){if(e_target(e) != self.vert && e_target(e) != self.horiz)operation(self.cm, onMouseDown)(e);};on(this.vert, "mousedown", barMouseDown);on(this.horiz, "mousedown", barMouseDown);}, clear:function clear(){var parent=this.horiz.parentNode;parent.removeChild(this.horiz);parent.removeChild(this.vert);}}, NativeScrollbars.prototype);function NullScrollbars(){}NullScrollbars.prototype = copyObj({update:function update(){return {bottom:0, right:0};}, setScrollLeft:function setScrollLeft(){}, setScrollTop:function setScrollTop(){}, clear:function clear(){}}, NullScrollbars.prototype);CodeMirror.scrollbarModel = {native:NativeScrollbars, "null":NullScrollbars};function initScrollbars(cm){if(cm.display.scrollbars){cm.display.scrollbars.clear();if(cm.display.scrollbars.addClass)rmClass(cm.display.wrapper, cm.display.scrollbars.addClass);}cm.display.scrollbars = new CodeMirror.scrollbarModel[cm.options.scrollbarStyle](function(node){cm.display.wrapper.insertBefore(node, cm.display.scrollbarFiller);on(node, "mousedown", function(){if(cm.state.focused)setTimeout(function(){cm.display.input.focus();}, 0);});node.setAttribute("cm-not-content", "true");}, function(pos, axis){if(axis == "horizontal")setScrollLeft(cm, pos);else setScrollTop(cm, pos);}, cm);if(cm.display.scrollbars.addClass)addClass(cm.display.wrapper, cm.display.scrollbars.addClass);}function updateScrollbars(cm, measure){if(!measure)measure = measureForScrollbars(cm);var startWidth=cm.display.barWidth, startHeight=cm.display.barHeight;updateScrollbarsInner(cm, measure);for(var i=0; i < 4 && startWidth != cm.display.barWidth || startHeight != cm.display.barHeight; i++) {if(startWidth != cm.display.barWidth && cm.options.lineWrapping)updateHeightsInViewport(cm);updateScrollbarsInner(cm, measureForScrollbars(cm));startWidth = cm.display.barWidth;startHeight = cm.display.barHeight;}}function updateScrollbarsInner(cm, measure){var d=cm.display;var sizes=d.scrollbars.update(measure);d.sizer.style.paddingRight = (d.barWidth = sizes.right) + "px";d.sizer.style.paddingBottom = (d.barHeight = sizes.bottom) + "px";if(sizes.right && sizes.bottom){d.scrollbarFiller.style.display = "block";d.scrollbarFiller.style.height = sizes.bottom + "px";d.scrollbarFiller.style.width = sizes.right + "px";}else d.scrollbarFiller.style.display = "";if(sizes.bottom && cm.options.coverGutterNextToScrollbar && cm.options.fixedGutter){d.gutterFiller.style.display = "block";d.gutterFiller.style.height = sizes.bottom + "px";d.gutterFiller.style.width = measure.gutterWidth + "px";}else d.gutterFiller.style.display = "";}function visibleLines(display, doc, viewport){var top=viewport && viewport.top != null?Math.max(0, viewport.top):display.scroller.scrollTop;top = Math.floor(top - paddingTop(display));var bottom=viewport && viewport.bottom != null?viewport.bottom:top + display.wrapper.clientHeight;var from=lineAtHeight(doc, top), to=lineAtHeight(doc, bottom);if(viewport && viewport.ensure){var ensureFrom=viewport.ensure.from.line, ensureTo=viewport.ensure.to.line;if(ensureFrom < from){from = ensureFrom;to = lineAtHeight(doc, heightAtLine(getLine(doc, ensureFrom)) + display.wrapper.clientHeight);}else if(Math.min(ensureTo, doc.lastLine()) >= to){from = lineAtHeight(doc, heightAtLine(getLine(doc, ensureTo)) - display.wrapper.clientHeight);to = ensureTo;}}return {from:from, to:Math.max(to, from + 1)};}function alignHorizontally(cm){var display=cm.display, view=display.view;if(!display.alignWidgets && (!display.gutters.firstChild || !cm.options.fixedGutter)){return;}var comp=compensateForHScroll(display) - display.scroller.scrollLeft + cm.doc.scrollLeft;var gutterW=display.gutters.offsetWidth, left=comp + "px";for(var i=0; i < view.length; i++) if(!view[i].hidden){if(cm.options.fixedGutter && view[i].gutter)view[i].gutter.style.left = left;var align=view[i].alignable;if(align)for(var j=0; j < align.length; j++) align[j].style.left = left;}if(cm.options.fixedGutter)display.gutters.style.left = comp + gutterW + "px";}function maybeUpdateLineNumberWidth(cm){if(!cm.options.lineNumbers){return false;}var doc=cm.doc, last=lineNumberFor(cm.options, doc.first + doc.size - 1), display=cm.display;if(last.length != display.lineNumChars){var test=display.measure.appendChild(elt("div", [elt("div", last)], "CodeMirror-linenumber CodeMirror-gutter-elt"));var innerW=test.firstChild.offsetWidth, padding=test.offsetWidth - innerW;display.lineGutter.style.width = "";display.lineNumInnerWidth = Math.max(innerW, display.lineGutter.offsetWidth - padding) + 1;display.lineNumWidth = display.lineNumInnerWidth + padding;display.lineNumChars = display.lineNumInnerWidth?last.length:-1;display.lineGutter.style.width = display.lineNumWidth + "px";updateGutterSpace(cm);return true;}return false;}function lineNumberFor(options, i){return String(options.lineNumberFormatter(i + options.firstLineNumber));}function compensateForHScroll(display){return display.scroller.getBoundingClientRect().left - display.sizer.getBoundingClientRect().left;}function DisplayUpdate(cm, viewport, force){var display=cm.display;this.viewport = viewport;this.visible = visibleLines(display, cm.doc, viewport);this.editorIsHidden = !display.wrapper.offsetWidth;this.wrapperHeight = display.wrapper.clientHeight;this.wrapperWidth = display.wrapper.clientWidth;this.oldDisplayWidth = displayWidth(cm);this.force = force;this.dims = getDimensions(cm);this.events = [];}DisplayUpdate.prototype.signal = function(emitter, type){if(hasHandler(emitter, type))this.events.push(arguments);};DisplayUpdate.prototype.finish = function(){for(var i=0; i < this.events.length; i++) signal.apply(null, this.events[i]);};function maybeClipScrollbars(cm){var display=cm.display;if(!display.scrollbarsClipped && display.scroller.offsetWidth){display.nativeBarWidth = display.scroller.offsetWidth - display.scroller.clientWidth;display.heightForcer.style.height = scrollGap(cm) + "px";display.sizer.style.marginBottom = -display.nativeBarWidth + "px";display.sizer.style.borderRightWidth = scrollGap(cm) + "px";display.scrollbarsClipped = true;}}function updateDisplayIfNeeded(cm, update){var display=cm.display, doc=cm.doc;if(update.editorIsHidden){resetView(cm);return false;}if(!update.force && update.visible.from >= display.viewFrom && update.visible.to <= display.viewTo && (display.updateLineNumbers == null || display.updateLineNumbers >= display.viewTo) && display.renderedView == display.view && countDirtyView(cm) == 0){return false;}if(maybeUpdateLineNumberWidth(cm)){resetView(cm);update.dims = getDimensions(cm);}var end=doc.first + doc.size;var from=Math.max(update.visible.from - cm.options.viewportMargin, doc.first);var to=Math.min(end, update.visible.to + cm.options.viewportMargin);if(display.viewFrom < from && from - display.viewFrom < 20)from = Math.max(doc.first, display.viewFrom);if(display.viewTo > to && display.viewTo - to < 20)to = Math.min(end, display.viewTo);if(sawCollapsedSpans){from = visualLineNo(cm.doc, from);to = visualLineEndNo(cm.doc, to);}var different=from != display.viewFrom || to != display.viewTo || display.lastWrapHeight != update.wrapperHeight || display.lastWrapWidth != update.wrapperWidth;adjustView(cm, from, to);display.viewOffset = heightAtLine(getLine(cm.doc, display.viewFrom));cm.display.mover.style.top = display.viewOffset + "px";var toUpdate=countDirtyView(cm);if(!different && toUpdate == 0 && !update.force && display.renderedView == display.view && (display.updateLineNumbers == null || display.updateLineNumbers >= display.viewTo)){return false;}var focused=activeElt();if(toUpdate > 4)display.lineDiv.style.display = "none";patchDisplay(cm, display.updateLineNumbers, update.dims);if(toUpdate > 4)display.lineDiv.style.display = "";display.renderedView = display.view;if(focused && activeElt() != focused && focused.offsetHeight)focused.focus();removeChildren(display.cursorDiv);removeChildren(display.selectionDiv);display.gutters.style.height = 0;if(different){display.lastWrapHeight = update.wrapperHeight;display.lastWrapWidth = update.wrapperWidth;startWorker(cm, 400);}display.updateLineNumbers = null;return true;}function postUpdateDisplay(cm, update){var viewport=update.viewport;for(var first=true;; first = false) {if(!first || !cm.options.lineWrapping || update.oldDisplayWidth == displayWidth(cm)){if(viewport && viewport.top != null)viewport = {top:Math.min(cm.doc.height + paddingVert(cm.display) - displayHeight(cm), viewport.top)};update.visible = visibleLines(cm.display, cm.doc, viewport);if(update.visible.from >= cm.display.viewFrom && update.visible.to <= cm.display.viewTo)break;}if(!updateDisplayIfNeeded(cm, update))break;updateHeightsInViewport(cm);var barMeasure=measureForScrollbars(cm);updateSelection(cm);setDocumentHeight(cm, barMeasure);updateScrollbars(cm, barMeasure);}update.signal(cm, "update", cm);if(cm.display.viewFrom != cm.display.reportedViewFrom || cm.display.viewTo != cm.display.reportedViewTo){update.signal(cm, "viewportChange", cm, cm.display.viewFrom, cm.display.viewTo);cm.display.reportedViewFrom = cm.display.viewFrom;cm.display.reportedViewTo = cm.display.viewTo;}}function updateDisplaySimple(cm, viewport){var update=new DisplayUpdate(cm, viewport);if(updateDisplayIfNeeded(cm, update)){updateHeightsInViewport(cm);postUpdateDisplay(cm, update);var barMeasure=measureForScrollbars(cm);updateSelection(cm);setDocumentHeight(cm, barMeasure);updateScrollbars(cm, barMeasure);update.finish();}}function setDocumentHeight(cm, measure){cm.display.sizer.style.minHeight = measure.docHeight + "px";var total=measure.docHeight + cm.display.barHeight;cm.display.heightForcer.style.top = total + "px";cm.display.gutters.style.height = Math.max(total + scrollGap(cm), measure.clientHeight) + "px";}function updateHeightsInViewport(cm){var display=cm.display;var prevBottom=display.lineDiv.offsetTop;for(var i=0; i < display.view.length; i++) {var cur=display.view[i], height;if(cur.hidden)continue;if(ie && ie_version < 8){var bot=cur.node.offsetTop + cur.node.offsetHeight;height = bot - prevBottom;prevBottom = bot;}else {var box=cur.node.getBoundingClientRect();height = box.bottom - box.top;}var diff=cur.line.height - height;if(height < 2)height = textHeight(display);if(diff > 0.001 || diff < -0.001){updateLineHeight(cur.line, height);updateWidgetHeight(cur.line);if(cur.rest)for(var j=0; j < cur.rest.length; j++) updateWidgetHeight(cur.rest[j]);}}}function updateWidgetHeight(line){if(line.widgets)for(var i=0; i < line.widgets.length; ++i) line.widgets[i].height = line.widgets[i].node.offsetHeight;}function getDimensions(cm){var d=cm.display, left={}, width={};var gutterLeft=d.gutters.clientLeft;for(var n=d.gutters.firstChild, i=0; n; n = n.nextSibling, ++i) {left[cm.options.gutters[i]] = n.offsetLeft + n.clientLeft + gutterLeft;width[cm.options.gutters[i]] = n.clientWidth;}return {fixedPos:compensateForHScroll(d), gutterTotalWidth:d.gutters.offsetWidth, gutterLeft:left, gutterWidth:width, wrapperWidth:d.wrapper.clientWidth};}function patchDisplay(cm, updateNumbersFrom, dims){var display=cm.display, lineNumbers=cm.options.lineNumbers;var container=display.lineDiv, cur=container.firstChild;function rm(node){var next=node.nextSibling;if(webkit && mac && cm.display.currentWheelTarget == node)node.style.display = "none";else node.parentNode.removeChild(node);return next;}var view=display.view, lineN=display.viewFrom;for(var i=0; i < view.length; i++) {var lineView=view[i];if(lineView.hidden){}else if(!lineView.node || lineView.node.parentNode != container){var node=buildLineElement(cm, lineView, lineN, dims);container.insertBefore(node, cur);}else {while(cur != lineView.node) cur = rm(cur);var updateNumber=lineNumbers && updateNumbersFrom != null && updateNumbersFrom <= lineN && lineView.lineNumber;if(lineView.changes){if(indexOf(lineView.changes, "gutter") > -1)updateNumber = false;updateLineForChanges(cm, lineView, lineN, dims);}if(updateNumber){removeChildren(lineView.lineNumber);lineView.lineNumber.appendChild(document.createTextNode(lineNumberFor(cm.options, lineN)));}cur = lineView.node.nextSibling;}lineN += lineView.size;}while(cur) cur = rm(cur);}function updateLineForChanges(cm, lineView, lineN, dims){for(var j=0; j < lineView.changes.length; j++) {var type=lineView.changes[j];if(type == "text")updateLineText(cm, lineView);else if(type == "gutter")updateLineGutter(cm, lineView, lineN, dims);else if(type == "class")updateLineClasses(lineView);else if(type == "widget")updateLineWidgets(cm, lineView, dims);}lineView.changes = null;}function ensureLineWrapped(lineView){if(lineView.node == lineView.text){lineView.node = elt("div", null, null, "position: relative");if(lineView.text.parentNode)lineView.text.parentNode.replaceChild(lineView.node, lineView.text);lineView.node.appendChild(lineView.text);if(ie && ie_version < 8)lineView.node.style.zIndex = 2;}return lineView.node;}function updateLineBackground(lineView){var cls=lineView.bgClass?lineView.bgClass + " " + (lineView.line.bgClass || ""):lineView.line.bgClass;if(cls)cls += " CodeMirror-linebackground";if(lineView.background){if(cls)lineView.background.className = cls;else {lineView.background.parentNode.removeChild(lineView.background);lineView.background = null;}}else if(cls){var wrap=ensureLineWrapped(lineView);lineView.background = wrap.insertBefore(elt("div", null, cls), wrap.firstChild);}}function getLineContent(cm, lineView){var ext=cm.display.externalMeasured;if(ext && ext.line == lineView.line){cm.display.externalMeasured = null;lineView.measure = ext.measure;return ext.built;}return buildLineContent(cm, lineView);}function updateLineText(cm, lineView){var cls=lineView.text.className;var built=getLineContent(cm, lineView);if(lineView.text == lineView.node)lineView.node = built.pre;lineView.text.parentNode.replaceChild(built.pre, lineView.text);lineView.text = built.pre;if(built.bgClass != lineView.bgClass || built.textClass != lineView.textClass){lineView.bgClass = built.bgClass;lineView.textClass = built.textClass;updateLineClasses(lineView);}else if(cls){lineView.text.className = cls;}}function updateLineClasses(lineView){updateLineBackground(lineView);if(lineView.line.wrapClass)ensureLineWrapped(lineView).className = lineView.line.wrapClass;else if(lineView.node != lineView.text)lineView.node.className = "";var textClass=lineView.textClass?lineView.textClass + " " + (lineView.line.textClass || ""):lineView.line.textClass;lineView.text.className = textClass || "";}function updateLineGutter(cm, lineView, lineN, dims){if(lineView.gutter){lineView.node.removeChild(lineView.gutter);lineView.gutter = null;}var markers=lineView.line.gutterMarkers;if(cm.options.lineNumbers || markers){var wrap=ensureLineWrapped(lineView);var gutterWrap=lineView.gutter = elt("div", null, "CodeMirror-gutter-wrapper", "left: " + (cm.options.fixedGutter?dims.fixedPos:-dims.gutterTotalWidth) + "px; width: " + dims.gutterTotalWidth + "px");cm.display.input.setUneditable(gutterWrap);wrap.insertBefore(gutterWrap, lineView.text);if(lineView.line.gutterClass)gutterWrap.className += " " + lineView.line.gutterClass;if(cm.options.lineNumbers && (!markers || !markers["CodeMirror-linenumbers"]))lineView.lineNumber = gutterWrap.appendChild(elt("div", lineNumberFor(cm.options, lineN), "CodeMirror-linenumber CodeMirror-gutter-elt", "left: " + dims.gutterLeft["CodeMirror-linenumbers"] + "px; width: " + cm.display.lineNumInnerWidth + "px"));if(markers)for(var k=0; k < cm.options.gutters.length; ++k) {var id=cm.options.gutters[k], found=markers.hasOwnProperty(id) && markers[id];if(found)gutterWrap.appendChild(elt("div", [found], "CodeMirror-gutter-elt", "left: " + dims.gutterLeft[id] + "px; width: " + dims.gutterWidth[id] + "px"));}}}function updateLineWidgets(cm, lineView, dims){if(lineView.alignable)lineView.alignable = null;for(var node=lineView.node.firstChild, next; node; node = next) {var next=node.nextSibling;if(node.className == "CodeMirror-linewidget")lineView.node.removeChild(node);}insertLineWidgets(cm, lineView, dims);}function buildLineElement(cm, lineView, lineN, dims){var built=getLineContent(cm, lineView);lineView.text = lineView.node = built.pre;if(built.bgClass)lineView.bgClass = built.bgClass;if(built.textClass)lineView.textClass = built.textClass;updateLineClasses(lineView);updateLineGutter(cm, lineView, lineN, dims);insertLineWidgets(cm, lineView, dims);return lineView.node;}function insertLineWidgets(cm, lineView, dims){insertLineWidgetsFor(cm, lineView.line, lineView, dims, true);if(lineView.rest)for(var i=0; i < lineView.rest.length; i++) insertLineWidgetsFor(cm, lineView.rest[i], lineView, dims, false);}function insertLineWidgetsFor(cm, line, lineView, dims, allowAbove){if(!line.widgets){return;}var wrap=ensureLineWrapped(lineView);for(var i=0, ws=line.widgets; i < ws.length; ++i) {var widget=ws[i], node=elt("div", [widget.node], "CodeMirror-linewidget");if(!widget.handleMouseEvents)node.setAttribute("cm-ignore-events", "true");positionLineWidget(widget, node, lineView, dims);cm.display.input.setUneditable(node);if(allowAbove && widget.above)wrap.insertBefore(node, lineView.gutter || lineView.text);else wrap.appendChild(node);signalLater(widget, "redraw");}}function positionLineWidget(widget, node, lineView, dims){if(widget.noHScroll){(lineView.alignable || (lineView.alignable = [])).push(node);var width=dims.wrapperWidth;node.style.left = dims.fixedPos + "px";if(!widget.coverGutter){width -= dims.gutterTotalWidth;node.style.paddingLeft = dims.gutterTotalWidth + "px";}node.style.width = width + "px";}if(widget.coverGutter){node.style.zIndex = 5;node.style.position = "relative";if(!widget.noHScroll)node.style.marginLeft = -dims.gutterTotalWidth + "px";}}var Pos=CodeMirror.Pos = function(line, ch){if(!(this instanceof Pos))return new Pos(line, ch);this.line = line;this.ch = ch;};var cmp=CodeMirror.cmpPos = function(a, b){return a.line - b.line || a.ch - b.ch;};function copyPos(x){return Pos(x.line, x.ch);}function maxPos(a, b){return cmp(a, b) < 0?b:a;}function minPos(a, b){return cmp(a, b) < 0?a:b;}function ensureFocus(cm){if(!cm.state.focused){cm.display.input.focus();onFocus(cm);}}function isReadOnly(cm){return cm.options.readOnly || cm.doc.cantEdit;}var lastCopied=null;function applyTextInput(cm, inserted, deleted, sel, origin){var doc=cm.doc;cm.display.shift = false;if(!sel)sel = doc.sel;var textLines=splitLines(inserted), multiPaste=null;if(cm.state.pasteIncoming && sel.ranges.length > 1){if(lastCopied && lastCopied.join("\n") == inserted)multiPaste = sel.ranges.length % lastCopied.length == 0 && map(lastCopied, splitLines);else if(textLines.length == sel.ranges.length)multiPaste = map(textLines, function(l){return [l];});}for(var i=sel.ranges.length - 1; i >= 0; i--) {var range=sel.ranges[i];var from=range.from(), to=range.to();if(range.empty()){if(deleted && deleted > 0)from = Pos(from.line, from.ch - deleted);else if(cm.state.overwrite && !cm.state.pasteIncoming)to = Pos(to.line, Math.min(getLine(doc, to.line).text.length, to.ch + lst(textLines).length));}var updateInput=cm.curOp.updateInput;var changeEvent={from:from, to:to, text:multiPaste?multiPaste[i % multiPaste.length]:textLines, origin:origin || (cm.state.pasteIncoming?"paste":cm.state.cutIncoming?"cut":"+input")};makeChange(cm.doc, changeEvent);signalLater(cm, "inputRead", cm, changeEvent);}if(inserted && !cm.state.pasteIncoming)triggerElectric(cm, inserted);ensureCursorVisible(cm);cm.curOp.updateInput = updateInput;cm.curOp.typing = true;cm.state.pasteIncoming = cm.state.cutIncoming = false;}function triggerElectric(cm, inserted){if(!cm.options.electricChars || !cm.options.smartIndent){return;}var sel=cm.doc.sel;for(var i=sel.ranges.length - 1; i >= 0; i--) {var range=sel.ranges[i];if(range.head.ch > 100 || i && sel.ranges[i - 1].head.line == range.head.line)continue;var mode=cm.getModeAt(range.head);var indented=false;if(mode.electricChars){for(var j=0; j < mode.electricChars.length; j++) if(inserted.indexOf(mode.electricChars.charAt(j)) > -1){indented = indentLine(cm, range.head.line, "smart");break;}}else if(mode.electricInput){if(mode.electricInput.test(getLine(cm.doc, range.head.line).text.slice(0, range.head.ch)))indented = indentLine(cm, range.head.line, "smart");}if(indented)signalLater(cm, "electricInput", cm, range.head.line);}}function copyableRanges(cm){var text=[], ranges=[];for(var i=0; i < cm.doc.sel.ranges.length; i++) {var line=cm.doc.sel.ranges[i].head.line;var lineRange={anchor:Pos(line, 0), head:Pos(line + 1, 0)};ranges.push(lineRange);text.push(cm.getRange(lineRange.anchor, lineRange.head));}return {text:text, ranges:ranges};}function disableBrowserMagic(field){field.setAttribute("autocorrect", "off");field.setAttribute("autocapitalize", "off");field.setAttribute("spellcheck", "false");}function TextareaInput(cm){this.cm = cm;this.prevInput = "";this.pollingFast = false;this.polling = new Delayed();this.inaccurateSelection = false;this.hasSelection = false;this.composing = null;};function hiddenTextarea(){var te=elt("textarea", null, null, "position: absolute; padding: 0; width: 1px; height: 1em; outline: none");var div=elt("div", [te], null, "overflow: hidden; position: relative; width: 3px; height: 0px;");if(webkit)te.style.width = "1000px";else te.setAttribute("wrap", "off");if(ios)te.style.border = "1px solid black";disableBrowserMagic(te);return div;}TextareaInput.prototype = copyObj({init:function init(display){var input=this, cm=this.cm;var div=this.wrapper = hiddenTextarea();var te=this.textarea = div.firstChild;display.wrapper.insertBefore(div, display.wrapper.firstChild);if(ios)te.style.width = "0px";on(te, "input", function(){if(ie && ie_version >= 9 && input.hasSelection)input.hasSelection = null;input.poll();});on(te, "paste", function(){if(webkit && !cm.state.fakedLastChar && !(new Date() - cm.state.lastMiddleDown < 200)){var start=te.selectionStart, end=te.selectionEnd;te.value += "$";te.selectionEnd = end;te.selectionStart = start;cm.state.fakedLastChar = true;}cm.state.pasteIncoming = true;input.fastPoll();});function prepareCopyCut(e){if(cm.somethingSelected()){lastCopied = cm.getSelections();if(input.inaccurateSelection){input.prevInput = "";input.inaccurateSelection = false;te.value = lastCopied.join("\n");selectInput(te);}}else if(!cm.options.lineWiseCopyCut){return;}else {var ranges=copyableRanges(cm);lastCopied = ranges.text;if(e.type == "cut"){cm.setSelections(ranges.ranges, null, sel_dontScroll);}else {input.prevInput = "";te.value = ranges.text.join("\n");selectInput(te);}}if(e.type == "cut")cm.state.cutIncoming = true;}on(te, "cut", prepareCopyCut);on(te, "copy", prepareCopyCut);on(display.scroller, "paste", function(e){if(eventInWidget(display, e))return;cm.state.pasteIncoming = true;input.focus();});on(display.lineSpace, "selectstart", function(e){if(!eventInWidget(display, e))e_preventDefault(e);});on(te, "compositionstart", function(){var start=cm.getCursor("from");input.composing = {start:start, range:cm.markText(start, cm.getCursor("to"), {className:"CodeMirror-composing"})};});on(te, "compositionend", function(){if(input.composing){input.poll();input.composing.range.clear();input.composing = null;}});}, prepareSelection:(function(_prepareSelection){var _prepareSelectionWrapper=function prepareSelection(){return _prepareSelection.apply(this, arguments);};_prepareSelectionWrapper.toString = function(){return _prepareSelection.toString();};return _prepareSelectionWrapper;})(function(){var cm=this.cm, display=cm.display, doc=cm.doc;var result=prepareSelection(cm);if(cm.options.moveInputWithCursor){var headPos=cursorCoords(cm, doc.sel.primary().head, "div");var wrapOff=display.wrapper.getBoundingClientRect(), lineOff=display.lineDiv.getBoundingClientRect();result.teTop = Math.max(0, Math.min(display.wrapper.clientHeight - 10, headPos.top + lineOff.top - wrapOff.top));result.teLeft = Math.max(0, Math.min(display.wrapper.clientWidth - 10, headPos.left + lineOff.left - wrapOff.left));}return result;}), showSelection:function showSelection(drawn){var cm=this.cm, display=cm.display;removeChildrenAndAdd(display.cursorDiv, drawn.cursors);removeChildrenAndAdd(display.selectionDiv, drawn.selection);if(drawn.teTop != null){this.wrapper.style.top = drawn.teTop + "px";this.wrapper.style.left = drawn.teLeft + "px";}}, reset:function reset(typing){if(this.contextMenuPending){return;}var minimal, selected, cm=this.cm, doc=cm.doc;if(cm.somethingSelected()){this.prevInput = "";var range=doc.sel.primary();minimal = hasCopyEvent && (range.to().line - range.from().line > 100 || (selected = cm.getSelection()).length > 1000);var content=minimal?"-":selected || cm.getSelection();this.textarea.value = content;if(cm.state.focused)selectInput(this.textarea);if(ie && ie_version >= 9)this.hasSelection = content;}else if(!typing){this.prevInput = this.textarea.value = "";if(ie && ie_version >= 9)this.hasSelection = null;}this.inaccurateSelection = minimal;}, getField:function getField(){return this.textarea;}, supportsTouch:function supportsTouch(){return false;}, focus:function focus(){if(this.cm.options.readOnly != "nocursor" && (!mobile || activeElt() != this.textarea)){try{this.textarea.focus();}catch(e) {}}}, blur:function blur(){this.textarea.blur();}, resetPosition:function resetPosition(){this.wrapper.style.top = this.wrapper.style.left = 0;}, receivedFocus:function receivedFocus(){this.slowPoll();}, slowPoll:function slowPoll(){var input=this;if(input.pollingFast){return;}input.polling.set(this.cm.options.pollInterval, function(){input.poll();if(input.cm.state.focused)input.slowPoll();});}, fastPoll:function fastPoll(){var missed=false, input=this;input.pollingFast = true;function p(){var changed=input.poll();if(!changed && !missed){missed = true;input.polling.set(60, p);}else {input.pollingFast = false;input.slowPoll();}}input.polling.set(20, p);}, poll:function poll(){var cm=this.cm, input=this.textarea, prevInput=this.prevInput;if(!cm.state.focused || hasSelection(input) && !prevInput || isReadOnly(cm) || cm.options.disableInput || cm.state.keySeq){return false;}if(cm.state.pasteIncoming && cm.state.fakedLastChar){input.value = input.value.substring(0, input.value.length - 1);cm.state.fakedLastChar = false;}var text=input.value;if(text == prevInput && !cm.somethingSelected()){return false;}if(ie && ie_version >= 9 && this.hasSelection === text || mac && /[\uf700-\uf7ff]/.test(text)){cm.display.input.reset();return false;}if(cm.doc.sel == cm.display.selForContextMenu){var first=text.charCodeAt(0);if(first == 8203 && !prevInput)prevInput = "​";if(first == 8666){this.reset();return this.cm.execCommand("undo");}}var same=0, l=Math.min(prevInput.length, text.length);while(same < l && prevInput.charCodeAt(same) == text.charCodeAt(same)) ++same;var self=this;runInOp(cm, function(){applyTextInput(cm, text.slice(same), prevInput.length - same, null, self.composing?"*compose":null);if(text.length > 1000 || text.indexOf("\n") > -1)input.value = self.prevInput = "";else self.prevInput = text;if(self.composing){self.composing.range.clear();self.composing.range = cm.markText(self.composing.start, cm.getCursor("to"), {className:"CodeMirror-composing"});}});return true;}, ensurePolled:function ensurePolled(){if(this.pollingFast && this.poll())this.pollingFast = false;}, onKeyPress:function onKeyPress(){if(ie && ie_version >= 9)this.hasSelection = null;this.fastPoll();}, onContextMenu:function onContextMenu(e){var input=this, cm=input.cm, display=cm.display, te=input.textarea;var pos=posFromMouse(cm, e), scrollPos=display.scroller.scrollTop;if(!pos || presto){return;}var reset=cm.options.resetSelectionOnContextMenu;if(reset && cm.doc.sel.contains(pos) == -1)operation(cm, setSelection)(cm.doc, simpleSelection(pos), sel_dontScroll);var oldCSS=te.style.cssText;input.wrapper.style.position = "absolute";te.style.cssText = "position: fixed; width: 30px; height: 30px; top: " + (e.clientY - 5) + "px; left: " + (e.clientX - 5) + "px; z-index: 1000; background: " + (ie?"rgba(255, 255, 255, .05)":"transparent") + "; outline: none; border-width: 0; outline: none; overflow: hidden; opacity: .05; filter: alpha(opacity=5);";if(webkit)var oldScrollY=window.scrollY;display.input.focus();if(webkit)window.scrollTo(null, oldScrollY);display.input.reset();if(!cm.somethingSelected())te.value = input.prevInput = " ";input.contextMenuPending = true;display.selForContextMenu = cm.doc.sel;clearTimeout(display.detectingSelectAll);function prepareSelectAllHack(){if(te.selectionStart != null){var selected=cm.somethingSelected();var extval="​" + (selected?te.value:"");te.value = "⇚";te.value = extval;input.prevInput = selected?"":"​";te.selectionStart = 1;te.selectionEnd = extval.length;display.selForContextMenu = cm.doc.sel;}}function rehide(){input.contextMenuPending = false;input.wrapper.style.position = "relative";te.style.cssText = oldCSS;if(ie && ie_version < 9)display.scrollbars.setScrollTop(display.scroller.scrollTop = scrollPos);if(te.selectionStart != null){if(!ie || ie && ie_version < 9)prepareSelectAllHack();var i=0, poll=(function(_poll){var _pollWrapper=function poll(){return _poll.apply(this, arguments);};_pollWrapper.toString = function(){return _poll.toString();};return _pollWrapper;})(function(){if(display.selForContextMenu == cm.doc.sel && te.selectionStart == 0 && te.selectionEnd > 0 && input.prevInput == "​")operation(cm, commands.selectAll)(cm);else if(i++ < 10)display.detectingSelectAll = setTimeout(poll, 500);else display.input.reset();});display.detectingSelectAll = setTimeout(poll, 200);}}if(ie && ie_version >= 9)prepareSelectAllHack();if(captureRightClick){e_stop(e);var mouseup=(function(_mouseup){var _mouseupWrapper=function mouseup(){return _mouseup.apply(this, arguments);};_mouseupWrapper.toString = function(){return _mouseup.toString();};return _mouseupWrapper;})(function(){off(window, "mouseup", mouseup);setTimeout(rehide, 20);});on(window, "mouseup", mouseup);}else {setTimeout(rehide, 50);}}, setUneditable:nothing, needsContentAttribute:false}, TextareaInput.prototype);function ContentEditableInput(cm){this.cm = cm;this.lastAnchorNode = this.lastAnchorOffset = this.lastFocusNode = this.lastFocusOffset = null;this.polling = new Delayed();this.gracePeriod = false;}ContentEditableInput.prototype = copyObj({init:function init(display){var input=this, cm=input.cm;var div=input.div = display.lineDiv;div.contentEditable = "true";disableBrowserMagic(div);on(div, "paste", function(e){var pasted=e.clipboardData && e.clipboardData.getData("text/plain");if(pasted){e.preventDefault();cm.replaceSelection(pasted, null, "paste");}});on(div, "compositionstart", function(e){var data=e.data;input.composing = {sel:cm.doc.sel, data:data, startData:data};if(!data)return;var prim=cm.doc.sel.primary();var line=cm.getLine(prim.head.line);var found=line.indexOf(data, Math.max(0, prim.head.ch - data.length));if(found > -1 && found <= prim.head.ch)input.composing.sel = simpleSelection(Pos(prim.head.line, found), Pos(prim.head.line, found + data.length));});on(div, "compositionupdate", function(e){input.composing.data = e.data;});on(div, "compositionend", function(e){var ours=input.composing;if(!ours)return;if(e.data != ours.startData && !/\u200b/.test(e.data))ours.data = e.data;setTimeout(function(){if(!ours.handled)input.applyComposition(ours);if(input.composing == ours)input.composing = null;}, 50);});on(div, "touchstart", function(){input.forceCompositionEnd();});on(div, "input", function(){if(input.composing)return;if(!input.pollContent())runInOp(input.cm, function(){regChange(cm);});});function onCopyCut(e){if(cm.somethingSelected()){lastCopied = cm.getSelections();if(e.type == "cut")cm.replaceSelection("", null, "cut");}else if(!cm.options.lineWiseCopyCut){return;}else {var ranges=copyableRanges(cm);lastCopied = ranges.text;if(e.type == "cut"){cm.operation(function(){cm.setSelections(ranges.ranges, 0, sel_dontScroll);cm.replaceSelection("", null, "cut");});}}if(e.clipboardData && !ios){e.preventDefault();e.clipboardData.clearData();e.clipboardData.setData("text/plain", lastCopied.join("\n"));}else {var kludge=hiddenTextarea(), te=kludge.firstChild;cm.display.lineSpace.insertBefore(kludge, cm.display.lineSpace.firstChild);te.value = lastCopied.join("\n");var hadFocus=document.activeElement;selectInput(te);setTimeout(function(){cm.display.lineSpace.removeChild(kludge);hadFocus.focus();}, 50);}}on(div, "copy", onCopyCut);on(div, "cut", onCopyCut);}, prepareSelection:(function(_prepareSelection){var _prepareSelectionWrapper=function prepareSelection(){return _prepareSelection.apply(this, arguments);};_prepareSelectionWrapper.toString = function(){return _prepareSelection.toString();};return _prepareSelectionWrapper;})(function(){var result=prepareSelection(this.cm, false);result.focus = this.cm.state.focused;return result;}), showSelection:function showSelection(info){if(!info || !this.cm.display.view.length){return;}if(info.focus)this.showPrimarySelection();this.showMultipleSelections(info);}, showPrimarySelection:function showPrimarySelection(){var sel=window.getSelection(), prim=this.cm.doc.sel.primary();var curAnchor=domToPos(this.cm, sel.anchorNode, sel.anchorOffset);var curFocus=domToPos(this.cm, sel.focusNode, sel.focusOffset);if(curAnchor && !curAnchor.bad && curFocus && !curFocus.bad && cmp(minPos(curAnchor, curFocus), prim.from()) == 0 && cmp(maxPos(curAnchor, curFocus), prim.to()) == 0){return;}var start=posToDOM(this.cm, prim.from());var end=posToDOM(this.cm, prim.to());if(!start && !end){return;}var view=this.cm.display.view;var old=sel.rangeCount && sel.getRangeAt(0);if(!start){start = {node:view[0].measure.map[2], offset:0};}else if(!end){var measure=view[view.length - 1].measure;var map=measure.maps?measure.maps[measure.maps.length - 1]:measure.map;end = {node:map[map.length - 1], offset:map[map.length - 2] - map[map.length - 3]};}try{var rng=range(start.node, start.offset, end.offset, end.node);}catch(e) {}if(rng){sel.removeAllRanges();sel.addRange(rng);if(old && sel.anchorNode == null)sel.addRange(old);else if(gecko)this.startGracePeriod();}this.rememberSelection();}, startGracePeriod:function startGracePeriod(){var input=this;clearTimeout(this.gracePeriod);this.gracePeriod = setTimeout(function(){input.gracePeriod = false;if(input.selectionChanged())input.cm.operation(function(){input.cm.curOp.selectionChanged = true;});}, 20);}, showMultipleSelections:function showMultipleSelections(info){removeChildrenAndAdd(this.cm.display.cursorDiv, info.cursors);removeChildrenAndAdd(this.cm.display.selectionDiv, info.selection);}, rememberSelection:function rememberSelection(){var sel=window.getSelection();this.lastAnchorNode = sel.anchorNode;this.lastAnchorOffset = sel.anchorOffset;this.lastFocusNode = sel.focusNode;this.lastFocusOffset = sel.focusOffset;}, selectionInEditor:function selectionInEditor(){var sel=window.getSelection();if(!sel.rangeCount){return false;}var node=sel.getRangeAt(0).commonAncestorContainer;return contains(this.div, node);}, focus:function focus(){if(this.cm.options.readOnly != "nocursor")this.div.focus();}, blur:function blur(){this.div.blur();}, getField:function getField(){return this.div;}, supportsTouch:function supportsTouch(){return true;}, receivedFocus:function receivedFocus(){var input=this;if(this.selectionInEditor())this.pollSelection();else runInOp(this.cm, function(){input.cm.curOp.selectionChanged = true;});function poll(){if(input.cm.state.focused){input.pollSelection();input.polling.set(input.cm.options.pollInterval, poll);}}this.polling.set(this.cm.options.pollInterval, poll);}, selectionChanged:function selectionChanged(){var sel=window.getSelection();return sel.anchorNode != this.lastAnchorNode || sel.anchorOffset != this.lastAnchorOffset || sel.focusNode != this.lastFocusNode || sel.focusOffset != this.lastFocusOffset;}, pollSelection:function pollSelection(){if(!this.composing && !this.gracePeriod && this.selectionChanged()){var sel=window.getSelection(), cm=this.cm;this.rememberSelection();var anchor=domToPos(cm, sel.anchorNode, sel.anchorOffset);var head=domToPos(cm, sel.focusNode, sel.focusOffset);if(anchor && head)runInOp(cm, function(){setSelection(cm.doc, simpleSelection(anchor, head), sel_dontScroll);if(anchor.bad || head.bad)cm.curOp.selectionChanged = true;});}}, pollContent:function pollContent(){var cm=this.cm, display=cm.display, sel=cm.doc.sel.primary();var from=sel.from(), to=sel.to();if(from.line < display.viewFrom || to.line > display.viewTo - 1){return false;}var fromIndex;if(from.line == display.viewFrom || (fromIndex = findViewIndex(cm, from.line)) == 0){var fromLine=lineNo(display.view[0].line);var fromNode=display.view[0].node;}else {var fromLine=lineNo(display.view[fromIndex].line);var fromNode=display.view[fromIndex - 1].node.nextSibling;}var toIndex=findViewIndex(cm, to.line);if(toIndex == display.view.length - 1){var toLine=display.viewTo - 1;var toNode=display.view[toIndex].node;}else {var toLine=lineNo(display.view[toIndex + 1].line) - 1;var toNode=display.view[toIndex + 1].node.previousSibling;}var newText=splitLines(domTextBetween(cm, fromNode, toNode, fromLine, toLine));var oldText=getBetween(cm.doc, Pos(fromLine, 0), Pos(toLine, getLine(cm.doc, toLine).text.length));while(newText.length > 1 && oldText.length > 1) {if(lst(newText) == lst(oldText)){newText.pop();oldText.pop();toLine--;}else if(newText[0] == oldText[0]){newText.shift();oldText.shift();fromLine++;}else break;}var cutFront=0, cutEnd=0;var newTop=newText[0], oldTop=oldText[0], maxCutFront=Math.min(newTop.length, oldTop.length);while(cutFront < maxCutFront && newTop.charCodeAt(cutFront) == oldTop.charCodeAt(cutFront)) ++cutFront;var newBot=lst(newText), oldBot=lst(oldText);var maxCutEnd=Math.min(newBot.length - (newText.length == 1?cutFront:0), oldBot.length - (oldText.length == 1?cutFront:0));while(cutEnd < maxCutEnd && newBot.charCodeAt(newBot.length - cutEnd - 1) == oldBot.charCodeAt(oldBot.length - cutEnd - 1)) ++cutEnd;newText[newText.length - 1] = newBot.slice(0, newBot.length - cutEnd);newText[0] = newText[0].slice(cutFront);var chFrom=Pos(fromLine, cutFront);var chTo=Pos(toLine, oldText.length?lst(oldText).length - cutEnd:0);if(newText.length > 1 || newText[0] || cmp(chFrom, chTo)){replaceRange(cm.doc, newText, chFrom, chTo, "+input");return true;}}, ensurePolled:function ensurePolled(){this.forceCompositionEnd();}, reset:function reset(){this.forceCompositionEnd();}, forceCompositionEnd:function forceCompositionEnd(){if(!this.composing || this.composing.handled){return;}this.applyComposition(this.composing);this.composing.handled = true;this.div.blur();this.div.focus();}, applyComposition:function applyComposition(composing){if(composing.data && composing.data != composing.startData)operation(this.cm, applyTextInput)(this.cm, composing.data, 0, composing.sel);}, setUneditable:function setUneditable(node){node.setAttribute("contenteditable", "false");}, onKeyPress:function onKeyPress(e){e.preventDefault();operation(this.cm, applyTextInput)(this.cm, String.fromCharCode(e.charCode == null?e.keyCode:e.charCode), 0);}, onContextMenu:nothing, resetPosition:nothing, needsContentAttribute:true}, ContentEditableInput.prototype);function posToDOM(cm, pos){var view=findViewForLine(cm, pos.line);if(!view || view.hidden){return null;}var line=getLine(cm.doc, pos.line);var info=mapFromLineView(view, line, pos.line);var order=getOrder(line), side="left";if(order){var partPos=getBidiPartAt(order, pos.ch);side = partPos % 2?"right":"left";}var result=nodeAndOffsetInLineMap(info.map, pos.ch, side);result.offset = result.collapse == "right"?result.end:result.start;return result;}function badPos(pos, bad){if(bad)pos.bad = true;return pos;}function domToPos(cm, node, offset){var lineNode;if(node == cm.display.lineDiv){lineNode = cm.display.lineDiv.childNodes[offset];if(!lineNode){return badPos(cm.clipPos(Pos(cm.display.viewTo - 1)), true);}node = null;offset = 0;}else {for(lineNode = node;; lineNode = lineNode.parentNode) {if(!lineNode || lineNode == cm.display.lineDiv){return null;}if(lineNode.parentNode && lineNode.parentNode == cm.display.lineDiv)break;}}for(var i=0; i < cm.display.view.length; i++) {var lineView=cm.display.view[i];if(lineView.node == lineNode){return locateNodeInLineView(lineView, node, offset);}}}function locateNodeInLineView(lineView, node, offset){var wrapper=lineView.text.firstChild, bad=false;if(!node || !contains(wrapper, node)){return badPos(Pos(lineNo(lineView.line), 0), true);}if(node == wrapper){bad = true;node = wrapper.childNodes[offset];offset = 0;if(!node){var line=lineView.rest?lst(lineView.rest):lineView.line;return badPos(Pos(lineNo(line), line.text.length), bad);}}var textNode=node.nodeType == 3?node:null, topNode=node;if(!textNode && node.childNodes.length == 1 && node.firstChild.nodeType == 3){textNode = node.firstChild;if(offset)offset = textNode.nodeValue.length;}while(topNode.parentNode != wrapper) topNode = topNode.parentNode;var measure=lineView.measure, maps=measure.maps;function find(textNode, topNode, offset){for(var i=-1; i < (maps?maps.length:0); i++) {var map=i < 0?measure.map:maps[i];for(var j=0; j < map.length; j += 3) {var curNode=map[j + 2];if(curNode == textNode || curNode == topNode){var line=lineNo(i < 0?lineView.line:lineView.rest[i]);var ch=map[j] + offset;if(offset < 0 || curNode != textNode)ch = map[j + (offset?1:0)];return Pos(line, ch);}}}}var found=find(textNode, topNode, offset);if(found){return badPos(found, bad);}for(var after=topNode.nextSibling, dist=textNode?textNode.nodeValue.length - offset:0; after; after = after.nextSibling) {found = find(after, after.firstChild, 0);if(found){return badPos(Pos(found.line, found.ch - dist), bad);}else dist += after.textContent.length;}for(var before=topNode.previousSibling, dist=offset; before; before = before.previousSibling) {found = find(before, before.firstChild, -1);if(found){return badPos(Pos(found.line, found.ch + dist), bad);}else dist += after.textContent.length;}}function domTextBetween(cm, from, to, fromLine, toLine){var text="", closing=false;function recognizeMarker(id){return function(marker){return marker.id == id;};}function walk(node){if(node.nodeType == 1){var cmText=node.getAttribute("cm-text");if(cmText != null){if(cmText == "")cmText = node.textContent.replace(/\u200b/g, "");text += cmText;return;}var markerID=node.getAttribute("cm-marker"), range;if(markerID){var found=cm.findMarks(Pos(fromLine, 0), Pos(toLine + 1, 0), recognizeMarker(+markerID));if(found.length && (range = found[0].find()))text += getBetween(cm.doc, range.from, range.to).join("\n");return;}if(node.getAttribute("contenteditable") == "false"){return;}for(var i=0; i < node.childNodes.length; i++) walk(node.childNodes[i]);if(/^(pre|div|p)$/i.test(node.nodeName))closing = true;}else if(node.nodeType == 3){var val=node.nodeValue;if(!val){return;}if(closing){text += "\n";closing = false;}text += val;}}for(;;) {walk(from);if(from == to)break;from = from.nextSibling;}return text;}CodeMirror.inputStyles = {textarea:TextareaInput, contenteditable:ContentEditableInput};function Selection(ranges, primIndex){this.ranges = ranges;this.primIndex = primIndex;}Selection.prototype = {primary:function primary(){return this.ranges[this.primIndex];}, equals:function equals(other){if(other == this){return true;}if(other.primIndex != this.primIndex || other.ranges.length != this.ranges.length){return false;}for(var i=0; i < this.ranges.length; i++) {var here=this.ranges[i], there=other.ranges[i];if(cmp(here.anchor, there.anchor) != 0 || cmp(here.head, there.head) != 0){return false;}}return true;}, deepCopy:function deepCopy(){for(var out=[], i=0; i < this.ranges.length; i++) out[i] = new Range(copyPos(this.ranges[i].anchor), copyPos(this.ranges[i].head));return new Selection(out, this.primIndex);}, somethingSelected:function somethingSelected(){for(var i=0; i < this.ranges.length; i++) if(!this.ranges[i].empty()){return true;}return false;}, contains:function contains(pos, end){if(!end)end = pos;for(var i=0; i < this.ranges.length; i++) {var range=this.ranges[i];if(cmp(end, range.from()) >= 0 && cmp(pos, range.to()) <= 0){return i;}}return -1;}};function Range(anchor, head){this.anchor = anchor;this.head = head;}Range.prototype = {from:function from(){return minPos(this.anchor, this.head);}, to:function to(){return maxPos(this.anchor, this.head);}, empty:function empty(){return this.head.line == this.anchor.line && this.head.ch == this.anchor.ch;}};function normalizeSelection(ranges, primIndex){var prim=ranges[primIndex];ranges.sort(function(a, b){return cmp(a.from(), b.from());});primIndex = indexOf(ranges, prim);for(var i=1; i < ranges.length; i++) {var cur=ranges[i], prev=ranges[i - 1];if(cmp(prev.to(), cur.from()) >= 0){var from=minPos(prev.from(), cur.from()), to=maxPos(prev.to(), cur.to());var inv=prev.empty()?cur.from() == cur.head:prev.from() == prev.head;if(i <= primIndex)--primIndex;ranges.splice(--i, 2, new Range(inv?to:from, inv?from:to));}}return new Selection(ranges, primIndex);}function simpleSelection(anchor, head){return new Selection([new Range(anchor, head || anchor)], 0);}function clipLine(doc, n){return Math.max(doc.first, Math.min(n, doc.first + doc.size - 1));}function clipPos(doc, pos){if(pos.line < doc.first){return Pos(doc.first, 0);}var last=doc.first + doc.size - 1;if(pos.line > last){return Pos(last, getLine(doc, last).text.length);}return clipToLen(pos, getLine(doc, pos.line).text.length);}function clipToLen(pos, linelen){var ch=pos.ch;if(ch == null || ch > linelen){return Pos(pos.line, linelen);}else if(ch < 0){return Pos(pos.line, 0);}else {return pos;}}function isLine(doc, l){return l >= doc.first && l < doc.first + doc.size;}function clipPosArray(doc, array){for(var out=[], i=0; i < array.length; i++) out[i] = clipPos(doc, array[i]);return out;}function extendRange(doc, range, head, other){if(doc.cm && doc.cm.display.shift || doc.extend){var anchor=range.anchor;if(other){var posBefore=cmp(head, anchor) < 0;if(posBefore != cmp(other, anchor) < 0){anchor = head;head = other;}else if(posBefore != cmp(head, other) < 0){head = other;}}return new Range(anchor, head);}else {return new Range(other || head, head);}}function extendSelection(doc, head, other, options){setSelection(doc, new Selection([extendRange(doc, doc.sel.primary(), head, other)], 0), options);}function extendSelections(doc, heads, options){for(var out=[], i=0; i < doc.sel.ranges.length; i++) out[i] = extendRange(doc, doc.sel.ranges[i], heads[i], null);var newSel=normalizeSelection(out, doc.sel.primIndex);setSelection(doc, newSel, options);}function replaceOneSelection(doc, i, range, options){var ranges=doc.sel.ranges.slice(0);ranges[i] = range;setSelection(doc, normalizeSelection(ranges, doc.sel.primIndex), options);}function setSimpleSelection(doc, anchor, head, options){setSelection(doc, simpleSelection(anchor, head), options);}function filterSelectionChange(doc, sel){var obj={ranges:sel.ranges, update:function update(ranges){this.ranges = [];for(var i=0; i < ranges.length; i++) this.ranges[i] = new Range(clipPos(doc, ranges[i].anchor), clipPos(doc, ranges[i].head));}};signal(doc, "beforeSelectionChange", doc, obj);if(doc.cm)signal(doc.cm, "beforeSelectionChange", doc.cm, obj);if(obj.ranges != sel.ranges){return normalizeSelection(obj.ranges, obj.ranges.length - 1);}else {return sel;}}function setSelectionReplaceHistory(doc, sel, options){var done=doc.history.done, last=lst(done);if(last && last.ranges){done[done.length - 1] = sel;setSelectionNoUndo(doc, sel, options);}else {setSelection(doc, sel, options);}}function setSelection(doc, sel, options){setSelectionNoUndo(doc, sel, options);addSelectionToHistory(doc, doc.sel, doc.cm?doc.cm.curOp.id:NaN, options);}function setSelectionNoUndo(doc, sel, options){if(hasHandler(doc, "beforeSelectionChange") || doc.cm && hasHandler(doc.cm, "beforeSelectionChange"))sel = filterSelectionChange(doc, sel);var bias=options && options.bias || (cmp(sel.primary().head, doc.sel.primary().head) < 0?-1:1);setSelectionInner(doc, skipAtomicInSelection(doc, sel, bias, true));if(!(options && options.scroll === false) && doc.cm)ensureCursorVisible(doc.cm);}function setSelectionInner(doc, sel){if(sel.equals(doc.sel)){return;}doc.sel = sel;if(doc.cm){doc.cm.curOp.updateInput = doc.cm.curOp.selectionChanged = true;signalCursorActivity(doc.cm);}signalLater(doc, "cursorActivity", doc);}function reCheckSelection(doc){setSelectionInner(doc, skipAtomicInSelection(doc, doc.sel, null, false), sel_dontScroll);}function skipAtomicInSelection(doc, sel, bias, mayClear){var out;for(var i=0; i < sel.ranges.length; i++) {var range=sel.ranges[i];var newAnchor=skipAtomic(doc, range.anchor, bias, mayClear);var newHead=skipAtomic(doc, range.head, bias, mayClear);if(out || newAnchor != range.anchor || newHead != range.head){if(!out)out = sel.ranges.slice(0, i);out[i] = new Range(newAnchor, newHead);}}return out?normalizeSelection(out, sel.primIndex):sel;}function skipAtomic(_x, _x2, _x3, _x4){var _again=true;_function: while(_again) {_again = false;var doc=_x, pos=_x2, bias=_x3, mayClear=_x4;flipped = curPos = dir = line = i = sp = m = newPos = undefined;var flipped=false, curPos=pos;var dir=bias || 1;doc.cantEdit = false;search: for(;;) {var line=getLine(doc, curPos.line);if(line.markedSpans){for(var i=0; i < line.markedSpans.length; ++i) {var sp=line.markedSpans[i], m=sp.marker;if((sp.from == null || (m.inclusiveLeft?sp.from <= curPos.ch:sp.from < curPos.ch)) && (sp.to == null || (m.inclusiveRight?sp.to >= curPos.ch:sp.to > curPos.ch))){if(mayClear){signal(m, "beforeCursorEnter");if(m.explicitlyCleared){if(!line.markedSpans)break;else {--i;continue;}}}if(!m.atomic)continue;var newPos=m.find(dir < 0?-1:1);if(cmp(newPos, curPos) == 0){newPos.ch += dir;if(newPos.ch < 0){if(newPos.line > doc.first)newPos = clipPos(doc, Pos(newPos.line - 1));else newPos = null;}else if(newPos.ch > line.text.length){if(newPos.line < doc.first + doc.size - 1)newPos = Pos(newPos.line + 1, 0);else newPos = null;}if(!newPos){if(flipped){if(!mayClear){_x = doc;_x2 = pos;_x3 = bias;_x4 = true;_again = true;continue _function;}doc.cantEdit = true;return Pos(doc.first, 0);}flipped = true;newPos = pos;dir = -dir;}}curPos = newPos;continue search;}}}return curPos;}}}function updateSelection(cm){cm.display.input.showSelection(cm.display.input.prepareSelection());}function prepareSelection(cm, primary){var doc=cm.doc, result={};var curFragment=result.cursors = document.createDocumentFragment();var selFragment=result.selection = document.createDocumentFragment();for(var i=0; i < doc.sel.ranges.length; i++) {if(primary === false && i == doc.sel.primIndex)continue;var range=doc.sel.ranges[i];var collapsed=range.empty();if(collapsed || cm.options.showCursorWhenSelecting)drawSelectionCursor(cm, range, curFragment);if(!collapsed)drawSelectionRange(cm, range, selFragment);}return result;}function drawSelectionCursor(cm, range, output){var pos=cursorCoords(cm, range.head, "div", null, null, !cm.options.singleCursorHeightPerLine);var cursor=output.appendChild(elt("div", " ", "CodeMirror-cursor"));cursor.style.left = pos.left + "px";cursor.style.top = pos.top + "px";cursor.style.height = Math.max(0, pos.bottom - pos.top) * cm.options.cursorHeight + "px";if(pos.other){var otherCursor=output.appendChild(elt("div", " ", "CodeMirror-cursor CodeMirror-secondarycursor"));otherCursor.style.display = "";otherCursor.style.left = pos.other.left + "px";otherCursor.style.top = pos.other.top + "px";otherCursor.style.height = (pos.other.bottom - pos.other.top) * 0.85 + "px";}}function drawSelectionRange(cm, range, output){var display=cm.display, doc=cm.doc;var fragment=document.createDocumentFragment();var padding=paddingH(cm.display), leftSide=padding.left;var rightSide=Math.max(display.sizerWidth, displayWidth(cm) - display.sizer.offsetLeft) - padding.right;function add(left, top, width, bottom){if(top < 0)top = 0;top = Math.round(top);bottom = Math.round(bottom);fragment.appendChild(elt("div", null, "CodeMirror-selected", "position: absolute; left: " + left + "px; top: " + top + "px; width: " + (width == null?rightSide - left:width) + "px; height: " + (bottom - top) + "px"));}function drawForLine(line, fromArg, toArg){var lineObj=getLine(doc, line);var lineLen=lineObj.text.length;var start, end;function coords(ch, bias){return charCoords(cm, Pos(line, ch), "div", lineObj, bias);}iterateBidiSections(getOrder(lineObj), fromArg || 0, toArg == null?lineLen:toArg, function(from, to, dir){var leftPos=coords(from, "left"), rightPos, left, right;if(from == to){rightPos = leftPos;left = right = leftPos.left;}else {rightPos = coords(to - 1, "right");if(dir == "rtl"){var tmp=leftPos;leftPos = rightPos;rightPos = tmp;}left = leftPos.left;right = rightPos.right;}if(fromArg == null && from == 0)left = leftSide;if(rightPos.top - leftPos.top > 3){add(left, leftPos.top, null, leftPos.bottom);left = leftSide;if(leftPos.bottom < rightPos.top)add(left, leftPos.bottom, null, rightPos.top);}if(toArg == null && to == lineLen)right = rightSide;if(!start || leftPos.top < start.top || leftPos.top == start.top && leftPos.left < start.left)start = leftPos;if(!end || rightPos.bottom > end.bottom || rightPos.bottom == end.bottom && rightPos.right > end.right)end = rightPos;if(left < leftSide + 1)left = leftSide;add(left, rightPos.top, right - left, rightPos.bottom);});return {start:start, end:end};}var sFrom=range.from(), sTo=range.to();if(sFrom.line == sTo.line){drawForLine(sFrom.line, sFrom.ch, sTo.ch);}else {var fromLine=getLine(doc, sFrom.line), toLine=getLine(doc, sTo.line);var singleVLine=visualLine(fromLine) == visualLine(toLine);var leftEnd=drawForLine(sFrom.line, sFrom.ch, singleVLine?fromLine.text.length + 1:null).end;var rightStart=drawForLine(sTo.line, singleVLine?0:null, sTo.ch).start;if(singleVLine){if(leftEnd.top < rightStart.top - 2){add(leftEnd.right, leftEnd.top, null, leftEnd.bottom);add(leftSide, rightStart.top, rightStart.left, rightStart.bottom);}else {add(leftEnd.right, leftEnd.top, rightStart.left - leftEnd.right, leftEnd.bottom);}}if(leftEnd.bottom < rightStart.top)add(leftSide, leftEnd.bottom, null, rightStart.top);}output.appendChild(fragment);}function restartBlink(cm){if(!cm.state.focused){return;}var display=cm.display;clearInterval(display.blinker);var on=true;display.cursorDiv.style.visibility = "";if(cm.options.cursorBlinkRate > 0)display.blinker = setInterval(function(){display.cursorDiv.style.visibility = (on = !on)?"":"hidden";}, cm.options.cursorBlinkRate);else if(cm.options.cursorBlinkRate < 0)display.cursorDiv.style.visibility = "hidden";}function startWorker(cm, time){if(cm.doc.mode.startState && cm.doc.frontier < cm.display.viewTo)cm.state.highlight.set(time, bind(highlightWorker, cm));}function highlightWorker(cm){var doc=cm.doc;if(doc.frontier < doc.first)doc.frontier = doc.first;if(doc.frontier >= cm.display.viewTo){return;}var end=+new Date() + cm.options.workTime;var state=copyState(doc.mode, getStateBefore(cm, doc.frontier));var changedLines=[];doc.iter(doc.frontier, Math.min(doc.first + doc.size, cm.display.viewTo + 500), function(line){if(doc.frontier >= cm.display.viewFrom){var oldStyles=line.styles;var highlighted=highlightLine(cm, line, state, true);line.styles = highlighted.styles;var oldCls=line.styleClasses, newCls=highlighted.classes;if(newCls)line.styleClasses = newCls;else if(oldCls)line.styleClasses = null;var ischange=!oldStyles || oldStyles.length != line.styles.length || oldCls != newCls && (!oldCls || !newCls || oldCls.bgClass != newCls.bgClass || oldCls.textClass != newCls.textClass);for(var i=0; !ischange && i < oldStyles.length; ++i) ischange = oldStyles[i] != line.styles[i];if(ischange)changedLines.push(doc.frontier);line.stateAfter = copyState(doc.mode, state);}else {processLine(cm, line.text, state);line.stateAfter = doc.frontier % 5 == 0?copyState(doc.mode, state):null;}++doc.frontier;if(+new Date() > end){startWorker(cm, cm.options.workDelay);return true;}});if(changedLines.length)runInOp(cm, function(){for(var i=0; i < changedLines.length; i++) regLineChange(cm, changedLines[i], "text");});}function findStartLine(cm, n, precise){var minindent, minline, doc=cm.doc;var lim=precise?-1:n - (cm.doc.mode.innerMode?1000:100);for(var search=n; search > lim; --search) {if(search <= doc.first){return doc.first;}var line=getLine(doc, search - 1);if(line.stateAfter && (!precise || search <= doc.frontier)){return search;}var indented=countColumn(line.text, null, cm.options.tabSize);if(minline == null || minindent > indented){minline = search - 1;minindent = indented;}}return minline;}function getStateBefore(cm, n, precise){var doc=cm.doc, display=cm.display;if(!doc.mode.startState){return true;}var pos=findStartLine(cm, n, precise), state=pos > doc.first && getLine(doc, pos - 1).stateAfter;if(!state)state = startState(doc.mode);else state = copyState(doc.mode, state);doc.iter(pos, n, function(line){processLine(cm, line.text, state);var save=pos == n - 1 || pos % 5 == 0 || pos >= display.viewFrom && pos < display.viewTo;line.stateAfter = save?copyState(doc.mode, state):null;++pos;});if(precise)doc.frontier = pos;return state;}function paddingTop(display){return display.lineSpace.offsetTop;}function paddingVert(display){return display.mover.offsetHeight - display.lineSpace.offsetHeight;}function paddingH(display){if(display.cachedPaddingH){return display.cachedPaddingH;}var e=removeChildrenAndAdd(display.measure, elt("pre", "x"));var style=window.getComputedStyle?window.getComputedStyle(e):e.currentStyle;var data={left:parseInt(style.paddingLeft), right:parseInt(style.paddingRight)};if(!isNaN(data.left) && !isNaN(data.right))display.cachedPaddingH = data;return data;}function scrollGap(cm){return scrollerGap - cm.display.nativeBarWidth;}function displayWidth(cm){return cm.display.scroller.clientWidth - scrollGap(cm) - cm.display.barWidth;}function displayHeight(cm){return cm.display.scroller.clientHeight - scrollGap(cm) - cm.display.barHeight;}function ensureLineHeights(cm, lineView, rect){var wrapping=cm.options.lineWrapping;var curWidth=wrapping && displayWidth(cm);if(!lineView.measure.heights || wrapping && lineView.measure.width != curWidth){var heights=lineView.measure.heights = [];if(wrapping){lineView.measure.width = curWidth;var rects=lineView.text.firstChild.getClientRects();for(var i=0; i < rects.length - 1; i++) {var cur=rects[i], next=rects[i + 1];if(Math.abs(cur.bottom - next.bottom) > 2)heights.push((cur.bottom + next.top) / 2 - rect.top);}}heights.push(rect.bottom - rect.top);}}function mapFromLineView(lineView, line, lineN){if(lineView.line == line){return {map:lineView.measure.map, cache:lineView.measure.cache};}for(var i=0; i < lineView.rest.length; i++) if(lineView.rest[i] == line){return {map:lineView.measure.maps[i], cache:lineView.measure.caches[i]};}for(var i=0; i < lineView.rest.length; i++) if(lineNo(lineView.rest[i]) > lineN){return {map:lineView.measure.maps[i], cache:lineView.measure.caches[i], before:true};}}function updateExternalMeasurement(cm, line){line = visualLine(line);var lineN=lineNo(line);var view=cm.display.externalMeasured = new LineView(cm.doc, line, lineN);view.lineN = lineN;var built=view.built = buildLineContent(cm, view);view.text = built.pre;removeChildrenAndAdd(cm.display.lineMeasure, built.pre);return view;}function measureChar(cm, line, ch, bias){return measureCharPrepared(cm, prepareMeasureForLine(cm, line), ch, bias);}function findViewForLine(cm, lineN){if(lineN >= cm.display.viewFrom && lineN < cm.display.viewTo){return cm.display.view[findViewIndex(cm, lineN)];}var ext=cm.display.externalMeasured;if(ext && lineN >= ext.lineN && lineN < ext.lineN + ext.size){return ext;}}function prepareMeasureForLine(cm, line){var lineN=lineNo(line);var view=findViewForLine(cm, lineN);if(view && !view.text)view = null;else if(view && view.changes)updateLineForChanges(cm, view, lineN, getDimensions(cm));if(!view)view = updateExternalMeasurement(cm, line);var info=mapFromLineView(view, line, lineN);return {line:line, view:view, rect:null, map:info.map, cache:info.cache, before:info.before, hasHeights:false};}function measureCharPrepared(cm, prepared, ch, bias, varHeight){if(prepared.before)ch = -1;var key=ch + (bias || ""), found;if(prepared.cache.hasOwnProperty(key)){found = prepared.cache[key];}else {if(!prepared.rect)prepared.rect = prepared.view.text.getBoundingClientRect();if(!prepared.hasHeights){ensureLineHeights(cm, prepared.view, prepared.rect);prepared.hasHeights = true;}found = measureCharInner(cm, prepared, ch, bias);if(!found.bogus)prepared.cache[key] = found;}return {left:found.left, right:found.right, top:varHeight?found.rtop:found.top, bottom:varHeight?found.rbottom:found.bottom};}var nullRect={left:0, right:0, top:0, bottom:0};function nodeAndOffsetInLineMap(map, ch, bias){var node, start, end, collapse;for(var i=0; i < map.length; i += 3) {var mStart=map[i], mEnd=map[i + 1];if(ch < mStart){start = 0;end = 1;collapse = "left";}else if(ch < mEnd){start = ch - mStart;end = start + 1;}else if(i == map.length - 3 || ch == mEnd && map[i + 3] > ch){end = mEnd - mStart;start = end - 1;if(ch >= mEnd)collapse = "right";}if(start != null){node = map[i + 2];if(mStart == mEnd && bias == (node.insertLeft?"left":"right"))collapse = bias;if(bias == "left" && start == 0)while(i && map[i - 2] == map[i - 3] && map[i - 1].insertLeft) {node = map[(i -= 3) + 2];collapse = "left";}if(bias == "right" && start == mEnd - mStart)while(i < map.length - 3 && map[i + 3] == map[i + 4] && !map[i + 5].insertLeft) {node = map[(i += 3) + 2];collapse = "right";}break;}}return {node:node, start:start, end:end, collapse:collapse, coverStart:mStart, coverEnd:mEnd};}function measureCharInner(cm, prepared, ch, bias){var place=nodeAndOffsetInLineMap(prepared.map, ch, bias);var node=place.node, start=place.start, end=place.end, collapse=place.collapse;var rect;if(node.nodeType == 3){for(var i=0; i < 4; i++) {while(start && isExtendingChar(prepared.line.text.charAt(place.coverStart + start))) --start;while(place.coverStart + end < place.coverEnd && isExtendingChar(prepared.line.text.charAt(place.coverStart + end))) ++end;if(ie && ie_version < 9 && start == 0 && end == place.coverEnd - place.coverStart){rect = node.parentNode.getBoundingClientRect();}else if(ie && cm.options.lineWrapping){var rects=range(node, start, end).getClientRects();if(rects.length)rect = rects[bias == "right"?rects.length - 1:0];else rect = nullRect;}else {rect = range(node, start, end).getBoundingClientRect() || nullRect;}if(rect.left || rect.right || start == 0)break;end = start;start = start - 1;collapse = "right";}if(ie && ie_version < 11)rect = maybeUpdateRectForZooming(cm.display.measure, rect);}else {if(start > 0)collapse = bias = "right";var rects;if(cm.options.lineWrapping && (rects = node.getClientRects()).length > 1)rect = rects[bias == "right"?rects.length - 1:0];else rect = node.getBoundingClientRect();}if(ie && ie_version < 9 && !start && (!rect || !rect.left && !rect.right)){var rSpan=node.parentNode.getClientRects()[0];if(rSpan)rect = {left:rSpan.left, right:rSpan.left + charWidth(cm.display), top:rSpan.top, bottom:rSpan.bottom};else rect = nullRect;}var rtop=rect.top - prepared.rect.top, rbot=rect.bottom - prepared.rect.top;var mid=(rtop + rbot) / 2;var heights=prepared.view.measure.heights;for(var i=0; i < heights.length - 1; i++) if(mid < heights[i])break;var top=i?heights[i - 1]:0, bot=heights[i];var result={left:(collapse == "right"?rect.right:rect.left) - prepared.rect.left, right:(collapse == "left"?rect.left:rect.right) - prepared.rect.left, top:top, bottom:bot};if(!rect.left && !rect.right)result.bogus = true;if(!cm.options.singleCursorHeightPerLine){result.rtop = rtop;result.rbottom = rbot;}return result;}function maybeUpdateRectForZooming(measure, rect){if(!window.screen || screen.logicalXDPI == null || screen.logicalXDPI == screen.deviceXDPI || !hasBadZoomedRects(measure)){return rect;}var scaleX=screen.logicalXDPI / screen.deviceXDPI;var scaleY=screen.logicalYDPI / screen.deviceYDPI;return {left:rect.left * scaleX, right:rect.right * scaleX, top:rect.top * scaleY, bottom:rect.bottom * scaleY};}function clearLineMeasurementCacheFor(lineView){if(lineView.measure){lineView.measure.cache = {};lineView.measure.heights = null;if(lineView.rest)for(var i=0; i < lineView.rest.length; i++) lineView.measure.caches[i] = {};}}function clearLineMeasurementCache(cm){cm.display.externalMeasure = null;removeChildren(cm.display.lineMeasure);for(var i=0; i < cm.display.view.length; i++) clearLineMeasurementCacheFor(cm.display.view[i]);}function clearCaches(cm){clearLineMeasurementCache(cm);cm.display.cachedCharWidth = cm.display.cachedTextHeight = cm.display.cachedPaddingH = null;if(!cm.options.lineWrapping)cm.display.maxLineChanged = true;cm.display.lineNumChars = null;}function pageScrollX(){return window.pageXOffset || (document.documentElement || document.body).scrollLeft;}function pageScrollY(){return window.pageYOffset || (document.documentElement || document.body).scrollTop;}function intoCoordSystem(cm, lineObj, rect, context){if(lineObj.widgets)for(var i=0; i < lineObj.widgets.length; ++i) if(lineObj.widgets[i].above){var size=widgetHeight(lineObj.widgets[i]);rect.top += size;rect.bottom += size;}if(context == "line"){return rect;}if(!context)context = "local";var yOff=heightAtLine(lineObj);if(context == "local")yOff += paddingTop(cm.display);else yOff -= cm.display.viewOffset;if(context == "page" || context == "window"){var lOff=cm.display.lineSpace.getBoundingClientRect();yOff += lOff.top + (context == "window"?0:pageScrollY());var xOff=lOff.left + (context == "window"?0:pageScrollX());rect.left += xOff;rect.right += xOff;}rect.top += yOff;rect.bottom += yOff;return rect;}function fromCoordSystem(cm, coords, context){if(context == "div"){return coords;}var left=coords.left, top=coords.top;if(context == "page"){left -= pageScrollX();top -= pageScrollY();}else if(context == "local" || !context){var localBox=cm.display.sizer.getBoundingClientRect();left += localBox.left;top += localBox.top;}var lineSpaceBox=cm.display.lineSpace.getBoundingClientRect();return {left:left - lineSpaceBox.left, top:top - lineSpaceBox.top};}function charCoords(cm, pos, context, lineObj, bias){if(!lineObj)lineObj = getLine(cm.doc, pos.line);return intoCoordSystem(cm, lineObj, measureChar(cm, lineObj, pos.ch, bias), context);}function cursorCoords(cm, pos, context, lineObj, preparedMeasure, varHeight){lineObj = lineObj || getLine(cm.doc, pos.line);if(!preparedMeasure)preparedMeasure = prepareMeasureForLine(cm, lineObj);function get(ch, right){var m=measureCharPrepared(cm, preparedMeasure, ch, right?"right":"left", varHeight);if(right)m.left = m.right;else m.right = m.left;return intoCoordSystem(cm, lineObj, m, context);}function getBidi(ch, partPos){var part=order[partPos], right=part.level % 2;if(ch == bidiLeft(part) && partPos && part.level < order[partPos - 1].level){part = order[--partPos];ch = bidiRight(part) - (part.level % 2?0:1);right = true;}else if(ch == bidiRight(part) && partPos < order.length - 1 && part.level < order[partPos + 1].level){part = order[++partPos];ch = bidiLeft(part) - part.level % 2;right = false;}if(right && ch == part.to && ch > part.from){return get(ch - 1);}return get(ch, right);}var order=getOrder(lineObj), ch=pos.ch;if(!order){return get(ch);}var partPos=getBidiPartAt(order, ch);var val=getBidi(ch, partPos);if(bidiOther != null)val.other = getBidi(ch, bidiOther);return val;}function estimateCoords(cm, pos){var left=0, pos=clipPos(cm.doc, pos);if(!cm.options.lineWrapping)left = charWidth(cm.display) * pos.ch;var lineObj=getLine(cm.doc, pos.line);var top=heightAtLine(lineObj) + paddingTop(cm.display);return {left:left, right:left, top:top, bottom:top + lineObj.height};}function PosWithInfo(line, ch, outside, xRel){var pos=Pos(line, ch);pos.xRel = xRel;if(outside)pos.outside = true;return pos;}function coordsChar(cm, x, y){var doc=cm.doc;y += cm.display.viewOffset;if(y < 0){return PosWithInfo(doc.first, 0, true, -1);}var lineN=lineAtHeight(doc, y), last=doc.first + doc.size - 1;if(lineN > last){return PosWithInfo(doc.first + doc.size - 1, getLine(doc, last).text.length, true, 1);}if(x < 0)x = 0;var lineObj=getLine(doc, lineN);for(;;) {var found=coordsCharInner(cm, lineObj, lineN, x, y);var merged=collapsedSpanAtEnd(lineObj);var mergedPos=merged && merged.find(0, true);if(merged && (found.ch > mergedPos.from.ch || found.ch == mergedPos.from.ch && found.xRel > 0))lineN = lineNo(lineObj = mergedPos.to.line);else {return found;}}}function coordsCharInner(cm, lineObj, lineNo, x, y){var innerOff=y - heightAtLine(lineObj);var wrongLine=false, adjust=2 * cm.display.wrapper.clientWidth;var preparedMeasure=prepareMeasureForLine(cm, lineObj);function getX(ch){var sp=cursorCoords(cm, Pos(lineNo, ch), "line", lineObj, preparedMeasure);wrongLine = true;if(innerOff > sp.bottom){return sp.left - adjust;}else if(innerOff < sp.top){return sp.left + adjust;}else wrongLine = false;return sp.left;}var bidi=getOrder(lineObj), dist=lineObj.text.length;var from=lineLeft(lineObj), to=lineRight(lineObj);var fromX=getX(from), fromOutside=wrongLine, toX=getX(to), toOutside=wrongLine;if(x > toX){return PosWithInfo(lineNo, to, toOutside, 1);}for(;;) {if(bidi?to == from || to == moveVisually(lineObj, from, 1):to - from <= 1){var ch=x < fromX || x - fromX <= toX - x?from:to;var xDiff=x - (ch == from?fromX:toX);while(isExtendingChar(lineObj.text.charAt(ch))) ++ch;var pos=PosWithInfo(lineNo, ch, ch == from?fromOutside:toOutside, xDiff < -1?-1:xDiff > 1?1:0);return pos;}var step=Math.ceil(dist / 2), middle=from + step;if(bidi){middle = from;for(var i=0; i < step; ++i) middle = moveVisually(lineObj, middle, 1);}var middleX=getX(middle);if(middleX > x){to = middle;toX = middleX;if(toOutside = wrongLine)toX += 1000;dist = step;}else {from = middle;fromX = middleX;fromOutside = wrongLine;dist -= step;}}}var measureText;function textHeight(display){if(display.cachedTextHeight != null){return display.cachedTextHeight;}if(measureText == null){measureText = elt("pre");for(var i=0; i < 49; ++i) {measureText.appendChild(document.createTextNode("x"));measureText.appendChild(elt("br"));}measureText.appendChild(document.createTextNode("x"));}removeChildrenAndAdd(display.measure, measureText);var height=measureText.offsetHeight / 50;if(height > 3)display.cachedTextHeight = height;removeChildren(display.measure);return height || 1;}function charWidth(display){if(display.cachedCharWidth != null){return display.cachedCharWidth;}var anchor=elt("span", "xxxxxxxxxx");var pre=elt("pre", [anchor]);removeChildrenAndAdd(display.measure, pre);var rect=anchor.getBoundingClientRect(), width=(rect.right - rect.left) / 10;if(width > 2)display.cachedCharWidth = width;return width || 10;}var operationGroup=null;var nextOpId=0;function startOperation(cm){cm.curOp = {cm:cm, viewChanged:false, startHeight:cm.doc.height, forceUpdate:false, updateInput:null, typing:false, changeObjs:null, cursorActivityHandlers:null, cursorActivityCalled:0, selectionChanged:false, updateMaxLine:false, scrollLeft:null, scrollTop:null, scrollToPos:null, focus:false, id:++nextOpId};if(operationGroup){operationGroup.ops.push(cm.curOp);}else {cm.curOp.ownsGroup = operationGroup = {ops:[cm.curOp], delayedCallbacks:[]};}}function fireCallbacksForOps(group){var callbacks=group.delayedCallbacks, i=0;do{for(; i < callbacks.length; i++) callbacks[i]();for(var j=0; j < group.ops.length; j++) {var op=group.ops[j];if(op.cursorActivityHandlers)while(op.cursorActivityCalled < op.cursorActivityHandlers.length) op.cursorActivityHandlers[op.cursorActivityCalled++](op.cm);}}while(i < callbacks.length);}function endOperation(cm){var op=cm.curOp, group=op.ownsGroup;if(!group){return;}try{fireCallbacksForOps(group);}finally {operationGroup = null;for(var i=0; i < group.ops.length; i++) group.ops[i].cm.curOp = null;endOperations(group);}}function endOperations(group){var ops=group.ops;for(var i=0; i < ops.length; i++) endOperation_R1(ops[i]);for(var i=0; i < ops.length; i++) endOperation_W1(ops[i]);for(var i=0; i < ops.length; i++) endOperation_R2(ops[i]);for(var i=0; i < ops.length; i++) endOperation_W2(ops[i]);for(var i=0; i < ops.length; i++) endOperation_finish(ops[i]);}function endOperation_R1(op){var cm=op.cm, display=cm.display;maybeClipScrollbars(cm);if(op.updateMaxLine)findMaxLine(cm);op.mustUpdate = op.viewChanged || op.forceUpdate || op.scrollTop != null || op.scrollToPos && (op.scrollToPos.from.line < display.viewFrom || op.scrollToPos.to.line >= display.viewTo) || display.maxLineChanged && cm.options.lineWrapping;op.update = op.mustUpdate && new DisplayUpdate(cm, op.mustUpdate && {top:op.scrollTop, ensure:op.scrollToPos}, op.forceUpdate);}function endOperation_W1(op){op.updatedDisplay = op.mustUpdate && updateDisplayIfNeeded(op.cm, op.update);}function endOperation_R2(op){var cm=op.cm, display=cm.display;if(op.updatedDisplay)updateHeightsInViewport(cm);op.barMeasure = measureForScrollbars(cm);if(display.maxLineChanged && !cm.options.lineWrapping){op.adjustWidthTo = measureChar(cm, display.maxLine, display.maxLine.text.length).left + 3;cm.display.sizerWidth = op.adjustWidthTo;op.barMeasure.scrollWidth = Math.max(display.scroller.clientWidth, display.sizer.offsetLeft + op.adjustWidthTo + scrollGap(cm) + cm.display.barWidth);op.maxScrollLeft = Math.max(0, display.sizer.offsetLeft + op.adjustWidthTo - displayWidth(cm));}if(op.updatedDisplay || op.selectionChanged)op.preparedSelection = display.input.prepareSelection();}function endOperation_W2(op){var cm=op.cm;if(op.adjustWidthTo != null){cm.display.sizer.style.minWidth = op.adjustWidthTo + "px";if(op.maxScrollLeft < cm.doc.scrollLeft)setScrollLeft(cm, Math.min(cm.display.scroller.scrollLeft, op.maxScrollLeft), true);cm.display.maxLineChanged = false;}if(op.preparedSelection)cm.display.input.showSelection(op.preparedSelection);if(op.updatedDisplay)setDocumentHeight(cm, op.barMeasure);if(op.updatedDisplay || op.startHeight != cm.doc.height)updateScrollbars(cm, op.barMeasure);if(op.selectionChanged)restartBlink(cm);if(cm.state.focused && op.updateInput)cm.display.input.reset(op.typing);if(op.focus && op.focus == activeElt())ensureFocus(op.cm);}function endOperation_finish(op){var cm=op.cm, display=cm.display, doc=cm.doc;if(op.updatedDisplay)postUpdateDisplay(cm, op.update);if(display.wheelStartX != null && (op.scrollTop != null || op.scrollLeft != null || op.scrollToPos))display.wheelStartX = display.wheelStartY = null;if(op.scrollTop != null && (display.scroller.scrollTop != op.scrollTop || op.forceScroll)){doc.scrollTop = Math.max(0, Math.min(display.scroller.scrollHeight - display.scroller.clientHeight, op.scrollTop));display.scrollbars.setScrollTop(doc.scrollTop);display.scroller.scrollTop = doc.scrollTop;}if(op.scrollLeft != null && (display.scroller.scrollLeft != op.scrollLeft || op.forceScroll)){doc.scrollLeft = Math.max(0, Math.min(display.scroller.scrollWidth - displayWidth(cm), op.scrollLeft));display.scrollbars.setScrollLeft(doc.scrollLeft);display.scroller.scrollLeft = doc.scrollLeft;alignHorizontally(cm);}if(op.scrollToPos){var coords=scrollPosIntoView(cm, clipPos(doc, op.scrollToPos.from), clipPos(doc, op.scrollToPos.to), op.scrollToPos.margin);if(op.scrollToPos.isCursor && cm.state.focused)maybeScrollWindow(cm, coords);}var hidden=op.maybeHiddenMarkers, unhidden=op.maybeUnhiddenMarkers;if(hidden)for(var i=0; i < hidden.length; ++i) if(!hidden[i].lines.length)signal(hidden[i], "hide");if(unhidden)for(var i=0; i < unhidden.length; ++i) if(unhidden[i].lines.length)signal(unhidden[i], "unhide");if(display.wrapper.offsetHeight)doc.scrollTop = cm.display.scroller.scrollTop;if(op.changeObjs)signal(cm, "changes", cm, op.changeObjs);if(op.update)op.update.finish();}function runInOp(cm, f){if(cm.curOp){return f();}startOperation(cm);try{return f();}finally {endOperation(cm);}}function operation(cm, f){return function(){if(cm.curOp)return f.apply(cm, arguments);startOperation(cm);try{return f.apply(cm, arguments);}finally {endOperation(cm);}};}function methodOp(f){return function(){if(this.curOp)return f.apply(this, arguments);startOperation(this);try{return f.apply(this, arguments);}finally {endOperation(this);}};}function docMethodOp(f){return function(){var cm=this.cm;if(!cm || cm.curOp)return f.apply(this, arguments);startOperation(cm);try{return f.apply(this, arguments);}finally {endOperation(cm);}};}function LineView(doc, line, lineN){this.line = line;this.rest = visualLineContinued(line);this.size = this.rest?lineNo(lst(this.rest)) - lineN + 1:1;this.node = this.text = null;this.hidden = lineIsHidden(doc, line);}function buildViewArray(cm, from, to){var array=[], nextPos;for(var pos=from; pos < to; pos = nextPos) {var view=new LineView(cm.doc, getLine(cm.doc, pos), pos);nextPos = pos + view.size;array.push(view);}return array;}function regChange(cm, from, to, lendiff){if(from == null)from = cm.doc.first;if(to == null)to = cm.doc.first + cm.doc.size;if(!lendiff)lendiff = 0;var display=cm.display;if(lendiff && to < display.viewTo && (display.updateLineNumbers == null || display.updateLineNumbers > from))display.updateLineNumbers = from;cm.curOp.viewChanged = true;if(from >= display.viewTo){if(sawCollapsedSpans && visualLineNo(cm.doc, from) < display.viewTo)resetView(cm);}else if(to <= display.viewFrom){if(sawCollapsedSpans && visualLineEndNo(cm.doc, to + lendiff) > display.viewFrom){resetView(cm);}else {display.viewFrom += lendiff;display.viewTo += lendiff;}}else if(from <= display.viewFrom && to >= display.viewTo){resetView(cm);}else if(from <= display.viewFrom){var cut=viewCuttingPoint(cm, to, to + lendiff, 1);if(cut){display.view = display.view.slice(cut.index);display.viewFrom = cut.lineN;display.viewTo += lendiff;}else {resetView(cm);}}else if(to >= display.viewTo){var cut=viewCuttingPoint(cm, from, from, -1);if(cut){display.view = display.view.slice(0, cut.index);display.viewTo = cut.lineN;}else {resetView(cm);}}else {var cutTop=viewCuttingPoint(cm, from, from, -1);var cutBot=viewCuttingPoint(cm, to, to + lendiff, 1);if(cutTop && cutBot){display.view = display.view.slice(0, cutTop.index).concat(buildViewArray(cm, cutTop.lineN, cutBot.lineN)).concat(display.view.slice(cutBot.index));display.viewTo += lendiff;}else {resetView(cm);}}var ext=display.externalMeasured;if(ext){if(to < ext.lineN)ext.lineN += lendiff;else if(from < ext.lineN + ext.size)display.externalMeasured = null;}}function regLineChange(cm, line, type){cm.curOp.viewChanged = true;var display=cm.display, ext=cm.display.externalMeasured;if(ext && line >= ext.lineN && line < ext.lineN + ext.size)display.externalMeasured = null;if(line < display.viewFrom || line >= display.viewTo){return;}var lineView=display.view[findViewIndex(cm, line)];if(lineView.node == null){return;}var arr=lineView.changes || (lineView.changes = []);if(indexOf(arr, type) == -1)arr.push(type);}function resetView(cm){cm.display.viewFrom = cm.display.viewTo = cm.doc.first;cm.display.view = [];cm.display.viewOffset = 0;}function findViewIndex(cm, n){if(n >= cm.display.viewTo){return null;}n -= cm.display.viewFrom;if(n < 0){return null;}var view=cm.display.view;for(var i=0; i < view.length; i++) {n -= view[i].size;if(n < 0){return i;}}}function viewCuttingPoint(cm, oldN, newN, dir){var index=findViewIndex(cm, oldN), diff, view=cm.display.view;if(!sawCollapsedSpans || newN == cm.doc.first + cm.doc.size){return {index:index, lineN:newN};}for(var i=0, n=cm.display.viewFrom; i < index; i++) n += view[i].size;if(n != oldN){if(dir > 0){if(index == view.length - 1){return null;}diff = n + view[index].size - oldN;index++;}else {diff = n - oldN;}oldN += diff;newN += diff;}while(visualLineNo(cm.doc, newN) != newN) {if(index == (dir < 0?0:view.length - 1)){return null;}newN += dir * view[index - (dir < 0?1:0)].size;index += dir;}return {index:index, lineN:newN};}function adjustView(cm, from, to){var display=cm.display, view=display.view;if(view.length == 0 || from >= display.viewTo || to <= display.viewFrom){display.view = buildViewArray(cm, from, to);display.viewFrom = from;}else {if(display.viewFrom > from)display.view = buildViewArray(cm, from, display.viewFrom).concat(display.view);else if(display.viewFrom < from)display.view = display.view.slice(findViewIndex(cm, from));display.viewFrom = from;if(display.viewTo < to)display.view = display.view.concat(buildViewArray(cm, display.viewTo, to));else if(display.viewTo > to)display.view = display.view.slice(0, findViewIndex(cm, to));}display.viewTo = to;}function countDirtyView(cm){var view=cm.display.view, dirty=0;for(var i=0; i < view.length; i++) {var lineView=view[i];if(!lineView.hidden && (!lineView.node || lineView.changes))++dirty;}return dirty;}function registerEventHandlers(cm){var d=cm.display;on(d.scroller, "mousedown", operation(cm, onMouseDown));if(ie && ie_version < 11)on(d.scroller, "dblclick", operation(cm, function(e){if(signalDOMEvent(cm, e))return;var pos=posFromMouse(cm, e);if(!pos || clickInGutter(cm, e) || eventInWidget(cm.display, e))return;e_preventDefault(e);var word=cm.findWordAt(pos);extendSelection(cm.doc, word.anchor, word.head);}));else on(d.scroller, "dblclick", function(e){signalDOMEvent(cm, e) || e_preventDefault(e);});if(!captureRightClick)on(d.scroller, "contextmenu", function(e){onContextMenu(cm, e);});var touchFinished, prevTouch={end:0};function finishTouch(){if(d.activeTouch){touchFinished = setTimeout(function(){d.activeTouch = null;}, 1000);prevTouch = d.activeTouch;prevTouch.end = +new Date();}};function isMouseLikeTouchEvent(e){if(e.touches.length != 1){return false;}var touch=e.touches[0];return touch.radiusX <= 1 && touch.radiusY <= 1;}function farAway(touch, other){if(other.left == null){return true;}var dx=other.left - touch.left, dy=other.top - touch.top;return dx * dx + dy * dy > 20 * 20;}on(d.scroller, "touchstart", function(e){if(!isMouseLikeTouchEvent(e)){clearTimeout(touchFinished);var now=+new Date();d.activeTouch = {start:now, moved:false, prev:now - prevTouch.end <= 300?prevTouch:null};if(e.touches.length == 1){d.activeTouch.left = e.touches[0].pageX;d.activeTouch.top = e.touches[0].pageY;}}});on(d.scroller, "touchmove", function(){if(d.activeTouch)d.activeTouch.moved = true;});on(d.scroller, "touchend", function(e){var touch=d.activeTouch;if(touch && !eventInWidget(d, e) && touch.left != null && !touch.moved && new Date() - touch.start < 300){var pos=cm.coordsChar(d.activeTouch, "page"), range;if(!touch.prev || farAway(touch, touch.prev))range = new Range(pos, pos);else if(!touch.prev.prev || farAway(touch, touch.prev.prev))range = cm.findWordAt(pos);else range = new Range(Pos(pos.line, 0), clipPos(cm.doc, Pos(pos.line + 1, 0)));cm.setSelection(range.anchor, range.head);cm.focus();e_preventDefault(e);}finishTouch();});on(d.scroller, "touchcancel", finishTouch);on(d.scroller, "scroll", function(){if(d.scroller.clientHeight){setScrollTop(cm, d.scroller.scrollTop);setScrollLeft(cm, d.scroller.scrollLeft, true);signal(cm, "scroll", cm);}});on(d.scroller, "mousewheel", function(e){onScrollWheel(cm, e);});on(d.scroller, "DOMMouseScroll", function(e){onScrollWheel(cm, e);});on(d.wrapper, "scroll", function(){d.wrapper.scrollTop = d.wrapper.scrollLeft = 0;});d.dragFunctions = {simple:function simple(e){if(!signalDOMEvent(cm, e))e_stop(e);}, start:function start(e){onDragStart(cm, e);}, drop:operation(cm, onDrop)};var inp=d.input.getField();on(inp, "keyup", function(e){onKeyUp.call(cm, e);});on(inp, "keydown", operation(cm, onKeyDown));on(inp, "keypress", operation(cm, onKeyPress));on(inp, "focus", bind(onFocus, cm));on(inp, "blur", bind(onBlur, cm));}function dragDropChanged(cm, value, old){var wasOn=old && old != CodeMirror.Init;if(!value != !wasOn){var funcs=cm.display.dragFunctions;var toggle=value?on:off;toggle(cm.display.scroller, "dragstart", funcs.start);toggle(cm.display.scroller, "dragenter", funcs.simple);toggle(cm.display.scroller, "dragover", funcs.simple);toggle(cm.display.scroller, "drop", funcs.drop);}}function onResize(cm){var d=cm.display;if(d.lastWrapHeight == d.wrapper.clientHeight && d.lastWrapWidth == d.wrapper.clientWidth){return;}d.cachedCharWidth = d.cachedTextHeight = d.cachedPaddingH = null;d.scrollbarsClipped = false;cm.setSize();}function eventInWidget(display, e){for(var n=e_target(e); n != display.wrapper; n = n.parentNode) {if(!n || n.nodeType == 1 && n.getAttribute("cm-ignore-events") == "true" || n.parentNode == display.sizer && n != display.mover){return true;}}}function posFromMouse(cm, e, liberal, forRect){var display=cm.display;if(!liberal && e_target(e).getAttribute("cm-not-content") == "true"){return null;}var x, y, space=display.lineSpace.getBoundingClientRect();try{x = e.clientX - space.left;y = e.clientY - space.top;}catch(e) {return null;}var coords=coordsChar(cm, x, y), line;if(forRect && coords.xRel == 1 && (line = getLine(cm.doc, coords.line).text).length == coords.ch){var colDiff=countColumn(line, line.length, cm.options.tabSize) - line.length;coords = Pos(coords.line, Math.max(0, Math.round((x - paddingH(cm.display).left) / charWidth(cm.display)) - colDiff));}return coords;}function onMouseDown(e){var cm=this, display=cm.display;if(display.activeTouch && display.input.supportsTouch() || signalDOMEvent(cm, e)){return;}display.shift = e.shiftKey;if(eventInWidget(display, e)){if(!webkit){display.scroller.draggable = false;setTimeout(function(){display.scroller.draggable = true;}, 100);}return;}if(clickInGutter(cm, e)){return;}var start=posFromMouse(cm, e);window.focus();switch(e_button(e)){case 1:if(start)leftButtonDown(cm, e, start);else if(e_target(e) == display.scroller)e_preventDefault(e);break;case 2:if(webkit)cm.state.lastMiddleDown = +new Date();if(start)extendSelection(cm.doc, start);setTimeout(function(){display.input.focus();}, 20);e_preventDefault(e);break;case 3:if(captureRightClick)onContextMenu(cm, e);else delayBlurEvent(cm);break;}}var lastClick, lastDoubleClick;function leftButtonDown(cm, e, start){if(ie)setTimeout(bind(ensureFocus, cm), 0);else cm.curOp.focus = activeElt();var now=+new Date(), type;if(lastDoubleClick && lastDoubleClick.time > now - 400 && cmp(lastDoubleClick.pos, start) == 0){type = "triple";}else if(lastClick && lastClick.time > now - 400 && cmp(lastClick.pos, start) == 0){type = "double";lastDoubleClick = {time:now, pos:start};}else {type = "single";lastClick = {time:now, pos:start};}var sel=cm.doc.sel, modifier=mac?e.metaKey:e.ctrlKey, contained;if(cm.options.dragDrop && dragAndDrop && !isReadOnly(cm) && type == "single" && (contained = sel.contains(start)) > -1 && !sel.ranges[contained].empty())leftButtonStartDrag(cm, e, start, modifier);else leftButtonSelect(cm, e, start, type, modifier);}function leftButtonStartDrag(cm, e, start, modifier){var display=cm.display, startTime=+new Date();var dragEnd=operation(cm, function(e2){if(webkit)display.scroller.draggable = false;cm.state.draggingText = false;off(document, "mouseup", dragEnd);off(display.scroller, "drop", dragEnd);if(Math.abs(e.clientX - e2.clientX) + Math.abs(e.clientY - e2.clientY) < 10){e_preventDefault(e2);if(!modifier && +new Date() - 200 < startTime)extendSelection(cm.doc, start);if(webkit || ie && ie_version == 9)setTimeout(function(){document.body.focus();display.input.focus();}, 20);else display.input.focus();}});if(webkit)display.scroller.draggable = true;cm.state.draggingText = dragEnd;if(display.scroller.dragDrop)display.scroller.dragDrop();on(document, "mouseup", dragEnd);on(display.scroller, "drop", dragEnd);}function leftButtonSelect(cm, e, start, type, addNew){var display=cm.display, doc=cm.doc;e_preventDefault(e);var ourRange, ourIndex, startSel=doc.sel, ranges=startSel.ranges;if(addNew && !e.shiftKey){ourIndex = doc.sel.contains(start);if(ourIndex > -1)ourRange = ranges[ourIndex];else ourRange = new Range(start, start);}else {ourRange = doc.sel.primary();ourIndex = doc.sel.primIndex;}if(e.altKey){type = "rect";if(!addNew)ourRange = new Range(start, start);start = posFromMouse(cm, e, true, true);ourIndex = -1;}else if(type == "double"){var word=cm.findWordAt(start);if(cm.display.shift || doc.extend)ourRange = extendRange(doc, ourRange, word.anchor, word.head);else ourRange = word;}else if(type == "triple"){var line=new Range(Pos(start.line, 0), clipPos(doc, Pos(start.line + 1, 0)));if(cm.display.shift || doc.extend)ourRange = extendRange(doc, ourRange, line.anchor, line.head);else ourRange = line;}else {ourRange = extendRange(doc, ourRange, start);}if(!addNew){ourIndex = 0;setSelection(doc, new Selection([ourRange], 0), sel_mouse);startSel = doc.sel;}else if(ourIndex == -1){ourIndex = ranges.length;setSelection(doc, normalizeSelection(ranges.concat([ourRange]), ourIndex), {scroll:false, origin:"*mouse"});}else if(ranges.length > 1 && ranges[ourIndex].empty() && type == "single" && !e.shiftKey){setSelection(doc, normalizeSelection(ranges.slice(0, ourIndex).concat(ranges.slice(ourIndex + 1)), 0));startSel = doc.sel;}else {replaceOneSelection(doc, ourIndex, ourRange, sel_mouse);}var lastPos=start;function extendTo(pos){if(cmp(lastPos, pos) == 0){return;}lastPos = pos;if(type == "rect"){var ranges=[], tabSize=cm.options.tabSize;var startCol=countColumn(getLine(doc, start.line).text, start.ch, tabSize);var posCol=countColumn(getLine(doc, pos.line).text, pos.ch, tabSize);var left=Math.min(startCol, posCol), right=Math.max(startCol, posCol);for(var line=Math.min(start.line, pos.line), end=Math.min(cm.lastLine(), Math.max(start.line, pos.line)); line <= end; line++) {var text=getLine(doc, line).text, leftPos=findColumn(text, left, tabSize);if(left == right)ranges.push(new Range(Pos(line, leftPos), Pos(line, leftPos)));else if(text.length > leftPos)ranges.push(new Range(Pos(line, leftPos), Pos(line, findColumn(text, right, tabSize))));}if(!ranges.length)ranges.push(new Range(start, start));setSelection(doc, normalizeSelection(startSel.ranges.slice(0, ourIndex).concat(ranges), ourIndex), {origin:"*mouse", scroll:false});cm.scrollIntoView(pos);}else {var oldRange=ourRange;var anchor=oldRange.anchor, head=pos;if(type != "single"){if(type == "double")var range=cm.findWordAt(pos);else var range=new Range(Pos(pos.line, 0), clipPos(doc, Pos(pos.line + 1, 0)));if(cmp(range.anchor, anchor) > 0){head = range.head;anchor = minPos(oldRange.from(), range.anchor);}else {head = range.anchor;anchor = maxPos(oldRange.to(), range.head);}}var ranges=startSel.ranges.slice(0);ranges[ourIndex] = new Range(clipPos(doc, anchor), head);setSelection(doc, normalizeSelection(ranges, ourIndex), sel_mouse);}}var editorSize=display.wrapper.getBoundingClientRect();var counter=0;function extend(e){var curCount=++counter;var cur=posFromMouse(cm, e, true, type == "rect");if(!cur){return;}if(cmp(cur, lastPos) != 0){cm.curOp.focus = activeElt();extendTo(cur);var visible=visibleLines(display, doc);if(cur.line >= visible.to || cur.line < visible.from)setTimeout(operation(cm, function(){if(counter == curCount)extend(e);}), 150);}else {var outside=e.clientY < editorSize.top?-20:e.clientY > editorSize.bottom?20:0;if(outside)setTimeout(operation(cm, function(){if(counter != curCount)return;display.scroller.scrollTop += outside;extend(e);}), 50);}}function done(e){counter = Infinity;e_preventDefault(e);display.input.focus();off(document, "mousemove", move);off(document, "mouseup", up);doc.history.lastSelOrigin = null;}var move=operation(cm, function(e){if(!e_button(e))done(e);else extend(e);});var up=operation(cm, done);on(document, "mousemove", move);on(document, "mouseup", up);}function gutterEvent(cm, e, type, prevent, signalfn){try{var mX=e.clientX, mY=e.clientY;}catch(e) {return false;}if(mX >= Math.floor(cm.display.gutters.getBoundingClientRect().right)){return false;}if(prevent)e_preventDefault(e);var display=cm.display;var lineBox=display.lineDiv.getBoundingClientRect();if(mY > lineBox.bottom || !hasHandler(cm, type)){return e_defaultPrevented(e);}mY -= lineBox.top - display.viewOffset;for(var i=0; i < cm.options.gutters.length; ++i) {var g=display.gutters.childNodes[i];if(g && g.getBoundingClientRect().right >= mX){var line=lineAtHeight(cm.doc, mY);var gutter=cm.options.gutters[i];signalfn(cm, type, cm, line, gutter, e);return e_defaultPrevented(e);}}}function clickInGutter(cm, e){return gutterEvent(cm, e, "gutterClick", true, signalLater);}var lastDrop=0;function onDrop(e){var cm=this;if(signalDOMEvent(cm, e) || eventInWidget(cm.display, e)){return;}e_preventDefault(e);if(ie)lastDrop = +new Date();var pos=posFromMouse(cm, e, true), files=e.dataTransfer.files;if(!pos || isReadOnly(cm)){return;}if(files && files.length && window.FileReader && window.File){var n=files.length, text=Array(n), read=0;var loadFile=function loadFile(file, i){var reader=new FileReader();reader.onload = operation(cm, function(){text[i] = reader.result;if(++read == n){pos = clipPos(cm.doc, pos);var change={from:pos, to:pos, text:splitLines(text.join("\n")), origin:"paste"};makeChange(cm.doc, change);setSelectionReplaceHistory(cm.doc, simpleSelection(pos, changeEnd(change)));}});reader.readAsText(file);};for(var i=0; i < n; ++i) loadFile(files[i], i);}else {if(cm.state.draggingText && cm.doc.sel.contains(pos) > -1){cm.state.draggingText(e);setTimeout(function(){cm.display.input.focus();}, 20);return;}try{var text=e.dataTransfer.getData("Text");if(text){if(cm.state.draggingText && !(mac?e.altKey:e.ctrlKey))var selected=cm.listSelections();setSelectionNoUndo(cm.doc, simpleSelection(pos, pos));if(selected)for(var i=0; i < selected.length; ++i) replaceRange(cm.doc, "", selected[i].anchor, selected[i].head, "drag");cm.replaceSelection(text, "around", "paste");cm.display.input.focus();}}catch(e) {}}}function onDragStart(cm, e){if(ie && (!cm.state.draggingText || +new Date() - lastDrop < 100)){e_stop(e);return;}if(signalDOMEvent(cm, e) || eventInWidget(cm.display, e)){return;}e.dataTransfer.setData("Text", cm.getSelection());if(e.dataTransfer.setDragImage && !safari){var img=elt("img", null, null, "position: fixed; left: 0; top: 0;");img.src = "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==";if(presto){img.width = img.height = 1;cm.display.wrapper.appendChild(img);img._top = img.offsetTop;}e.dataTransfer.setDragImage(img, 0, 0);if(presto)img.parentNode.removeChild(img);}}function setScrollTop(cm, val){if(Math.abs(cm.doc.scrollTop - val) < 2){return;}cm.doc.scrollTop = val;if(!gecko)updateDisplaySimple(cm, {top:val});if(cm.display.scroller.scrollTop != val)cm.display.scroller.scrollTop = val;cm.display.scrollbars.setScrollTop(val);if(gecko)updateDisplaySimple(cm);startWorker(cm, 100);}function setScrollLeft(cm, val, isScroller){if(isScroller?val == cm.doc.scrollLeft:Math.abs(cm.doc.scrollLeft - val) < 2){return;}val = Math.min(val, cm.display.scroller.scrollWidth - cm.display.scroller.clientWidth);cm.doc.scrollLeft = val;alignHorizontally(cm);if(cm.display.scroller.scrollLeft != val)cm.display.scroller.scrollLeft = val;cm.display.scrollbars.setScrollLeft(val);}var wheelSamples=0, wheelPixelsPerUnit=null;if(ie)wheelPixelsPerUnit = -0.53;else if(gecko)wheelPixelsPerUnit = 15;else if(chrome)wheelPixelsPerUnit = -0.7;else if(safari)wheelPixelsPerUnit = -1 / 3;var wheelEventDelta=function wheelEventDelta(e){var dx=e.wheelDeltaX, dy=e.wheelDeltaY;if(dx == null && e.detail && e.axis == e.HORIZONTAL_AXIS)dx = e.detail;if(dy == null && e.detail && e.axis == e.VERTICAL_AXIS)dy = e.detail;else if(dy == null)dy = e.wheelDelta;return {x:dx, y:dy};};CodeMirror.wheelEventPixels = function(e){var delta=wheelEventDelta(e);delta.x *= wheelPixelsPerUnit;delta.y *= wheelPixelsPerUnit;return delta;};function onScrollWheel(cm, e){var delta=wheelEventDelta(e), dx=delta.x, dy=delta.y;var display=cm.display, scroll=display.scroller;if(!(dx && scroll.scrollWidth > scroll.clientWidth || dy && scroll.scrollHeight > scroll.clientHeight)){return;}if(dy && mac && webkit){outer: for(var cur=e.target, view=display.view; cur != scroll; cur = cur.parentNode) {for(var i=0; i < view.length; i++) {if(view[i].node == cur){cm.display.currentWheelTarget = cur;break outer;}}}}if(dx && !gecko && !presto && wheelPixelsPerUnit != null){if(dy)setScrollTop(cm, Math.max(0, Math.min(scroll.scrollTop + dy * wheelPixelsPerUnit, scroll.scrollHeight - scroll.clientHeight)));setScrollLeft(cm, Math.max(0, Math.min(scroll.scrollLeft + dx * wheelPixelsPerUnit, scroll.scrollWidth - scroll.clientWidth)));e_preventDefault(e);display.wheelStartX = null;return;}if(dy && wheelPixelsPerUnit != null){var pixels=dy * wheelPixelsPerUnit;var top=cm.doc.scrollTop, bot=top + display.wrapper.clientHeight;if(pixels < 0)top = Math.max(0, top + pixels - 50);else bot = Math.min(cm.doc.height, bot + pixels + 50);updateDisplaySimple(cm, {top:top, bottom:bot});}if(wheelSamples < 20){if(display.wheelStartX == null){display.wheelStartX = scroll.scrollLeft;display.wheelStartY = scroll.scrollTop;display.wheelDX = dx;display.wheelDY = dy;setTimeout(function(){if(display.wheelStartX == null)return;var movedX=scroll.scrollLeft - display.wheelStartX;var movedY=scroll.scrollTop - display.wheelStartY;var sample=movedY && display.wheelDY && movedY / display.wheelDY || movedX && display.wheelDX && movedX / display.wheelDX;display.wheelStartX = display.wheelStartY = null;if(!sample)return;wheelPixelsPerUnit = (wheelPixelsPerUnit * wheelSamples + sample) / (wheelSamples + 1);++wheelSamples;}, 200);}else {display.wheelDX += dx;display.wheelDY += dy;}}}function doHandleBinding(cm, bound, dropShift){if(typeof bound == "string"){bound = commands[bound];if(!bound){return false;}}cm.display.input.ensurePolled();var prevShift=cm.display.shift, done=false;try{if(isReadOnly(cm))cm.state.suppressEdits = true;if(dropShift)cm.display.shift = false;done = bound(cm) != Pass;}finally {cm.display.shift = prevShift;cm.state.suppressEdits = false;}return done;}function lookupKeyForEditor(cm, name, handle){for(var i=0; i < cm.state.keyMaps.length; i++) {var result=lookupKey(name, cm.state.keyMaps[i], handle, cm);if(result){return result;}}return cm.options.extraKeys && lookupKey(name, cm.options.extraKeys, handle, cm) || lookupKey(name, cm.options.keyMap, handle, cm);}var stopSeq=new Delayed();function dispatchKey(cm, name, e, handle){var seq=cm.state.keySeq;if(seq){if(isModifierKey(name)){return "handled";}stopSeq.set(50, function(){if(cm.state.keySeq == seq){cm.state.keySeq = null;cm.display.input.reset();}});name = seq + " " + name;}var result=lookupKeyForEditor(cm, name, handle);if(result == "multi")cm.state.keySeq = name;if(result == "handled")signalLater(cm, "keyHandled", cm, name, e);if(result == "handled" || result == "multi"){e_preventDefault(e);restartBlink(cm);}if(seq && !result && /\'$/.test(name)){e_preventDefault(e);return true;}return !!result;}function handleKeyBinding(cm, e){var name=keyName(e, true);if(!name){return false;}if(e.shiftKey && !cm.state.keySeq){return dispatchKey(cm, "Shift-" + name, e, function(b){return doHandleBinding(cm, b, true);}) || dispatchKey(cm, name, e, function(b){if(typeof b == "string"?/^go[A-Z]/.test(b):b.motion)return doHandleBinding(cm, b);});}else {return dispatchKey(cm, name, e, function(b){return doHandleBinding(cm, b);});}}function handleCharBinding(cm, e, ch){return dispatchKey(cm, "'" + ch + "'", e, function(b){return doHandleBinding(cm, b, true);});}var lastStoppedKey=null;function onKeyDown(e){var cm=this;cm.curOp.focus = activeElt();if(signalDOMEvent(cm, e)){return;}if(ie && ie_version < 11 && e.keyCode == 27)e.returnValue = false;var code=e.keyCode;cm.display.shift = code == 16 || e.shiftKey;var handled=handleKeyBinding(cm, e);if(presto){lastStoppedKey = handled?code:null;if(!handled && code == 88 && !hasCopyEvent && (mac?e.metaKey:e.ctrlKey))cm.replaceSelection("", null, "cut");}if(code == 18 && !/\bCodeMirror-crosshair\b/.test(cm.display.lineDiv.className))showCrossHair(cm);}function showCrossHair(cm){var lineDiv=cm.display.lineDiv;addClass(lineDiv, "CodeMirror-crosshair");function up(e){if(e.keyCode == 18 || !e.altKey){rmClass(lineDiv, "CodeMirror-crosshair");off(document, "keyup", up);off(document, "mouseover", up);}}on(document, "keyup", up);on(document, "mouseover", up);}function onKeyUp(e){if(e.keyCode == 16)this.doc.sel.shift = false;signalDOMEvent(this, e);}function onKeyPress(e){var cm=this;if(eventInWidget(cm.display, e) || signalDOMEvent(cm, e) || e.ctrlKey && !e.altKey || mac && e.metaKey){return;}var keyCode=e.keyCode, charCode=e.charCode;if(presto && keyCode == lastStoppedKey){lastStoppedKey = null;e_preventDefault(e);return;}if(presto && (!e.which || e.which < 10) && handleKeyBinding(cm, e)){return;}var ch=String.fromCharCode(charCode == null?keyCode:charCode);if(handleCharBinding(cm, e, ch)){return;}cm.display.input.onKeyPress(e);}function delayBlurEvent(cm){cm.state.delayingBlurEvent = true;setTimeout(function(){if(cm.state.delayingBlurEvent){cm.state.delayingBlurEvent = false;onBlur(cm);}}, 100);}function onFocus(cm){if(cm.state.delayingBlurEvent)cm.state.delayingBlurEvent = false;if(cm.options.readOnly == "nocursor"){return;}if(!cm.state.focused){signal(cm, "focus", cm);cm.state.focused = true;addClass(cm.display.wrapper, "CodeMirror-focused");if(!cm.curOp && cm.display.selForContextMenu != cm.doc.sel){cm.display.input.reset();if(webkit)setTimeout(function(){cm.display.input.reset(true);}, 20);}cm.display.input.receivedFocus();}restartBlink(cm);}function onBlur(cm){if(cm.state.delayingBlurEvent){return;}if(cm.state.focused){signal(cm, "blur", cm);cm.state.focused = false;rmClass(cm.display.wrapper, "CodeMirror-focused");}clearInterval(cm.display.blinker);setTimeout(function(){if(!cm.state.focused)cm.display.shift = false;}, 150);}function onContextMenu(cm, e){if(eventInWidget(cm.display, e) || contextMenuInGutter(cm, e)){return;}cm.display.input.onContextMenu(e);}function contextMenuInGutter(cm, e){if(!hasHandler(cm, "gutterContextMenu")){return false;}return gutterEvent(cm, e, "gutterContextMenu", false, signal);}var changeEnd=CodeMirror.changeEnd = function(change){if(!change.text)return change.to;return Pos(change.from.line + change.text.length - 1, lst(change.text).length + (change.text.length == 1?change.from.ch:0));};function adjustForChange(pos, change){if(cmp(pos, change.from) < 0){return pos;}if(cmp(pos, change.to) <= 0){return changeEnd(change);}var line=pos.line + change.text.length - (change.to.line - change.from.line) - 1, ch=pos.ch;if(pos.line == change.to.line)ch += changeEnd(change).ch - change.to.ch;return Pos(line, ch);}function computeSelAfterChange(doc, change){var out=[];for(var i=0; i < doc.sel.ranges.length; i++) {var range=doc.sel.ranges[i];out.push(new Range(adjustForChange(range.anchor, change), adjustForChange(range.head, change)));}return normalizeSelection(out, doc.sel.primIndex);}function offsetPos(pos, old, nw){if(pos.line == old.line){return Pos(nw.line, pos.ch - old.ch + nw.ch);}else {return Pos(nw.line + (pos.line - old.line), pos.ch);}}function computeReplacedSel(doc, changes, hint){var out=[];var oldPrev=Pos(doc.first, 0), newPrev=oldPrev;for(var i=0; i < changes.length; i++) {var change=changes[i];var from=offsetPos(change.from, oldPrev, newPrev);var to=offsetPos(changeEnd(change), oldPrev, newPrev);oldPrev = change.to;newPrev = to;if(hint == "around"){var range=doc.sel.ranges[i], inv=cmp(range.head, range.anchor) < 0;out[i] = new Range(inv?to:from, inv?from:to);}else {out[i] = new Range(from, from);}}return new Selection(out, doc.sel.primIndex);}function filterChange(doc, change, update){var obj={canceled:false, from:change.from, to:change.to, text:change.text, origin:change.origin, cancel:function cancel(){this.canceled = true;}};if(update)obj.update = function(from, to, text, origin){if(from)this.from = clipPos(doc, from);if(to)this.to = clipPos(doc, to);if(text)this.text = text;if(origin !== undefined)this.origin = origin;};signal(doc, "beforeChange", doc, obj);if(doc.cm)signal(doc.cm, "beforeChange", doc.cm, obj);if(obj.canceled){return null;}return {from:obj.from, to:obj.to, text:obj.text, origin:obj.origin};}function makeChange(doc, change, ignoreReadOnly){if(doc.cm){if(!doc.cm.curOp){return operation(doc.cm, makeChange)(doc, change, ignoreReadOnly);}if(doc.cm.state.suppressEdits){return;}}if(hasHandler(doc, "beforeChange") || doc.cm && hasHandler(doc.cm, "beforeChange")){change = filterChange(doc, change, true);if(!change){return;}}var split=sawReadOnlySpans && !ignoreReadOnly && removeReadOnlyRanges(doc, change.from, change.to);if(split){for(var i=split.length - 1; i >= 0; --i) makeChangeInner(doc, {from:split[i].from, to:split[i].to, text:i?[""]:change.text});}else {makeChangeInner(doc, change);}}function makeChangeInner(doc, change){if(change.text.length == 1 && change.text[0] == "" && cmp(change.from, change.to) == 0){return;}var selAfter=computeSelAfterChange(doc, change);addChangeToHistory(doc, change, selAfter, doc.cm?doc.cm.curOp.id:NaN);makeChangeSingleDoc(doc, change, selAfter, stretchSpansOverChange(doc, change));var rebased=[];linkedDocs(doc, function(doc, sharedHist){if(!sharedHist && indexOf(rebased, doc.history) == -1){rebaseHist(doc.history, change);rebased.push(doc.history);}makeChangeSingleDoc(doc, change, null, stretchSpansOverChange(doc, change));});}function makeChangeFromHistory(doc, type, allowSelectionOnly){if(doc.cm && doc.cm.state.suppressEdits){return;}var hist=doc.history, event, selAfter=doc.sel;var source=type == "undo"?hist.done:hist.undone, dest=type == "undo"?hist.undone:hist.done;for(var i=0; i < source.length; i++) {event = source[i];if(allowSelectionOnly?event.ranges && !event.equals(doc.sel):!event.ranges)break;}if(i == source.length){return;}hist.lastOrigin = hist.lastSelOrigin = null;for(;;) {event = source.pop();if(event.ranges){pushSelectionToHistory(event, dest);if(allowSelectionOnly && !event.equals(doc.sel)){setSelection(doc, event, {clearRedo:false});return;}selAfter = event;}else break;}var antiChanges=[];pushSelectionToHistory(selAfter, dest);dest.push({changes:antiChanges, generation:hist.generation});hist.generation = event.generation || ++hist.maxGeneration;var filter=hasHandler(doc, "beforeChange") || doc.cm && hasHandler(doc.cm, "beforeChange");for(var i=event.changes.length - 1; i >= 0; --i) {var change=event.changes[i];change.origin = type;if(filter && !filterChange(doc, change, false)){source.length = 0;return;}antiChanges.push(historyChangeFromChange(doc, change));var after=i?computeSelAfterChange(doc, change):lst(source);makeChangeSingleDoc(doc, change, after, mergeOldSpans(doc, change));if(!i && doc.cm)doc.cm.scrollIntoView({from:change.from, to:changeEnd(change)});var rebased=[];linkedDocs(doc, function(doc, sharedHist){if(!sharedHist && indexOf(rebased, doc.history) == -1){rebaseHist(doc.history, change);rebased.push(doc.history);}makeChangeSingleDoc(doc, change, null, mergeOldSpans(doc, change));});}}function shiftDoc(doc, distance){if(distance == 0){return;}doc.first += distance;doc.sel = new Selection(map(doc.sel.ranges, function(range){return new Range(Pos(range.anchor.line + distance, range.anchor.ch), Pos(range.head.line + distance, range.head.ch));}), doc.sel.primIndex);if(doc.cm){regChange(doc.cm, doc.first, doc.first - distance, distance);for(var d=doc.cm.display, l=d.viewFrom; l < d.viewTo; l++) regLineChange(doc.cm, l, "gutter");}}function makeChangeSingleDoc(doc, change, selAfter, spans){if(doc.cm && !doc.cm.curOp){return operation(doc.cm, makeChangeSingleDoc)(doc, change, selAfter, spans);}if(change.to.line < doc.first){shiftDoc(doc, change.text.length - 1 - (change.to.line - change.from.line));return;}if(change.from.line > doc.lastLine()){return;}if(change.from.line < doc.first){var shift=change.text.length - 1 - (doc.first - change.from.line);shiftDoc(doc, shift);change = {from:Pos(doc.first, 0), to:Pos(change.to.line + shift, change.to.ch), text:[lst(change.text)], origin:change.origin};}var last=doc.lastLine();if(change.to.line > last){change = {from:change.from, to:Pos(last, getLine(doc, last).text.length), text:[change.text[0]], origin:change.origin};}change.removed = getBetween(doc, change.from, change.to);if(!selAfter)selAfter = computeSelAfterChange(doc, change);if(doc.cm)makeChangeSingleDocInEditor(doc.cm, change, spans);else updateDoc(doc, change, spans);setSelectionNoUndo(doc, selAfter, sel_dontScroll);}function makeChangeSingleDocInEditor(cm, change, spans){var doc=cm.doc, display=cm.display, from=change.from, to=change.to;var recomputeMaxLength=false, checkWidthStart=from.line;if(!cm.options.lineWrapping){checkWidthStart = lineNo(visualLine(getLine(doc, from.line)));doc.iter(checkWidthStart, to.line + 1, function(line){if(line == display.maxLine){recomputeMaxLength = true;return true;}});}if(doc.sel.contains(change.from, change.to) > -1)signalCursorActivity(cm);updateDoc(doc, change, spans, estimateHeight(cm));if(!cm.options.lineWrapping){doc.iter(checkWidthStart, from.line + change.text.length, function(line){var len=lineLength(line);if(len > display.maxLineLength){display.maxLine = line;display.maxLineLength = len;display.maxLineChanged = true;recomputeMaxLength = false;}});if(recomputeMaxLength)cm.curOp.updateMaxLine = true;}doc.frontier = Math.min(doc.frontier, from.line);startWorker(cm, 400);var lendiff=change.text.length - (to.line - from.line) - 1;if(change.full)regChange(cm);else if(from.line == to.line && change.text.length == 1 && !isWholeLineUpdate(cm.doc, change))regLineChange(cm, from.line, "text");else regChange(cm, from.line, to.line + 1, lendiff);var changesHandler=hasHandler(cm, "changes"), changeHandler=hasHandler(cm, "change");if(changeHandler || changesHandler){var obj={from:from, to:to, text:change.text, removed:change.removed, origin:change.origin};if(changeHandler)signalLater(cm, "change", cm, obj);if(changesHandler)(cm.curOp.changeObjs || (cm.curOp.changeObjs = [])).push(obj);}cm.display.selForContextMenu = null;}function replaceRange(doc, code, from, to, origin){if(!to)to = from;if(cmp(to, from) < 0){var tmp=to;to = from;from = tmp;}if(typeof code == "string")code = splitLines(code);makeChange(doc, {from:from, to:to, text:code, origin:origin});}function maybeScrollWindow(cm, coords){if(signalDOMEvent(cm, "scrollCursorIntoView")){return;}var display=cm.display, box=display.sizer.getBoundingClientRect(), doScroll=null;if(coords.top + box.top < 0)doScroll = true;else if(coords.bottom + box.top > (window.innerHeight || document.documentElement.clientHeight))doScroll = false;if(doScroll != null && !phantom){var scrollNode=elt("div", "​", null, "position: absolute; top: " + (coords.top - display.viewOffset - paddingTop(cm.display)) + "px; height: " + (coords.bottom - coords.top + scrollGap(cm) + display.barHeight) + "px; left: " + coords.left + "px; width: 2px;");cm.display.lineSpace.appendChild(scrollNode);scrollNode.scrollIntoView(doScroll);cm.display.lineSpace.removeChild(scrollNode);}}function scrollPosIntoView(cm, pos, end, margin){if(margin == null)margin = 0;for(var limit=0; limit < 5; limit++) {var changed=false, coords=cursorCoords(cm, pos);var endCoords=!end || end == pos?coords:cursorCoords(cm, end);var scrollPos=calculateScrollPos(cm, Math.min(coords.left, endCoords.left), Math.min(coords.top, endCoords.top) - margin, Math.max(coords.left, endCoords.left), Math.max(coords.bottom, endCoords.bottom) + margin);var startTop=cm.doc.scrollTop, startLeft=cm.doc.scrollLeft;if(scrollPos.scrollTop != null){setScrollTop(cm, scrollPos.scrollTop);if(Math.abs(cm.doc.scrollTop - startTop) > 1)changed = true;}if(scrollPos.scrollLeft != null){setScrollLeft(cm, scrollPos.scrollLeft);if(Math.abs(cm.doc.scrollLeft - startLeft) > 1)changed = true;}if(!changed)break;}return coords;}function scrollIntoView(cm, x1, y1, x2, y2){var scrollPos=calculateScrollPos(cm, x1, y1, x2, y2);if(scrollPos.scrollTop != null)setScrollTop(cm, scrollPos.scrollTop);if(scrollPos.scrollLeft != null)setScrollLeft(cm, scrollPos.scrollLeft);}function calculateScrollPos(cm, x1, y1, x2, y2){var display=cm.display, snapMargin=textHeight(cm.display);if(y1 < 0)y1 = 0;var screentop=cm.curOp && cm.curOp.scrollTop != null?cm.curOp.scrollTop:display.scroller.scrollTop;var screen=displayHeight(cm), result={};if(y2 - y1 > screen)y2 = y1 + screen;var docBottom=cm.doc.height + paddingVert(display);var atTop=y1 < snapMargin, atBottom=y2 > docBottom - snapMargin;if(y1 < screentop){result.scrollTop = atTop?0:y1;}else if(y2 > screentop + screen){var newTop=Math.min(y1, (atBottom?docBottom:y2) - screen);if(newTop != screentop)result.scrollTop = newTop;}var screenleft=cm.curOp && cm.curOp.scrollLeft != null?cm.curOp.scrollLeft:display.scroller.scrollLeft;var screenw=displayWidth(cm) - (cm.options.fixedGutter?display.gutters.offsetWidth:0);var tooWide=x2 - x1 > screenw;if(tooWide)x2 = x1 + screenw;if(x1 < 10)result.scrollLeft = 0;else if(x1 < screenleft)result.scrollLeft = Math.max(0, x1 - (tooWide?0:10));else if(x2 > screenw + screenleft - 3)result.scrollLeft = x2 + (tooWide?0:10) - screenw;return result;}function addToScrollPos(cm, left, top){if(left != null || top != null)resolveScrollToPos(cm);if(left != null)cm.curOp.scrollLeft = (cm.curOp.scrollLeft == null?cm.doc.scrollLeft:cm.curOp.scrollLeft) + left;if(top != null)cm.curOp.scrollTop = (cm.curOp.scrollTop == null?cm.doc.scrollTop:cm.curOp.scrollTop) + top;}function ensureCursorVisible(cm){resolveScrollToPos(cm);var cur=cm.getCursor(), from=cur, to=cur;if(!cm.options.lineWrapping){from = cur.ch?Pos(cur.line, cur.ch - 1):cur;to = Pos(cur.line, cur.ch + 1);}cm.curOp.scrollToPos = {from:from, to:to, margin:cm.options.cursorScrollMargin, isCursor:true};}function resolveScrollToPos(cm){var range=cm.curOp.scrollToPos;if(range){cm.curOp.scrollToPos = null;var from=estimateCoords(cm, range.from), to=estimateCoords(cm, range.to);var sPos=calculateScrollPos(cm, Math.min(from.left, to.left), Math.min(from.top, to.top) - range.margin, Math.max(from.right, to.right), Math.max(from.bottom, to.bottom) + range.margin);cm.scrollTo(sPos.scrollLeft, sPos.scrollTop);}}function indentLine(cm, n, how, aggressive){var doc=cm.doc, state;if(how == null)how = "add";if(how == "smart"){if(!doc.mode.indent)how = "prev";else state = getStateBefore(cm, n);}var tabSize=cm.options.tabSize;var line=getLine(doc, n), curSpace=countColumn(line.text, null, tabSize);if(line.stateAfter)line.stateAfter = null;var curSpaceString=line.text.match(/^\s*/)[0], indentation;if(!aggressive && !/\S/.test(line.text)){indentation = 0;how = "not";}else if(how == "smart"){indentation = doc.mode.indent(state, line.text.slice(curSpaceString.length), line.text);if(indentation == Pass || indentation > 150){if(!aggressive){return;}how = "prev";}}if(how == "prev"){if(n > doc.first)indentation = countColumn(getLine(doc, n - 1).text, null, tabSize);else indentation = 0;}else if(how == "add"){indentation = curSpace + cm.options.indentUnit;}else if(how == "subtract"){indentation = curSpace - cm.options.indentUnit;}else if(typeof how == "number"){indentation = curSpace + how;}indentation = Math.max(0, indentation);var indentString="", pos=0;if(cm.options.indentWithTabs)for(var i=Math.floor(indentation / tabSize); i; --i) {pos += tabSize;indentString += "\t";}if(pos < indentation)indentString += spaceStr(indentation - pos);if(indentString != curSpaceString){replaceRange(doc, indentString, Pos(n, 0), Pos(n, curSpaceString.length), "+input");line.stateAfter = null;return true;}else {for(var i=0; i < doc.sel.ranges.length; i++) {var range=doc.sel.ranges[i];if(range.head.line == n && range.head.ch < curSpaceString.length){var pos=Pos(n, curSpaceString.length);replaceOneSelection(doc, i, new Range(pos, pos));break;}}}}function changeLine(doc, handle, changeType, op){var no=handle, line=handle;if(typeof handle == "number")line = getLine(doc, clipLine(doc, handle));else no = lineNo(handle);if(no == null){return null;}if(op(line, no) && doc.cm)regLineChange(doc.cm, no, changeType);return line;}function deleteNearSelection(cm, compute){var ranges=cm.doc.sel.ranges, kill=[];for(var i=0; i < ranges.length; i++) {var toKill=compute(ranges[i]);while(kill.length && cmp(toKill.from, lst(kill).to) <= 0) {var replaced=kill.pop();if(cmp(replaced.from, toKill.from) < 0){toKill.from = replaced.from;break;}}kill.push(toKill);}runInOp(cm, function(){for(var i=kill.length - 1; i >= 0; i--) replaceRange(cm.doc, "", kill[i].from, kill[i].to, "+delete");ensureCursorVisible(cm);});}function findPosH(doc, pos, dir, unit, visually){var line=pos.line, ch=pos.ch, origDir=dir;var lineObj=getLine(doc, line);var possible=true;function findNextLine(){var l=line + dir;if(l < doc.first || l >= doc.first + doc.size){return possible = false;}line = l;return lineObj = getLine(doc, l);}function moveOnce(boundToLine){var next=(visually?moveVisually:moveLogically)(lineObj, ch, dir, true);if(next == null){if(!boundToLine && findNextLine()){if(visually)ch = (dir < 0?lineRight:lineLeft)(lineObj);else ch = dir < 0?lineObj.text.length:0;}else {return possible = false;}}else ch = next;return true;}if(unit == "char")moveOnce();else if(unit == "column")moveOnce(true);else if(unit == "word" || unit == "group"){var sawType=null, group=unit == "group";var helper=doc.cm && doc.cm.getHelper(pos, "wordChars");for(var first=true;; first = false) {if(dir < 0 && !moveOnce(!first))break;var cur=lineObj.text.charAt(ch) || "\n";var type=isWordChar(cur, helper)?"w":group && cur == "\n"?"n":!group || /\s/.test(cur)?null:"p";if(group && !first && !type)type = "s";if(sawType && sawType != type){if(dir < 0){dir = 1;moveOnce();}break;}if(type)sawType = type;if(dir > 0 && !moveOnce(!first))break;}}var result=skipAtomic(doc, Pos(line, ch), origDir, true);if(!possible)result.hitSide = true;return result;}function findPosV(cm, pos, dir, unit){var doc=cm.doc, x=pos.left, y;if(unit == "page"){var pageSize=Math.min(cm.display.wrapper.clientHeight, window.innerHeight || document.documentElement.clientHeight);y = pos.top + dir * (pageSize - (dir < 0?1.5:0.5) * textHeight(cm.display));}else if(unit == "line"){y = dir > 0?pos.bottom + 3:pos.top - 3;}for(;;) {var target=coordsChar(cm, x, y);if(!target.outside)break;if(dir < 0?y <= 0:y >= doc.height){target.hitSide = true;break;}y += dir * 5;}return target;}CodeMirror.prototype = {constructor:CodeMirror, focus:function focus(){window.focus();this.display.input.focus();}, setOption:function setOption(option, value){var options=this.options, old=options[option];if(options[option] == value && option != "mode"){return;}options[option] = value;if(optionHandlers.hasOwnProperty(option))operation(this, optionHandlers[option])(this, value, old);}, getOption:function getOption(option){return this.options[option];}, getDoc:function getDoc(){return this.doc;}, addKeyMap:function addKeyMap(map, bottom){this.state.keyMaps[bottom?"push":"unshift"](getKeyMap(map));}, removeKeyMap:function removeKeyMap(map){var maps=this.state.keyMaps;for(var i=0; i < maps.length; ++i) if(maps[i] == map || maps[i].name == map){maps.splice(i, 1);return true;}}, addOverlay:methodOp(function(spec, options){var mode=spec.token?spec:CodeMirror.getMode(this.options, spec);if(mode.startState)throw new Error("Overlays may not be stateful.");this.state.overlays.push({mode:mode, modeSpec:spec, opaque:options && options.opaque});this.state.modeGen++;regChange(this);}), removeOverlay:methodOp(function(spec){var overlays=this.state.overlays;for(var i=0; i < overlays.length; ++i) {var cur=overlays[i].modeSpec;if(cur == spec || typeof spec == "string" && cur.name == spec){overlays.splice(i, 1);this.state.modeGen++;regChange(this);return;}}}), indentLine:methodOp(function(n, dir, aggressive){if(typeof dir != "string" && typeof dir != "number"){if(dir == null)dir = this.options.smartIndent?"smart":"prev";else dir = dir?"add":"subtract";}if(isLine(this.doc, n))indentLine(this, n, dir, aggressive);}), indentSelection:methodOp(function(how){var ranges=this.doc.sel.ranges, end=-1;for(var i=0; i < ranges.length; i++) {var range=ranges[i];if(!range.empty()){var from=range.from(), to=range.to();var start=Math.max(end, from.line);end = Math.min(this.lastLine(), to.line - (to.ch?0:1)) + 1;for(var j=start; j < end; ++j) indentLine(this, j, how);var newRanges=this.doc.sel.ranges;if(from.ch == 0 && ranges.length == newRanges.length && newRanges[i].from().ch > 0)replaceOneSelection(this.doc, i, new Range(from, newRanges[i].to()), sel_dontScroll);}else if(range.head.line > end){indentLine(this, range.head.line, how, true);end = range.head.line;if(i == this.doc.sel.primIndex)ensureCursorVisible(this);}}}), getTokenAt:function getTokenAt(pos, precise){return takeToken(this, pos, precise);}, getLineTokens:function getLineTokens(line, precise){return takeToken(this, Pos(line), precise, true);}, getTokenTypeAt:function getTokenTypeAt(pos){pos = clipPos(this.doc, pos);var styles=getLineStyles(this, getLine(this.doc, pos.line));var before=0, after=(styles.length - 1) / 2, ch=pos.ch;var type;if(ch == 0)type = styles[2];else for(;;) {var mid=before + after >> 1;if((mid?styles[mid * 2 - 1]:0) >= ch)after = mid;else if(styles[mid * 2 + 1] < ch)before = mid + 1;else {type = styles[mid * 2 + 2];break;}}var cut=type?type.indexOf("cm-overlay "):-1;return cut < 0?type:cut == 0?null:type.slice(0, cut - 1);}, getModeAt:function getModeAt(pos){var mode=this.doc.mode;if(!mode.innerMode){return mode;}return CodeMirror.innerMode(mode, this.getTokenAt(pos).state).mode;}, getHelper:function getHelper(pos, type){return this.getHelpers(pos, type)[0];}, getHelpers:function getHelpers(pos, type){var found=[];if(!helpers.hasOwnProperty(type)){return found;}var help=helpers[type], mode=this.getModeAt(pos);if(typeof mode[type] == "string"){if(help[mode[type]])found.push(help[mode[type]]);}else if(mode[type]){for(var i=0; i < mode[type].length; i++) {var val=help[mode[type][i]];if(val)found.push(val);}}else if(mode.helperType && help[mode.helperType]){found.push(help[mode.helperType]);}else if(help[mode.name]){found.push(help[mode.name]);}for(var i=0; i < help._global.length; i++) {var cur=help._global[i];if(cur.pred(mode, this) && indexOf(found, cur.val) == -1)found.push(cur.val);}return found;}, getStateAfter:function getStateAfter(line, precise){var doc=this.doc;line = clipLine(doc, line == null?doc.first + doc.size - 1:line);return getStateBefore(this, line + 1, precise);}, cursorCoords:(function(_cursorCoords){var _cursorCoordsWrapper=function cursorCoords(_x, _x2){return _cursorCoords.apply(this, arguments);};_cursorCoordsWrapper.toString = function(){return _cursorCoords.toString();};return _cursorCoordsWrapper;})(function(start, mode){var pos, range=this.doc.sel.primary();if(start == null)pos = range.head;else if(typeof start == "object")pos = clipPos(this.doc, start);else pos = start?range.from():range.to();return cursorCoords(this, pos, mode || "page");}), charCoords:(function(_charCoords){var _charCoordsWrapper=function charCoords(_x, _x2){return _charCoords.apply(this, arguments);};_charCoordsWrapper.toString = function(){return _charCoords.toString();};return _charCoordsWrapper;})(function(pos, mode){return charCoords(this, clipPos(this.doc, pos), mode || "page");}), coordsChar:(function(_coordsChar){var _coordsCharWrapper=function coordsChar(_x, _x2){return _coordsChar.apply(this, arguments);};_coordsCharWrapper.toString = function(){return _coordsChar.toString();};return _coordsCharWrapper;})(function(coords, mode){coords = fromCoordSystem(this, coords, mode || "page");return coordsChar(this, coords.left, coords.top);}), lineAtHeight:(function(_lineAtHeight){var _lineAtHeightWrapper=function lineAtHeight(_x, _x2){return _lineAtHeight.apply(this, arguments);};_lineAtHeightWrapper.toString = function(){return _lineAtHeight.toString();};return _lineAtHeightWrapper;})(function(height, mode){height = fromCoordSystem(this, {top:height, left:0}, mode || "page").top;return lineAtHeight(this.doc, height + this.display.viewOffset);}), heightAtLine:(function(_heightAtLine){var _heightAtLineWrapper=function heightAtLine(_x, _x2){return _heightAtLine.apply(this, arguments);};_heightAtLineWrapper.toString = function(){return _heightAtLine.toString();};return _heightAtLineWrapper;})(function(line, mode){var end=false, lineObj;if(typeof line == "number"){var last=this.doc.first + this.doc.size - 1;if(line < this.doc.first)line = this.doc.first;else if(line > last){line = last;end = true;}lineObj = getLine(this.doc, line);}else {lineObj = line;}return intoCoordSystem(this, lineObj, {top:0, left:0}, mode || "page").top + (end?this.doc.height - heightAtLine(lineObj):0);}), defaultTextHeight:function defaultTextHeight(){return textHeight(this.display);}, defaultCharWidth:function defaultCharWidth(){return charWidth(this.display);}, setGutterMarker:methodOp(function(line, gutterID, value){return changeLine(this.doc, line, "gutter", function(line){var markers=line.gutterMarkers || (line.gutterMarkers = {});markers[gutterID] = value;if(!value && isEmpty(markers))line.gutterMarkers = null;return true;});}), clearGutter:methodOp(function(gutterID){var cm=this, doc=cm.doc, i=doc.first;doc.iter(function(line){if(line.gutterMarkers && line.gutterMarkers[gutterID]){line.gutterMarkers[gutterID] = null;regLineChange(cm, i, "gutter");if(isEmpty(line.gutterMarkers))line.gutterMarkers = null;}++i;});}), lineInfo:function lineInfo(line){if(typeof line == "number"){if(!isLine(this.doc, line)){return null;}var n=line;line = getLine(this.doc, line);if(!line){return null;}}else {var n=lineNo(line);if(n == null){return null;}}return {line:n, handle:line, text:line.text, gutterMarkers:line.gutterMarkers, textClass:line.textClass, bgClass:line.bgClass, wrapClass:line.wrapClass, widgets:line.widgets};}, getViewport:function getViewport(){return {from:this.display.viewFrom, to:this.display.viewTo};}, addWidget:function addWidget(pos, node, scroll, vert, horiz){var display=this.display;pos = cursorCoords(this, clipPos(this.doc, pos));var top=pos.bottom, left=pos.left;node.style.position = "absolute";node.setAttribute("cm-ignore-events", "true");this.display.input.setUneditable(node);display.sizer.appendChild(node);if(vert == "over"){top = pos.top;}else if(vert == "above" || vert == "near"){var vspace=Math.max(display.wrapper.clientHeight, this.doc.height), hspace=Math.max(display.sizer.clientWidth, display.lineSpace.clientWidth);if((vert == "above" || pos.bottom + node.offsetHeight > vspace) && pos.top > node.offsetHeight)top = pos.top - node.offsetHeight;else if(pos.bottom + node.offsetHeight <= vspace)top = pos.bottom;if(left + node.offsetWidth > hspace)left = hspace - node.offsetWidth;}node.style.top = top + "px";node.style.left = node.style.right = "";if(horiz == "right"){left = display.sizer.clientWidth - node.offsetWidth;node.style.right = "0px";}else {if(horiz == "left")left = 0;else if(horiz == "middle")left = (display.sizer.clientWidth - node.offsetWidth) / 2;node.style.left = left + "px";}if(scroll)scrollIntoView(this, left, top, left + node.offsetWidth, top + node.offsetHeight);}, triggerOnKeyDown:methodOp(onKeyDown), triggerOnKeyPress:methodOp(onKeyPress), triggerOnKeyUp:onKeyUp, execCommand:function execCommand(cmd){if(commands.hasOwnProperty(cmd)){return commands[cmd](this);}}, triggerElectric:methodOp(function(text){triggerElectric(this, text);}), findPosH:(function(_findPosH){var _findPosHWrapper=function findPosH(_x, _x2, _x3, _x4){return _findPosH.apply(this, arguments);};_findPosHWrapper.toString = function(){return _findPosH.toString();};return _findPosHWrapper;})(function(from, amount, unit, visually){var dir=1;if(amount < 0){dir = -1;amount = -amount;}for(var i=0, cur=clipPos(this.doc, from); i < amount; ++i) {cur = findPosH(this.doc, cur, dir, unit, visually);if(cur.hitSide)break;}return cur;}), moveH:methodOp(function(dir, unit){var cm=this;cm.extendSelectionsBy(function(range){if(cm.display.shift || cm.doc.extend || range.empty())return findPosH(cm.doc, range.head, dir, unit, cm.options.rtlMoveVisually);else return dir < 0?range.from():range.to();}, sel_move);}), deleteH:methodOp(function(dir, unit){var sel=this.doc.sel, doc=this.doc;if(sel.somethingSelected())doc.replaceSelection("", null, "+delete");else deleteNearSelection(this, function(range){var other=findPosH(doc, range.head, dir, unit, false);return dir < 0?{from:other, to:range.head}:{from:range.head, to:other};});}), findPosV:(function(_findPosV){var _findPosVWrapper=function findPosV(_x, _x2, _x3, _x4){return _findPosV.apply(this, arguments);};_findPosVWrapper.toString = function(){return _findPosV.toString();};return _findPosVWrapper;})(function(from, amount, unit, goalColumn){var dir=1, x=goalColumn;if(amount < 0){dir = -1;amount = -amount;}for(var i=0, cur=clipPos(this.doc, from); i < amount; ++i) {var coords=cursorCoords(this, cur, "div");if(x == null)x = coords.left;else coords.left = x;cur = findPosV(this, coords, dir, unit);if(cur.hitSide)break;}return cur;}), moveV:methodOp(function(dir, unit){var cm=this, doc=this.doc, goals=[];var collapse=!cm.display.shift && !doc.extend && doc.sel.somethingSelected();doc.extendSelectionsBy(function(range){if(collapse)return dir < 0?range.from():range.to();var headPos=cursorCoords(cm, range.head, "div");if(range.goalColumn != null)headPos.left = range.goalColumn;goals.push(headPos.left);var pos=findPosV(cm, headPos, dir, unit);if(unit == "page" && range == doc.sel.primary())addToScrollPos(cm, null, charCoords(cm, pos, "div").top - headPos.top);return pos;}, sel_move);if(goals.length)for(var i=0; i < doc.sel.ranges.length; i++) doc.sel.ranges[i].goalColumn = goals[i];}), findWordAt:function findWordAt(pos){var doc=this.doc, line=getLine(doc, pos.line).text;var start=pos.ch, end=pos.ch;if(line){var helper=this.getHelper(pos, "wordChars");if((pos.xRel < 0 || end == line.length) && start)--start;else ++end;var startChar=line.charAt(start);var check=isWordChar(startChar, helper)?function(ch){return isWordChar(ch, helper);}:/\s/.test(startChar)?function(ch){return /\s/.test(ch);}:function(ch){return !/\s/.test(ch) && !isWordChar(ch);};while(start > 0 && check(line.charAt(start - 1))) --start;while(end < line.length && check(line.charAt(end))) ++end;}return new Range(Pos(pos.line, start), Pos(pos.line, end));}, toggleOverwrite:function toggleOverwrite(value){if(value != null && value == this.state.overwrite){return;}if(this.state.overwrite = !this.state.overwrite)addClass(this.display.cursorDiv, "CodeMirror-overwrite");else rmClass(this.display.cursorDiv, "CodeMirror-overwrite");signal(this, "overwriteToggle", this, this.state.overwrite);}, hasFocus:function hasFocus(){return this.display.input.getField() == activeElt();}, scrollTo:methodOp(function(x, y){if(x != null || y != null)resolveScrollToPos(this);if(x != null)this.curOp.scrollLeft = x;if(y != null)this.curOp.scrollTop = y;}), getScrollInfo:function getScrollInfo(){var scroller=this.display.scroller;return {left:scroller.scrollLeft, top:scroller.scrollTop, height:scroller.scrollHeight - scrollGap(this) - this.display.barHeight, width:scroller.scrollWidth - scrollGap(this) - this.display.barWidth, clientHeight:displayHeight(this), clientWidth:displayWidth(this)};}, scrollIntoView:methodOp(function(range, margin){if(range == null){range = {from:this.doc.sel.primary().head, to:null};if(margin == null)margin = this.options.cursorScrollMargin;}else if(typeof range == "number"){range = {from:Pos(range, 0), to:null};}else if(range.from == null){range = {from:range, to:null};}if(!range.to)range.to = range.from;range.margin = margin || 0;if(range.from.line != null){resolveScrollToPos(this);this.curOp.scrollToPos = range;}else {var sPos=calculateScrollPos(this, Math.min(range.from.left, range.to.left), Math.min(range.from.top, range.to.top) - range.margin, Math.max(range.from.right, range.to.right), Math.max(range.from.bottom, range.to.bottom) + range.margin);this.scrollTo(sPos.scrollLeft, sPos.scrollTop);}}), setSize:methodOp(function(width, height){var cm=this;function interpret(val){return typeof val == "number" || /^\d+$/.test(String(val))?val + "px":val;}if(width != null)cm.display.wrapper.style.width = interpret(width);if(height != null)cm.display.wrapper.style.height = interpret(height);if(cm.options.lineWrapping)clearLineMeasurementCache(this);var lineNo=cm.display.viewFrom;cm.doc.iter(lineNo, cm.display.viewTo, function(line){if(line.widgets)for(var i=0; i < line.widgets.length; i++) if(line.widgets[i].noHScroll){regLineChange(cm, lineNo, "widget");break;}++lineNo;});cm.curOp.forceUpdate = true;signal(cm, "refresh", this);}), operation:function operation(f){return runInOp(this, f);}, refresh:methodOp(function(){var oldHeight=this.display.cachedTextHeight;regChange(this);this.curOp.forceUpdate = true;clearCaches(this);this.scrollTo(this.doc.scrollLeft, this.doc.scrollTop);updateGutterSpace(this);if(oldHeight == null || Math.abs(oldHeight - textHeight(this.display)) > 0.5)estimateLineHeights(this);signal(this, "refresh", this);}), swapDoc:methodOp(function(doc){var old=this.doc;old.cm = null;attachDoc(this, doc);clearCaches(this);this.display.input.reset();this.scrollTo(doc.scrollLeft, doc.scrollTop);this.curOp.forceScroll = true;signalLater(this, "swapDoc", this, old);return old;}), getInputField:function getInputField(){return this.display.input.getField();}, getWrapperElement:function getWrapperElement(){return this.display.wrapper;}, getScrollerElement:function getScrollerElement(){return this.display.scroller;}, getGutterElement:function getGutterElement(){return this.display.gutters;}};eventMixin(CodeMirror);var defaults=CodeMirror.defaults = {};var optionHandlers=CodeMirror.optionHandlers = {};function option(name, deflt, handle, notOnInit){CodeMirror.defaults[name] = deflt;if(handle)optionHandlers[name] = notOnInit?function(cm, val, old){if(old != Init)handle(cm, val, old);}:handle;}var Init=CodeMirror.Init = {toString:function toString(){return "CodeMirror.Init";}};option("value", "", function(cm, val){cm.setValue(val);}, true);option("mode", null, function(cm, val){cm.doc.modeOption = val;loadMode(cm);}, true);option("indentUnit", 2, loadMode, true);option("indentWithTabs", false);option("smartIndent", true);option("tabSize", 4, function(cm){resetModeState(cm);clearCaches(cm);regChange(cm);}, true);option("specialChars", /[\t\u0000-\u0019\u00ad\u200b-\u200f\u2028\u2029\ufeff]/g, function(cm, val, old){cm.state.specialChars = new RegExp(val.source + (val.test("\t")?"":"|\t"), "g");if(old != CodeMirror.Init)cm.refresh();});option("specialCharPlaceholder", defaultSpecialCharPlaceholder, function(cm){cm.refresh();}, true);option("electricChars", true);option("inputStyle", mobile?"contenteditable":"textarea", function(){throw new Error("inputStyle can not (yet) be changed in a running editor");}, true);option("rtlMoveVisually", !windows);option("wholeLineUpdateBefore", true);option("theme", "default", function(cm){themeChanged(cm);guttersChanged(cm);}, true);option("keyMap", "default", function(cm, val, old){var next=getKeyMap(val);var prev=old != CodeMirror.Init && getKeyMap(old);if(prev && prev.detach)prev.detach(cm, next);if(next.attach)next.attach(cm, prev || null);});option("extraKeys", null);option("lineWrapping", false, wrappingChanged, true);option("gutters", [], function(cm){setGuttersForLineNumbers(cm.options);guttersChanged(cm);}, true);option("fixedGutter", true, function(cm, val){cm.display.gutters.style.left = val?compensateForHScroll(cm.display) + "px":"0";cm.refresh();}, true);option("coverGutterNextToScrollbar", false, function(cm){updateScrollbars(cm);}, true);option("scrollbarStyle", "native", function(cm){initScrollbars(cm);updateScrollbars(cm);cm.display.scrollbars.setScrollTop(cm.doc.scrollTop);cm.display.scrollbars.setScrollLeft(cm.doc.scrollLeft);}, true);option("lineNumbers", false, function(cm){setGuttersForLineNumbers(cm.options);guttersChanged(cm);}, true);option("firstLineNumber", 1, guttersChanged, true);option("lineNumberFormatter", function(integer){return integer;}, guttersChanged, true);option("showCursorWhenSelecting", false, updateSelection, true);option("resetSelectionOnContextMenu", true);option("lineWiseCopyCut", true);option("readOnly", false, function(cm, val){if(val == "nocursor"){onBlur(cm);cm.display.input.blur();cm.display.disabled = true;}else {cm.display.disabled = false;if(!val)cm.display.input.reset();}});option("disableInput", false, function(cm, val){if(!val)cm.display.input.reset();}, true);option("dragDrop", true, dragDropChanged);option("cursorBlinkRate", 530);option("cursorScrollMargin", 0);option("cursorHeight", 1, updateSelection, true);option("singleCursorHeightPerLine", true, updateSelection, true);option("workTime", 100);option("workDelay", 100);option("flattenSpans", true, resetModeState, true);option("addModeClass", false, resetModeState, true);option("pollInterval", 100);option("undoDepth", 200, function(cm, val){cm.doc.history.undoDepth = val;});option("historyEventDelay", 1250);option("viewportMargin", 10, function(cm){cm.refresh();}, true);option("maxHighlightLength", 10000, resetModeState, true);option("moveInputWithCursor", true, function(cm, val){if(!val)cm.display.input.resetPosition();});option("tabindex", null, function(cm, val){cm.display.input.getField().tabIndex = val || "";});option("autofocus", null);var modes=CodeMirror.modes = {}, mimeModes=CodeMirror.mimeModes = {};CodeMirror.defineMode = function(name, mode){if(!CodeMirror.defaults.mode && name != "null")CodeMirror.defaults.mode = name;if(arguments.length > 2)mode.dependencies = Array.prototype.slice.call(arguments, 2);modes[name] = mode;};CodeMirror.defineMIME = function(mime, spec){mimeModes[mime] = spec;};CodeMirror.resolveMode = function(spec){if(typeof spec == "string" && mimeModes.hasOwnProperty(spec)){spec = mimeModes[spec];}else if(spec && typeof spec.name == "string" && mimeModes.hasOwnProperty(spec.name)){var found=mimeModes[spec.name];if(typeof found == "string")found = {name:found};spec = createObj(found, spec);spec.name = found.name;}else if(typeof spec == "string" && /^[\w\-]+\/[\w\-]+\+xml$/.test(spec)){return CodeMirror.resolveMode("application/xml");}if(typeof spec == "string")return {name:spec};else return spec || {name:"null"};};CodeMirror.getMode = function(options, spec){var spec=CodeMirror.resolveMode(spec);var mfactory=modes[spec.name];if(!mfactory)return CodeMirror.getMode(options, "text/plain");var modeObj=mfactory(options, spec);if(modeExtensions.hasOwnProperty(spec.name)){var exts=modeExtensions[spec.name];for(var prop in exts) {if(!exts.hasOwnProperty(prop))continue;if(modeObj.hasOwnProperty(prop))modeObj["_" + prop] = modeObj[prop];modeObj[prop] = exts[prop];}}modeObj.name = spec.name;if(spec.helperType)modeObj.helperType = spec.helperType;if(spec.modeProps)for(var prop in spec.modeProps) modeObj[prop] = spec.modeProps[prop];return modeObj;};CodeMirror.defineMode("null", function(){return {token:function token(stream){stream.skipToEnd();}};});CodeMirror.defineMIME("text/plain", "null");var modeExtensions=CodeMirror.modeExtensions = {};CodeMirror.extendMode = function(mode, properties){var exts=modeExtensions.hasOwnProperty(mode)?modeExtensions[mode]:modeExtensions[mode] = {};copyObj(properties, exts);};CodeMirror.defineExtension = function(name, func){CodeMirror.prototype[name] = func;};CodeMirror.defineDocExtension = function(name, func){Doc.prototype[name] = func;};CodeMirror.defineOption = option;var initHooks=[];CodeMirror.defineInitHook = function(f){initHooks.push(f);};var helpers=CodeMirror.helpers = {};CodeMirror.registerHelper = function(type, name, value){if(!helpers.hasOwnProperty(type))helpers[type] = CodeMirror[type] = {_global:[]};helpers[type][name] = value;};CodeMirror.registerGlobalHelper = function(type, name, predicate, value){CodeMirror.registerHelper(type, name, value);helpers[type]._global.push({pred:predicate, val:value});};var copyState=CodeMirror.copyState = function(mode, state){if(state === true)return state;if(mode.copyState)return mode.copyState(state);var nstate={};for(var n in state) {var val=state[n];if(val instanceof Array)val = val.concat([]);nstate[n] = val;}return nstate;};var startState=CodeMirror.startState = function(mode, a1, a2){return mode.startState?mode.startState(a1, a2):true;};CodeMirror.innerMode = function(mode, state){while(mode.innerMode) {var info=mode.innerMode(state);if(!info || info.mode == mode)break;state = info.state;mode = info.mode;}return info || {mode:mode, state:state};};var commands=CodeMirror.commands = {selectAll:function selectAll(cm){cm.setSelection(Pos(cm.firstLine(), 0), Pos(cm.lastLine()), sel_dontScroll);}, singleSelection:function singleSelection(cm){cm.setSelection(cm.getCursor("anchor"), cm.getCursor("head"), sel_dontScroll);}, killLine:function killLine(cm){deleteNearSelection(cm, function(range){if(range.empty()){var len=getLine(cm.doc, range.head.line).text.length;if(range.head.ch == len && range.head.line < cm.lastLine())return {from:range.head, to:Pos(range.head.line + 1, 0)};else return {from:range.head, to:Pos(range.head.line, len)};}else {return {from:range.from(), to:range.to()};}});}, deleteLine:function deleteLine(cm){deleteNearSelection(cm, function(range){return {from:Pos(range.from().line, 0), to:clipPos(cm.doc, Pos(range.to().line + 1, 0))};});}, delLineLeft:function delLineLeft(cm){deleteNearSelection(cm, function(range){return {from:Pos(range.from().line, 0), to:range.from()};});}, delWrappedLineLeft:function delWrappedLineLeft(cm){deleteNearSelection(cm, function(range){var top=cm.charCoords(range.head, "div").top + 5;var leftPos=cm.coordsChar({left:0, top:top}, "div");return {from:leftPos, to:range.from()};});}, delWrappedLineRight:function delWrappedLineRight(cm){deleteNearSelection(cm, function(range){var top=cm.charCoords(range.head, "div").top + 5;var rightPos=cm.coordsChar({left:cm.display.lineDiv.offsetWidth + 100, top:top}, "div");return {from:range.from(), to:rightPos};});}, undo:function undo(cm){cm.undo();}, redo:function redo(cm){cm.redo();}, undoSelection:function undoSelection(cm){cm.undoSelection();}, redoSelection:function redoSelection(cm){cm.redoSelection();}, goDocStart:function goDocStart(cm){cm.extendSelection(Pos(cm.firstLine(), 0));}, goDocEnd:function goDocEnd(cm){cm.extendSelection(Pos(cm.lastLine()));}, goLineStart:function goLineStart(cm){cm.extendSelectionsBy(function(range){return lineStart(cm, range.head.line);}, {origin:"+move", bias:1});}, goLineStartSmart:function goLineStartSmart(cm){cm.extendSelectionsBy(function(range){return lineStartSmart(cm, range.head);}, {origin:"+move", bias:1});}, goLineEnd:function goLineEnd(cm){cm.extendSelectionsBy(function(range){return lineEnd(cm, range.head.line);}, {origin:"+move", bias:-1});}, goLineRight:function goLineRight(cm){cm.extendSelectionsBy(function(range){var top=cm.charCoords(range.head, "div").top + 5;return cm.coordsChar({left:cm.display.lineDiv.offsetWidth + 100, top:top}, "div");}, sel_move);}, goLineLeft:function goLineLeft(cm){cm.extendSelectionsBy(function(range){var top=cm.charCoords(range.head, "div").top + 5;return cm.coordsChar({left:0, top:top}, "div");}, sel_move);}, goLineLeftSmart:function goLineLeftSmart(cm){cm.extendSelectionsBy(function(range){var top=cm.charCoords(range.head, "div").top + 5;var pos=cm.coordsChar({left:0, top:top}, "div");if(pos.ch < cm.getLine(pos.line).search(/\S/))return lineStartSmart(cm, range.head);return pos;}, sel_move);}, goLineUp:function goLineUp(cm){cm.moveV(-1, "line");}, goLineDown:function goLineDown(cm){cm.moveV(1, "line");}, goPageUp:function goPageUp(cm){cm.moveV(-1, "page");}, goPageDown:function goPageDown(cm){cm.moveV(1, "page");}, goCharLeft:function goCharLeft(cm){cm.moveH(-1, "char");}, goCharRight:function goCharRight(cm){cm.moveH(1, "char");}, goColumnLeft:function goColumnLeft(cm){cm.moveH(-1, "column");}, goColumnRight:function goColumnRight(cm){cm.moveH(1, "column");}, goWordLeft:function goWordLeft(cm){cm.moveH(-1, "word");}, goGroupRight:function goGroupRight(cm){cm.moveH(1, "group");}, goGroupLeft:function goGroupLeft(cm){cm.moveH(-1, "group");}, goWordRight:function goWordRight(cm){cm.moveH(1, "word");}, delCharBefore:function delCharBefore(cm){cm.deleteH(-1, "char");}, delCharAfter:function delCharAfter(cm){cm.deleteH(1, "char");}, delWordBefore:function delWordBefore(cm){cm.deleteH(-1, "word");}, delWordAfter:function delWordAfter(cm){cm.deleteH(1, "word");}, delGroupBefore:function delGroupBefore(cm){cm.deleteH(-1, "group");}, delGroupAfter:function delGroupAfter(cm){cm.deleteH(1, "group");}, indentAuto:function indentAuto(cm){cm.indentSelection("smart");}, indentMore:function indentMore(cm){cm.indentSelection("add");}, indentLess:function indentLess(cm){cm.indentSelection("subtract");}, insertTab:function insertTab(cm){cm.replaceSelection("\t");}, insertSoftTab:function insertSoftTab(cm){var spaces=[], ranges=cm.listSelections(), tabSize=cm.options.tabSize;for(var i=0; i < ranges.length; i++) {var pos=ranges[i].from();var col=countColumn(cm.getLine(pos.line), pos.ch, tabSize);spaces.push(new Array(tabSize - col % tabSize + 1).join(" "));}cm.replaceSelections(spaces);}, defaultTab:function defaultTab(cm){if(cm.somethingSelected())cm.indentSelection("add");else cm.execCommand("insertTab");}, transposeChars:function transposeChars(cm){runInOp(cm, function(){var ranges=cm.listSelections(), newSel=[];for(var i=0; i < ranges.length; i++) {var cur=ranges[i].head, line=getLine(cm.doc, cur.line).text;if(line){if(cur.ch == line.length)cur = new Pos(cur.line, cur.ch - 1);if(cur.ch > 0){cur = new Pos(cur.line, cur.ch + 1);cm.replaceRange(line.charAt(cur.ch - 1) + line.charAt(cur.ch - 2), Pos(cur.line, cur.ch - 2), cur, "+transpose");}else if(cur.line > cm.doc.first){var prev=getLine(cm.doc, cur.line - 1).text;if(prev)cm.replaceRange(line.charAt(0) + "\n" + prev.charAt(prev.length - 1), Pos(cur.line - 1, prev.length - 1), Pos(cur.line, 1), "+transpose");}}newSel.push(new Range(cur, cur));}cm.setSelections(newSel);});}, newlineAndIndent:function newlineAndIndent(cm){runInOp(cm, function(){var len=cm.listSelections().length;for(var i=0; i < len; i++) {var range=cm.listSelections()[i];cm.replaceRange("\n", range.anchor, range.head, "+input");cm.indentLine(range.from().line + 1, null, true);ensureCursorVisible(cm);}});}, toggleOverwrite:function toggleOverwrite(cm){cm.toggleOverwrite();}};var keyMap=CodeMirror.keyMap = {};keyMap.basic = {Left:"goCharLeft", Right:"goCharRight", Up:"goLineUp", Down:"goLineDown", End:"goLineEnd", Home:"goLineStartSmart", PageUp:"goPageUp", PageDown:"goPageDown", Delete:"delCharAfter", Backspace:"delCharBefore", "Shift-Backspace":"delCharBefore", Tab:"defaultTab", "Shift-Tab":"indentAuto", Enter:"newlineAndIndent", Insert:"toggleOverwrite", Esc:"singleSelection"};keyMap.pcDefault = {"Ctrl-A":"selectAll", "Ctrl-D":"deleteLine", "Ctrl-Z":"undo", "Shift-Ctrl-Z":"redo", "Ctrl-Y":"redo", "Ctrl-Home":"goDocStart", "Ctrl-End":"goDocEnd", "Ctrl-Up":"goLineUp", "Ctrl-Down":"goLineDown", "Ctrl-Left":"goGroupLeft", "Ctrl-Right":"goGroupRight", "Alt-Left":"goLineStart", "Alt-Right":"goLineEnd", "Ctrl-Backspace":"delGroupBefore", "Ctrl-Delete":"delGroupAfter", "Ctrl-S":"save", "Ctrl-F":"find", "Ctrl-G":"findNext", "Shift-Ctrl-G":"findPrev", "Shift-Ctrl-F":"replace", "Shift-Ctrl-R":"replaceAll", "Ctrl-[":"indentLess", "Ctrl-]":"indentMore", "Ctrl-U":"undoSelection", "Shift-Ctrl-U":"redoSelection", "Alt-U":"redoSelection", fallthrough:"basic"};keyMap.emacsy = {"Ctrl-F":"goCharRight", "Ctrl-B":"goCharLeft", "Ctrl-P":"goLineUp", "Ctrl-N":"goLineDown", "Alt-F":"goWordRight", "Alt-B":"goWordLeft", "Ctrl-A":"goLineStart", "Ctrl-E":"goLineEnd", "Ctrl-V":"goPageDown", "Shift-Ctrl-V":"goPageUp", "Ctrl-D":"delCharAfter", "Ctrl-H":"delCharBefore", "Alt-D":"delWordAfter", "Alt-Backspace":"delWordBefore", "Ctrl-K":"killLine", "Ctrl-T":"transposeChars"};keyMap.macDefault = {"Cmd-A":"selectAll", "Cmd-D":"deleteLine", "Cmd-Z":"undo", "Shift-Cmd-Z":"redo", "Cmd-Y":"redo", "Cmd-Home":"goDocStart", "Cmd-Up":"goDocStart", "Cmd-End":"goDocEnd", "Cmd-Down":"goDocEnd", "Alt-Left":"goGroupLeft", "Alt-Right":"goGroupRight", "Cmd-Left":"goLineLeft", "Cmd-Right":"goLineRight", "Alt-Backspace":"delGroupBefore", "Ctrl-Alt-Backspace":"delGroupAfter", "Alt-Delete":"delGroupAfter", "Cmd-S":"save", "Cmd-F":"find", "Cmd-G":"findNext", "Shift-Cmd-G":"findPrev", "Cmd-Alt-F":"replace", "Shift-Cmd-Alt-F":"replaceAll", "Cmd-[":"indentLess", "Cmd-]":"indentMore", "Cmd-Backspace":"delWrappedLineLeft", "Cmd-Delete":"delWrappedLineRight", "Cmd-U":"undoSelection", "Shift-Cmd-U":"redoSelection", "Ctrl-Up":"goDocStart", "Ctrl-Down":"goDocEnd", fallthrough:["basic", "emacsy"]};keyMap["default"] = mac?keyMap.macDefault:keyMap.pcDefault;function normalizeKeyName(name){var parts=name.split(/-(?!$)/), name=parts[parts.length - 1];var alt, ctrl, shift, cmd;for(var i=0; i < parts.length - 1; i++) {var mod=parts[i];if(/^(cmd|meta|m)$/i.test(mod))cmd = true;else if(/^a(lt)?$/i.test(mod))alt = true;else if(/^(c|ctrl|control)$/i.test(mod))ctrl = true;else if(/^s(hift)$/i.test(mod))shift = true;else throw new Error("Unrecognized modifier name: " + mod);}if(alt)name = "Alt-" + name;if(ctrl)name = "Ctrl-" + name;if(cmd)name = "Cmd-" + name;if(shift)name = "Shift-" + name;return name;}CodeMirror.normalizeKeyMap = function(keymap){var copy={};for(var keyname in keymap) if(keymap.hasOwnProperty(keyname)){var value=keymap[keyname];if(/^(name|fallthrough|(de|at)tach)$/.test(keyname))continue;if(value == "..."){delete keymap[keyname];continue;}var keys=map(keyname.split(" "), normalizeKeyName);for(var i=0; i < keys.length; i++) {var val, name;if(i == keys.length - 1){name = keys.join(" ");val = value;}else {name = keys.slice(0, i + 1).join(" ");val = "...";}var prev=copy[name];if(!prev)copy[name] = val;else if(prev != val)throw new Error("Inconsistent bindings for " + name);}delete keymap[keyname];}for(var prop in copy) keymap[prop] = copy[prop];return keymap;};var lookupKey=CodeMirror.lookupKey = function(key, map, handle, context){map = getKeyMap(map);var found=map.call?map.call(key, context):map[key];if(found === false)return "nothing";if(found === "...")return "multi";if(found != null && handle(found))return "handled";if(map.fallthrough){if(Object.prototype.toString.call(map.fallthrough) != "[object Array]")return lookupKey(key, map.fallthrough, handle, context);for(var i=0; i < map.fallthrough.length; i++) {var result=lookupKey(key, map.fallthrough[i], handle, context);if(result)return result;}}};var isModifierKey=CodeMirror.isModifierKey = function(value){var name=typeof value == "string"?value:keyNames[value.keyCode];return name == "Ctrl" || name == "Alt" || name == "Shift" || name == "Mod";};var keyName=CodeMirror.keyName = function(event, noShift){if(presto && event.keyCode == 34 && event.char)return false;var base=keyNames[event.keyCode], name=base;if(name == null || event.altGraphKey)return false;if(event.altKey && base != "Alt")name = "Alt-" + name;if((flipCtrlCmd?event.metaKey:event.ctrlKey) && base != "Ctrl")name = "Ctrl-" + name;if((flipCtrlCmd?event.ctrlKey:event.metaKey) && base != "Cmd")name = "Cmd-" + name;if(!noShift && event.shiftKey && base != "Shift")name = "Shift-" + name;return name;};function getKeyMap(val){return typeof val == "string"?keyMap[val]:val;}CodeMirror.fromTextArea = function(textarea, options){options = options?copyObj(options):{};options.value = textarea.value;if(!options.tabindex && textarea.tabIndex)options.tabindex = textarea.tabIndex;if(!options.placeholder && textarea.placeholder)options.placeholder = textarea.placeholder;if(options.autofocus == null){var hasFocus=activeElt();options.autofocus = hasFocus == textarea || textarea.getAttribute("autofocus") != null && hasFocus == document.body;}function save(){textarea.value = cm.getValue();}if(textarea.form){on(textarea.form, "submit", save);if(!options.leaveSubmitMethodAlone){var form=textarea.form, realSubmit=form.submit;try{var wrappedSubmit=form.submit = function(){save();form.submit = realSubmit;form.submit();form.submit = wrappedSubmit;};}catch(e) {}}}options.finishInit = function(cm){cm.save = save;cm.getTextArea = function(){return textarea;};cm.toTextArea = function(){cm.toTextArea = isNaN;save();textarea.parentNode.removeChild(cm.getWrapperElement());textarea.style.display = "";if(textarea.form){off(textarea.form, "submit", save);if(typeof textarea.form.submit == "function")textarea.form.submit = realSubmit;}};};textarea.style.display = "none";var cm=CodeMirror(function(node){textarea.parentNode.insertBefore(node, textarea.nextSibling);}, options);return cm;};var StringStream=CodeMirror.StringStream = function(string, tabSize){this.pos = this.start = 0;this.string = string;this.tabSize = tabSize || 8;this.lastColumnPos = this.lastColumnValue = 0;this.lineStart = 0;};StringStream.prototype = {eol:function eol(){return this.pos >= this.string.length;}, sol:function sol(){return this.pos == this.lineStart;}, peek:function peek(){return this.string.charAt(this.pos) || undefined;}, next:function next(){if(this.pos < this.string.length){return this.string.charAt(this.pos++);}}, eat:function eat(match){var ch=this.string.charAt(this.pos);if(typeof match == "string")var ok=ch == match;else var ok=ch && (match.test?match.test(ch):match(ch));if(ok){++this.pos;return ch;}}, eatWhile:function eatWhile(match){var start=this.pos;while(this.eat(match)) {}return this.pos > start;}, eatSpace:function eatSpace(){var start=this.pos;while(/[\s\u00a0]/.test(this.string.charAt(this.pos))) ++this.pos;return this.pos > start;}, skipToEnd:function skipToEnd(){this.pos = this.string.length;}, skipTo:function skipTo(ch){var found=this.string.indexOf(ch, this.pos);if(found > -1){this.pos = found;return true;}}, backUp:function backUp(n){this.pos -= n;}, column:function column(){if(this.lastColumnPos < this.start){this.lastColumnValue = countColumn(this.string, this.start, this.tabSize, this.lastColumnPos, this.lastColumnValue);this.lastColumnPos = this.start;}return this.lastColumnValue - (this.lineStart?countColumn(this.string, this.lineStart, this.tabSize):0);}, indentation:function indentation(){return countColumn(this.string, null, this.tabSize) - (this.lineStart?countColumn(this.string, this.lineStart, this.tabSize):0);}, match:(function(_match){var _matchWrapper=function match(_x, _x2, _x3){return _match.apply(this, arguments);};_matchWrapper.toString = function(){return _match.toString();};return _matchWrapper;})(function(pattern, consume, caseInsensitive){if(typeof pattern == "string"){var cased=function cased(str){return caseInsensitive?str.toLowerCase():str;};var substr=this.string.substr(this.pos, pattern.length);if(cased(substr) == cased(pattern)){if(consume !== false)this.pos += pattern.length;return true;}}else {var match=this.string.slice(this.pos).match(pattern);if(match && match.index > 0)return null;if(match && consume !== false)this.pos += match[0].length;return match;}}), current:function current(){return this.string.slice(this.start, this.pos);}, hideFirstChars:function hideFirstChars(n, inner){this.lineStart += n;try{return inner();}finally {this.lineStart -= n;}}};var nextMarkerId=0;var TextMarker=CodeMirror.TextMarker = function(doc, type){this.lines = [];this.type = type;this.doc = doc;this.id = ++nextMarkerId;};eventMixin(TextMarker);TextMarker.prototype.clear = function(){if(this.explicitlyCleared)return;var cm=this.doc.cm, withOp=cm && !cm.curOp;if(withOp)startOperation(cm);if(hasHandler(this, "clear")){var found=this.find();if(found)signalLater(this, "clear", found.from, found.to);}var min=null, max=null;for(var i=0; i < this.lines.length; ++i) {var line=this.lines[i];var span=getMarkedSpanFor(line.markedSpans, this);if(cm && !this.collapsed)regLineChange(cm, lineNo(line), "text");else if(cm){if(span.to != null)max = lineNo(line);if(span.from != null)min = lineNo(line);}line.markedSpans = removeMarkedSpan(line.markedSpans, span);if(span.from == null && this.collapsed && !lineIsHidden(this.doc, line) && cm)updateLineHeight(line, textHeight(cm.display));}if(cm && this.collapsed && !cm.options.lineWrapping)for(var i=0; i < this.lines.length; ++i) {var visual=visualLine(this.lines[i]), len=lineLength(visual);if(len > cm.display.maxLineLength){cm.display.maxLine = visual;cm.display.maxLineLength = len;cm.display.maxLineChanged = true;}}if(min != null && cm && this.collapsed)regChange(cm, min, max + 1);this.lines.length = 0;this.explicitlyCleared = true;if(this.atomic && this.doc.cantEdit){this.doc.cantEdit = false;if(cm)reCheckSelection(cm.doc);}if(cm)signalLater(cm, "markerCleared", cm, this);if(withOp)endOperation(cm);if(this.parent)this.parent.clear();};TextMarker.prototype.find = function(side, lineObj){if(side == null && this.type == "bookmark")side = 1;var from, to;for(var i=0; i < this.lines.length; ++i) {var line=this.lines[i];var span=getMarkedSpanFor(line.markedSpans, this);if(span.from != null){from = Pos(lineObj?line:lineNo(line), span.from);if(side == -1)return from;}if(span.to != null){to = Pos(lineObj?line:lineNo(line), span.to);if(side == 1)return to;}}return from && {from:from, to:to};};TextMarker.prototype.changed = function(){var pos=this.find(-1, true), widget=this, cm=this.doc.cm;if(!pos || !cm)return;runInOp(cm, function(){var line=pos.line, lineN=lineNo(pos.line);var view=findViewForLine(cm, lineN);if(view){clearLineMeasurementCacheFor(view);cm.curOp.selectionChanged = cm.curOp.forceUpdate = true;}cm.curOp.updateMaxLine = true;if(!lineIsHidden(widget.doc, line) && widget.height != null){var oldHeight=widget.height;widget.height = null;var dHeight=widgetHeight(widget) - oldHeight;if(dHeight)updateLineHeight(line, line.height + dHeight);}});};TextMarker.prototype.attachLine = function(line){if(!this.lines.length && this.doc.cm){var op=this.doc.cm.curOp;if(!op.maybeHiddenMarkers || indexOf(op.maybeHiddenMarkers, this) == -1)(op.maybeUnhiddenMarkers || (op.maybeUnhiddenMarkers = [])).push(this);}this.lines.push(line);};TextMarker.prototype.detachLine = function(line){this.lines.splice(indexOf(this.lines, line), 1);if(!this.lines.length && this.doc.cm){var op=this.doc.cm.curOp;(op.maybeHiddenMarkers || (op.maybeHiddenMarkers = [])).push(this);}};var nextMarkerId=0;function markText(doc, from, to, options, type){if(options && options.shared){return markTextShared(doc, from, to, options, type);}if(doc.cm && !doc.cm.curOp){return operation(doc.cm, markText)(doc, from, to, options, type);}var marker=new TextMarker(doc, type), diff=cmp(from, to);if(options)copyObj(options, marker, false);if(diff > 0 || diff == 0 && marker.clearWhenEmpty !== false){return marker;}if(marker.replacedWith){marker.collapsed = true;marker.widgetNode = elt("span", [marker.replacedWith], "CodeMirror-widget");if(!options.handleMouseEvents)marker.widgetNode.setAttribute("cm-ignore-events", "true");if(options.insertLeft)marker.widgetNode.insertLeft = true;}if(marker.collapsed){if(conflictingCollapsedRange(doc, from.line, from, to, marker) || from.line != to.line && conflictingCollapsedRange(doc, to.line, from, to, marker))throw new Error("Inserting collapsed marker partially overlapping an existing one");sawCollapsedSpans = true;}if(marker.addToHistory)addChangeToHistory(doc, {from:from, to:to, origin:"markText"}, doc.sel, NaN);var curLine=from.line, cm=doc.cm, updateMaxLine;doc.iter(curLine, to.line + 1, function(line){if(cm && marker.collapsed && !cm.options.lineWrapping && visualLine(line) == cm.display.maxLine)updateMaxLine = true;if(marker.collapsed && curLine != from.line)updateLineHeight(line, 0);addMarkedSpan(line, new MarkedSpan(marker, curLine == from.line?from.ch:null, curLine == to.line?to.ch:null));++curLine;});if(marker.collapsed)doc.iter(from.line, to.line + 1, function(line){if(lineIsHidden(doc, line))updateLineHeight(line, 0);});if(marker.clearOnEnter)on(marker, "beforeCursorEnter", function(){marker.clear();});if(marker.readOnly){sawReadOnlySpans = true;if(doc.history.done.length || doc.history.undone.length)doc.clearHistory();}if(marker.collapsed){marker.id = ++nextMarkerId;marker.atomic = true;}if(cm){if(updateMaxLine)cm.curOp.updateMaxLine = true;if(marker.collapsed)regChange(cm, from.line, to.line + 1);else if(marker.className || marker.title || marker.startStyle || marker.endStyle || marker.css)for(var i=from.line; i <= to.line; i++) regLineChange(cm, i, "text");if(marker.atomic)reCheckSelection(cm.doc);signalLater(cm, "markerAdded", cm, marker);}return marker;}var SharedTextMarker=CodeMirror.SharedTextMarker = function(markers, primary){this.markers = markers;this.primary = primary;for(var i=0; i < markers.length; ++i) markers[i].parent = this;};eventMixin(SharedTextMarker);SharedTextMarker.prototype.clear = function(){if(this.explicitlyCleared)return;this.explicitlyCleared = true;for(var i=0; i < this.markers.length; ++i) this.markers[i].clear();signalLater(this, "clear");};SharedTextMarker.prototype.find = function(side, lineObj){return this.primary.find(side, lineObj);};function markTextShared(doc, from, to, options, type){options = copyObj(options);options.shared = false;var markers=[markText(doc, from, to, options, type)], primary=markers[0];var widget=options.widgetNode;linkedDocs(doc, function(doc){if(widget)options.widgetNode = widget.cloneNode(true);markers.push(markText(doc, clipPos(doc, from), clipPos(doc, to), options, type));for(var i=0; i < doc.linked.length; ++i) if(doc.linked[i].isParent)return;primary = lst(markers);});return new SharedTextMarker(markers, primary);}function findSharedMarkers(doc){return doc.findMarks(Pos(doc.first, 0), doc.clipPos(Pos(doc.lastLine())), function(m){return m.parent;});}function copySharedMarkers(doc, markers){for(var i=0; i < markers.length; i++) {var marker=markers[i], pos=marker.find();var mFrom=doc.clipPos(pos.from), mTo=doc.clipPos(pos.to);if(cmp(mFrom, mTo)){var subMark=markText(doc, mFrom, mTo, marker.primary, marker.primary.type);marker.markers.push(subMark);subMark.parent = marker;}}}function detachSharedMarkers(markers){for(var i=0; i < markers.length; i++) {var marker=markers[i], linked=[marker.primary.doc];;linkedDocs(marker.primary.doc, function(d){linked.push(d);});for(var j=0; j < marker.markers.length; j++) {var subMarker=marker.markers[j];if(indexOf(linked, subMarker.doc) == -1){subMarker.parent = null;marker.markers.splice(j--, 1);}}}}function MarkedSpan(marker, from, to){this.marker = marker;this.from = from;this.to = to;}function getMarkedSpanFor(spans, marker){if(spans)for(var i=0; i < spans.length; ++i) {var span=spans[i];if(span.marker == marker){return span;}}}function removeMarkedSpan(spans, span){for(var r, i=0; i < spans.length; ++i) if(spans[i] != span)(r || (r = [])).push(spans[i]);return r;}function addMarkedSpan(line, span){line.markedSpans = line.markedSpans?line.markedSpans.concat([span]):[span];span.marker.attachLine(line);}function markedSpansBefore(old, startCh, isInsert){if(old)for(var i=0, nw; i < old.length; ++i) {var span=old[i], marker=span.marker;var startsBefore=span.from == null || (marker.inclusiveLeft?span.from <= startCh:span.from < startCh);if(startsBefore || span.from == startCh && marker.type == "bookmark" && (!isInsert || !span.marker.insertLeft)){var endsAfter=span.to == null || (marker.inclusiveRight?span.to >= startCh:span.to > startCh);(nw || (nw = [])).push(new MarkedSpan(marker, span.from, endsAfter?null:span.to));}}return nw;}function markedSpansAfter(old, endCh, isInsert){if(old)for(var i=0, nw; i < old.length; ++i) {var span=old[i], marker=span.marker;var endsAfter=span.to == null || (marker.inclusiveRight?span.to >= endCh:span.to > endCh);if(endsAfter || span.from == endCh && marker.type == "bookmark" && (!isInsert || span.marker.insertLeft)){var startsBefore=span.from == null || (marker.inclusiveLeft?span.from <= endCh:span.from < endCh);(nw || (nw = [])).push(new MarkedSpan(marker, startsBefore?null:span.from - endCh, span.to == null?null:span.to - endCh));}}return nw;}function stretchSpansOverChange(doc, change){if(change.full){return null;}var oldFirst=isLine(doc, change.from.line) && getLine(doc, change.from.line).markedSpans;var oldLast=isLine(doc, change.to.line) && getLine(doc, change.to.line).markedSpans;if(!oldFirst && !oldLast){return null;}var startCh=change.from.ch, endCh=change.to.ch, isInsert=cmp(change.from, change.to) == 0;var first=markedSpansBefore(oldFirst, startCh, isInsert);var last=markedSpansAfter(oldLast, endCh, isInsert);var sameLine=change.text.length == 1, offset=lst(change.text).length + (sameLine?startCh:0);if(first){for(var i=0; i < first.length; ++i) {var span=first[i];if(span.to == null){var found=getMarkedSpanFor(last, span.marker);if(!found)span.to = startCh;else if(sameLine)span.to = found.to == null?null:found.to + offset;}}}if(last){for(var i=0; i < last.length; ++i) {var span=last[i];if(span.to != null)span.to += offset;if(span.from == null){var found=getMarkedSpanFor(first, span.marker);if(!found){span.from = offset;if(sameLine)(first || (first = [])).push(span);}}else {span.from += offset;if(sameLine)(first || (first = [])).push(span);}}}if(first)first = clearEmptySpans(first);if(last && last != first)last = clearEmptySpans(last);var newMarkers=[first];if(!sameLine){var gap=change.text.length - 2, gapMarkers;if(gap > 0 && first)for(var i=0; i < first.length; ++i) if(first[i].to == null)(gapMarkers || (gapMarkers = [])).push(new MarkedSpan(first[i].marker, null, null));for(var i=0; i < gap; ++i) newMarkers.push(gapMarkers);newMarkers.push(last);}return newMarkers;}function clearEmptySpans(spans){for(var i=0; i < spans.length; ++i) {var span=spans[i];if(span.from != null && span.from == span.to && span.marker.clearWhenEmpty !== false)spans.splice(i--, 1);}if(!spans.length){return null;}return spans;}function mergeOldSpans(doc, change){var old=getOldSpans(doc, change);var stretched=stretchSpansOverChange(doc, change);if(!old){return stretched;}if(!stretched){return old;}for(var i=0; i < old.length; ++i) {var oldCur=old[i], stretchCur=stretched[i];if(oldCur && stretchCur){spans: for(var j=0; j < stretchCur.length; ++j) {var span=stretchCur[j];for(var k=0; k < oldCur.length; ++k) if(oldCur[k].marker == span.marker)continue spans;oldCur.push(span);}}else if(stretchCur){old[i] = stretchCur;}}return old;}function removeReadOnlyRanges(doc, from, to){var markers=null;doc.iter(from.line, to.line + 1, function(line){if(line.markedSpans)for(var i=0; i < line.markedSpans.length; ++i) {var mark=line.markedSpans[i].marker;if(mark.readOnly && (!markers || indexOf(markers, mark) == -1))(markers || (markers = [])).push(mark);}});if(!markers){return null;}var parts=[{from:from, to:to}];for(var i=0; i < markers.length; ++i) {var mk=markers[i], m=mk.find(0);for(var j=0; j < parts.length; ++j) {var p=parts[j];if(cmp(p.to, m.from) < 0 || cmp(p.from, m.to) > 0)continue;var newParts=[j, 1], dfrom=cmp(p.from, m.from), dto=cmp(p.to, m.to);if(dfrom < 0 || !mk.inclusiveLeft && !dfrom)newParts.push({from:p.from, to:m.from});if(dto > 0 || !mk.inclusiveRight && !dto)newParts.push({from:m.to, to:p.to});parts.splice.apply(parts, newParts);j += newParts.length - 1;}}return parts;}function detachMarkedSpans(line){var spans=line.markedSpans;if(!spans){return;}for(var i=0; i < spans.length; ++i) spans[i].marker.detachLine(line);line.markedSpans = null;}function attachMarkedSpans(line, spans){if(!spans){return;}for(var i=0; i < spans.length; ++i) spans[i].marker.attachLine(line);line.markedSpans = spans;}function extraLeft(marker){return marker.inclusiveLeft?-1:0;}function extraRight(marker){return marker.inclusiveRight?1:0;}function compareCollapsedMarkers(a, b){var lenDiff=a.lines.length - b.lines.length;if(lenDiff != 0){return lenDiff;}var aPos=a.find(), bPos=b.find();var fromCmp=cmp(aPos.from, bPos.from) || extraLeft(a) - extraLeft(b);if(fromCmp){return -fromCmp;}var toCmp=cmp(aPos.to, bPos.to) || extraRight(a) - extraRight(b);if(toCmp){return toCmp;}return b.id - a.id;}function collapsedSpanAtSide(line, start){var sps=sawCollapsedSpans && line.markedSpans, found;if(sps)for(var sp, i=0; i < sps.length; ++i) {sp = sps[i];if(sp.marker.collapsed && (start?sp.from:sp.to) == null && (!found || compareCollapsedMarkers(found, sp.marker) < 0))found = sp.marker;}return found;}function collapsedSpanAtStart(line){return collapsedSpanAtSide(line, true);}function collapsedSpanAtEnd(line){return collapsedSpanAtSide(line, false);}function conflictingCollapsedRange(doc, lineNo, from, to, marker){var line=getLine(doc, lineNo);var sps=sawCollapsedSpans && line.markedSpans;if(sps)for(var i=0; i < sps.length; ++i) {var sp=sps[i];if(!sp.marker.collapsed)continue;var found=sp.marker.find(0);var fromCmp=cmp(found.from, from) || extraLeft(sp.marker) - extraLeft(marker);var toCmp=cmp(found.to, to) || extraRight(sp.marker) - extraRight(marker);if(fromCmp >= 0 && toCmp <= 0 || fromCmp <= 0 && toCmp >= 0)continue;if(fromCmp <= 0 && (cmp(found.to, from) > 0 || sp.marker.inclusiveRight && marker.inclusiveLeft) || fromCmp >= 0 && (cmp(found.from, to) < 0 || sp.marker.inclusiveLeft && marker.inclusiveRight)){return true;}}}function visualLine(line){var merged;while(merged = collapsedSpanAtStart(line)) line = merged.find(-1, true).line;return line;}function visualLineContinued(line){var merged, lines;while(merged = collapsedSpanAtEnd(line)) {line = merged.find(1, true).line;(lines || (lines = [])).push(line);}return lines;}function visualLineNo(doc, lineN){var line=getLine(doc, lineN), vis=visualLine(line);if(line == vis){return lineN;}return lineNo(vis);}function visualLineEndNo(doc, lineN){if(lineN > doc.lastLine()){return lineN;}var line=getLine(doc, lineN), merged;if(!lineIsHidden(doc, line)){return lineN;}while(merged = collapsedSpanAtEnd(line)) line = merged.find(1, true).line;return lineNo(line) + 1;}function lineIsHidden(doc, line){var sps=sawCollapsedSpans && line.markedSpans;if(sps)for(var sp, i=0; i < sps.length; ++i) {sp = sps[i];if(!sp.marker.collapsed)continue;if(sp.from == null){return true;}if(sp.marker.widgetNode)continue;if(sp.from == 0 && sp.marker.inclusiveLeft && lineIsHiddenInner(doc, line, sp)){return true;}}}function lineIsHiddenInner(_x, _x2, _x3){var _again=true;_function: while(_again) {_again = false;var doc=_x, line=_x2, span=_x3;end = sp = i = undefined;if(span.to == null){var end=span.marker.find(1, true);_x = doc;_x2 = end.line;_x3 = getMarkedSpanFor(end.line.markedSpans, span.marker);_again = true;continue _function;}if(span.marker.inclusiveRight && span.to == line.text.length){return true;}for(var sp, i=0; i < line.markedSpans.length; ++i) {sp = line.markedSpans[i];if(sp.marker.collapsed && !sp.marker.widgetNode && sp.from == span.to && (sp.to == null || sp.to != span.from) && (sp.marker.inclusiveLeft || span.marker.inclusiveRight) && lineIsHiddenInner(doc, line, sp)){return true;}}}}var LineWidget=CodeMirror.LineWidget = function(doc, node, options){if(options)for(var opt in options) if(options.hasOwnProperty(opt))this[opt] = options[opt];this.doc = doc;this.node = node;};eventMixin(LineWidget);function adjustScrollWhenAboveVisible(cm, line, diff){if(heightAtLine(line) < (cm.curOp && cm.curOp.scrollTop || cm.doc.scrollTop))addToScrollPos(cm, null, diff);}LineWidget.prototype.clear = function(){var cm=this.doc.cm, ws=this.line.widgets, line=this.line, no=lineNo(line);if(no == null || !ws)return;for(var i=0; i < ws.length; ++i) if(ws[i] == this)ws.splice(i--, 1);if(!ws.length)line.widgets = null;var height=widgetHeight(this);updateLineHeight(line, Math.max(0, line.height - height));if(cm)runInOp(cm, function(){adjustScrollWhenAboveVisible(cm, line, -height);regLineChange(cm, no, "widget");});};LineWidget.prototype.changed = function(){var oldH=this.height, cm=this.doc.cm, line=this.line;this.height = null;var diff=widgetHeight(this) - oldH;if(!diff)return;updateLineHeight(line, line.height + diff);if(cm)runInOp(cm, function(){cm.curOp.forceUpdate = true;adjustScrollWhenAboveVisible(cm, line, diff);});};function widgetHeight(widget){if(widget.height != null){return widget.height;}var cm=widget.doc.cm;if(!cm){return 0;}if(!contains(document.body, widget.node)){var parentStyle="position: relative;";if(widget.coverGutter)parentStyle += "margin-left: -" + cm.display.gutters.offsetWidth + "px;";if(widget.noHScroll)parentStyle += "width: " + cm.display.wrapper.clientWidth + "px;";removeChildrenAndAdd(cm.display.measure, elt("div", [widget.node], null, parentStyle));}return widget.height = widget.node.offsetHeight;}function addLineWidget(doc, handle, node, options){var widget=new LineWidget(doc, node, options);var cm=doc.cm;if(cm && widget.noHScroll)cm.display.alignWidgets = true;changeLine(doc, handle, "widget", function(line){var widgets=line.widgets || (line.widgets = []);if(widget.insertAt == null)widgets.push(widget);else widgets.splice(Math.min(widgets.length - 1, Math.max(0, widget.insertAt)), 0, widget);widget.line = line;if(cm && !lineIsHidden(doc, line)){var aboveVisible=heightAtLine(line) < doc.scrollTop;updateLineHeight(line, line.height + widgetHeight(widget));if(aboveVisible)addToScrollPos(cm, null, widget.height);cm.curOp.forceUpdate = true;}return true;});return widget;}var Line=CodeMirror.Line = function(text, markedSpans, estimateHeight){this.text = text;attachMarkedSpans(this, markedSpans);this.height = estimateHeight?estimateHeight(this):1;};eventMixin(Line);Line.prototype.lineNo = function(){return lineNo(this);};function updateLine(line, text, markedSpans, estimateHeight){line.text = text;if(line.stateAfter)line.stateAfter = null;if(line.styles)line.styles = null;if(line.order != null)line.order = null;detachMarkedSpans(line);attachMarkedSpans(line, markedSpans);var estHeight=estimateHeight?estimateHeight(line):1;if(estHeight != line.height)updateLineHeight(line, estHeight);}function cleanUpLine(line){line.parent = null;detachMarkedSpans(line);}function extractLineClasses(type, output){if(type)for(;;) {var lineClass=type.match(/(?:^|\s+)line-(background-)?(\S+)/);if(!lineClass)break;type = type.slice(0, lineClass.index) + type.slice(lineClass.index + lineClass[0].length);var prop=lineClass[1]?"bgClass":"textClass";if(output[prop] == null)output[prop] = lineClass[2];else if(!new RegExp("(?:^|s)" + lineClass[2] + "(?:$|s)").test(output[prop]))output[prop] += " " + lineClass[2];}return type;}function callBlankLine(mode, state){if(mode.blankLine){return mode.blankLine(state);}if(!mode.innerMode){return;}var inner=CodeMirror.innerMode(mode, state);if(inner.mode.blankLine){return inner.mode.blankLine(inner.state);}}function readToken(mode, stream, state, inner){for(var i=0; i < 10; i++) {if(inner)inner[0] = CodeMirror.innerMode(mode, state).mode;var style=mode.token(stream, state);if(stream.pos > stream.start){return style;}}throw new Error("Mode " + mode.name + " failed to advance stream.");}function takeToken(cm, pos, precise, asArray){function getObj(copy){return {start:stream.start, end:stream.pos, string:stream.current(), type:style || null, state:copy?copyState(doc.mode, state):state};}var doc=cm.doc, mode=doc.mode, style;pos = clipPos(doc, pos);var line=getLine(doc, pos.line), state=getStateBefore(cm, pos.line, precise);var stream=new StringStream(line.text, cm.options.tabSize), tokens;if(asArray)tokens = [];while((asArray || stream.pos < pos.ch) && !stream.eol()) {stream.start = stream.pos;style = readToken(mode, stream, state);if(asArray)tokens.push(getObj(true));}return asArray?tokens:getObj();}function runMode(cm, text, mode, state, f, lineClasses, forceToEnd){var flattenSpans=mode.flattenSpans;if(flattenSpans == null)flattenSpans = cm.options.flattenSpans;var curStart=0, curStyle=null;var stream=new StringStream(text, cm.options.tabSize), style;var inner=cm.options.addModeClass && [null];if(text == "")extractLineClasses(callBlankLine(mode, state), lineClasses);while(!stream.eol()) {if(stream.pos > cm.options.maxHighlightLength){flattenSpans = false;if(forceToEnd)processLine(cm, text, state, stream.pos);stream.pos = text.length;style = null;}else {style = extractLineClasses(readToken(mode, stream, state, inner), lineClasses);}if(inner){var mName=inner[0].name;if(mName)style = "m-" + (style?mName + " " + style:mName);}if(!flattenSpans || curStyle != style){while(curStart < stream.start) {curStart = Math.min(stream.start, curStart + 50000);f(curStart, curStyle);}curStyle = style;}stream.start = stream.pos;}while(curStart < stream.pos) {var pos=Math.min(stream.pos, curStart + 50000);f(pos, curStyle);curStart = pos;}}function highlightLine(cm, line, state, forceToEnd){var st=[cm.state.modeGen], lineClasses={};runMode(cm, line.text, cm.doc.mode, state, function(end, style){st.push(end, style);}, lineClasses, forceToEnd);for(var o=0; o < cm.state.overlays.length; ++o) {var overlay=cm.state.overlays[o], i=1, at=0;runMode(cm, line.text, overlay.mode, true, function(end, style){var start=i;while(at < end) {var i_end=st[i];if(i_end > end)st.splice(i, 1, end, st[i + 1], i_end);i += 2;at = Math.min(end, i_end);}if(!style)return;if(overlay.opaque){st.splice(start, i - start, end, "cm-overlay " + style);i = start + 2;}else {for(; start < i; start += 2) {var cur=st[start + 1];st[start + 1] = (cur?cur + " ":"") + "cm-overlay " + style;}}}, lineClasses);}return {styles:st, classes:lineClasses.bgClass || lineClasses.textClass?lineClasses:null};}function getLineStyles(cm, line, updateFrontier){if(!line.styles || line.styles[0] != cm.state.modeGen){var result=highlightLine(cm, line, line.stateAfter = getStateBefore(cm, lineNo(line)));line.styles = result.styles;if(result.classes)line.styleClasses = result.classes;else if(line.styleClasses)line.styleClasses = null;if(updateFrontier === cm.doc.frontier)cm.doc.frontier++;}return line.styles;}function processLine(cm, text, state, startAt){var mode=cm.doc.mode;var stream=new StringStream(text, cm.options.tabSize);stream.start = stream.pos = startAt || 0;if(text == "")callBlankLine(mode, state);while(!stream.eol() && stream.pos <= cm.options.maxHighlightLength) {readToken(mode, stream, state);stream.start = stream.pos;}}var styleToClassCache={}, styleToClassCacheWithMode={};function interpretTokenStyle(style, options){if(!style || /^\s*$/.test(style)){return null;}var cache=options.addModeClass?styleToClassCacheWithMode:styleToClassCache;return cache[style] || (cache[style] = style.replace(/\S+/g, "cm-$&"));}function buildLineContent(cm, lineView){var content=elt("span", null, null, webkit?"padding-right: .1px":null);var builder={pre:elt("pre", [content]), content:content, col:0, pos:0, cm:cm, splitSpaces:(ie || webkit) && cm.getOption("lineWrapping")};lineView.measure = {};for(var i=0; i <= (lineView.rest?lineView.rest.length:0); i++) {var line=i?lineView.rest[i - 1]:lineView.line, order;builder.pos = 0;builder.addToken = buildToken;if(hasBadBidiRects(cm.display.measure) && (order = getOrder(line)))builder.addToken = buildTokenBadBidi(builder.addToken, order);builder.map = [];var allowFrontierUpdate=lineView != cm.display.externalMeasured && lineNo(line);insertLineContent(line, builder, getLineStyles(cm, line, allowFrontierUpdate));if(line.styleClasses){if(line.styleClasses.bgClass)builder.bgClass = joinClasses(line.styleClasses.bgClass, builder.bgClass || "");if(line.styleClasses.textClass)builder.textClass = joinClasses(line.styleClasses.textClass, builder.textClass || "");}if(builder.map.length == 0)builder.map.push(0, 0, builder.content.appendChild(zeroWidthElement(cm.display.measure)));if(i == 0){lineView.measure.map = builder.map;lineView.measure.cache = {};}else {(lineView.measure.maps || (lineView.measure.maps = [])).push(builder.map);(lineView.measure.caches || (lineView.measure.caches = [])).push({});}}if(webkit && /\bcm-tab\b/.test(builder.content.lastChild.className))builder.content.className = "cm-tab-wrap-hack";signal(cm, "renderLine", cm, lineView.line, builder.pre);if(builder.pre.className)builder.textClass = joinClasses(builder.pre.className, builder.textClass || "");return builder;}function defaultSpecialCharPlaceholder(ch){var token=elt("span", "•", "cm-invalidchar");token.title = "\\u" + ch.charCodeAt(0).toString(16);token.setAttribute("aria-label", token.title);return token;}function buildToken(builder, text, style, startStyle, endStyle, title, css){if(!text){return;}var displayText=builder.splitSpaces?text.replace(/ {3,}/g, splitSpaces):text;var special=builder.cm.state.specialChars, mustWrap=false;if(!special.test(text)){builder.col += text.length;var content=document.createTextNode(displayText);builder.map.push(builder.pos, builder.pos + text.length, content);if(ie && ie_version < 9)mustWrap = true;builder.pos += text.length;}else {var content=document.createDocumentFragment(), pos=0;while(true) {special.lastIndex = pos;var m=special.exec(text);var skipped=m?m.index - pos:text.length - pos;if(skipped){var txt=document.createTextNode(displayText.slice(pos, pos + skipped));if(ie && ie_version < 9)content.appendChild(elt("span", [txt]));else content.appendChild(txt);builder.map.push(builder.pos, builder.pos + skipped, txt);builder.col += skipped;builder.pos += skipped;}if(!m)break;pos += skipped + 1;if(m[0] == "\t"){var tabSize=builder.cm.options.tabSize, tabWidth=tabSize - builder.col % tabSize;var txt=content.appendChild(elt("span", spaceStr(tabWidth), "cm-tab"));txt.setAttribute("role", "presentation");txt.setAttribute("cm-text", "\t");builder.col += tabWidth;}else {var txt=builder.cm.options.specialCharPlaceholder(m[0]);txt.setAttribute("cm-text", m[0]);if(ie && ie_version < 9)content.appendChild(elt("span", [txt]));else content.appendChild(txt);builder.col += 1;}builder.map.push(builder.pos, builder.pos + 1, txt);builder.pos++;}}if(style || startStyle || endStyle || mustWrap || css){var fullStyle=style || "";if(startStyle)fullStyle += startStyle;if(endStyle)fullStyle += endStyle;var token=elt("span", [content], fullStyle, css);if(title)token.title = title;return builder.content.appendChild(token);}builder.content.appendChild(content);}function splitSpaces(old){var out=" ";for(var i=0; i < old.length - 2; ++i) out += i % 2?" ":" ";out += " ";return out;}function buildTokenBadBidi(inner, order){return function(builder, text, style, startStyle, endStyle, title, css){style = style?style + " cm-force-border":"cm-force-border";var start=builder.pos, end=start + text.length;for(;;) {for(var i=0; i < order.length; i++) {var part=order[i];if(part.to > start && part.from <= start)break;}if(part.to >= end)return inner(builder, text, style, startStyle, endStyle, title, css);inner(builder, text.slice(0, part.to - start), style, startStyle, null, title, css);startStyle = null;text = text.slice(part.to - start);start = part.to;}};}function buildCollapsedSpan(builder, size, marker, ignoreWidget){var widget=!ignoreWidget && marker.widgetNode;if(widget)builder.map.push(builder.pos, builder.pos + size, widget);if(!ignoreWidget && builder.cm.display.input.needsContentAttribute){if(!widget)widget = builder.content.appendChild(document.createElement("span"));widget.setAttribute("cm-marker", marker.id);}if(widget){builder.cm.display.input.setUneditable(widget);builder.content.appendChild(widget);}builder.pos += size;}function insertLineContent(line, builder, styles){var spans=line.markedSpans, allText=line.text, at=0;if(!spans){for(var i=1; i < styles.length; i += 2) builder.addToken(builder, allText.slice(at, at = styles[i]), interpretTokenStyle(styles[i + 1], builder.cm.options));return;}var len=allText.length, pos=0, i=1, text="", style, css;var nextChange=0, spanStyle, spanEndStyle, spanStartStyle, title, collapsed;for(;;) {if(nextChange == pos){spanStyle = spanEndStyle = spanStartStyle = title = css = "";collapsed = null;nextChange = Infinity;var foundBookmarks=[];for(var j=0; j < spans.length; ++j) {var sp=spans[j], m=sp.marker;if(m.type == "bookmark" && sp.from == pos && m.widgetNode){foundBookmarks.push(m);}else if(sp.from <= pos && (sp.to == null || sp.to > pos || m.collapsed && sp.to == pos && sp.from == pos)){if(sp.to != null && sp.to != pos && nextChange > sp.to){nextChange = sp.to;spanEndStyle = "";}if(m.className)spanStyle += " " + m.className;if(m.css)css = m.css;if(m.startStyle && sp.from == pos)spanStartStyle += " " + m.startStyle;if(m.endStyle && sp.to == nextChange)spanEndStyle += " " + m.endStyle;if(m.title && !title)title = m.title;if(m.collapsed && (!collapsed || compareCollapsedMarkers(collapsed.marker, m) < 0))collapsed = sp;}else if(sp.from > pos && nextChange > sp.from){nextChange = sp.from;}}if(collapsed && (collapsed.from || 0) == pos){buildCollapsedSpan(builder, (collapsed.to == null?len + 1:collapsed.to) - pos, collapsed.marker, collapsed.from == null);if(collapsed.to == null){return;}if(collapsed.to == pos)collapsed = false;}if(!collapsed && foundBookmarks.length)for(var j=0; j < foundBookmarks.length; ++j) buildCollapsedSpan(builder, 0, foundBookmarks[j]);}if(pos >= len)break;var upto=Math.min(len, nextChange);while(true) {if(text){var end=pos + text.length;if(!collapsed){var tokenText=end > upto?text.slice(0, upto - pos):text;builder.addToken(builder, tokenText, style?style + spanStyle:spanStyle, spanStartStyle, pos + tokenText.length == nextChange?spanEndStyle:"", title, css);}if(end >= upto){text = text.slice(upto - pos);pos = upto;break;}pos = end;spanStartStyle = "";}text = allText.slice(at, at = styles[i++]);style = interpretTokenStyle(styles[i++], builder.cm.options);}}}function isWholeLineUpdate(doc, change){return change.from.ch == 0 && change.to.ch == 0 && lst(change.text) == "" && (!doc.cm || doc.cm.options.wholeLineUpdateBefore);}function updateDoc(doc, change, markedSpans, estimateHeight){function spansFor(n){return markedSpans?markedSpans[n]:null;}function update(line, text, spans){updateLine(line, text, spans, estimateHeight);signalLater(line, "change", line, change);}function linesFor(start, end){for(var i=start, result=[]; i < end; ++i) result.push(new Line(text[i], spansFor(i), estimateHeight));return result;}var from=change.from, to=change.to, text=change.text;var firstLine=getLine(doc, from.line), lastLine=getLine(doc, to.line);var lastText=lst(text), lastSpans=spansFor(text.length - 1), nlines=to.line - from.line;if(change.full){doc.insert(0, linesFor(0, text.length));doc.remove(text.length, doc.size - text.length);}else if(isWholeLineUpdate(doc, change)){var added=linesFor(0, text.length - 1);update(lastLine, lastLine.text, lastSpans);if(nlines)doc.remove(from.line, nlines);if(added.length)doc.insert(from.line, added);}else if(firstLine == lastLine){if(text.length == 1){update(firstLine, firstLine.text.slice(0, from.ch) + lastText + firstLine.text.slice(to.ch), lastSpans);}else {var added=linesFor(1, text.length - 1);added.push(new Line(lastText + firstLine.text.slice(to.ch), lastSpans, estimateHeight));update(firstLine, firstLine.text.slice(0, from.ch) + text[0], spansFor(0));doc.insert(from.line + 1, added);}}else if(text.length == 1){update(firstLine, firstLine.text.slice(0, from.ch) + text[0] + lastLine.text.slice(to.ch), spansFor(0));doc.remove(from.line + 1, nlines);}else {update(firstLine, firstLine.text.slice(0, from.ch) + text[0], spansFor(0));update(lastLine, lastText + lastLine.text.slice(to.ch), lastSpans);var added=linesFor(1, text.length - 1);if(nlines > 1)doc.remove(from.line + 1, nlines - 1);doc.insert(from.line + 1, added);}signalLater(doc, "change", doc, change);}function LeafChunk(lines){this.lines = lines;this.parent = null;for(var i=0, height=0; i < lines.length; ++i) {lines[i].parent = this;height += lines[i].height;}this.height = height;}LeafChunk.prototype = {chunkSize:function chunkSize(){return this.lines.length;}, removeInner:function removeInner(at, n){for(var i=at, e=at + n; i < e; ++i) {var line=this.lines[i];this.height -= line.height;cleanUpLine(line);signalLater(line, "delete");}this.lines.splice(at, n);}, collapse:function collapse(lines){lines.push.apply(lines, this.lines);}, insertInner:function insertInner(at, lines, height){this.height += height;this.lines = this.lines.slice(0, at).concat(lines).concat(this.lines.slice(at));for(var i=0; i < lines.length; ++i) lines[i].parent = this;}, iterN:function iterN(at, n, op){for(var e=at + n; at < e; ++at) if(op(this.lines[at])){return true;}}};function BranchChunk(children){this.children = children;var size=0, height=0;for(var i=0; i < children.length; ++i) {var ch=children[i];size += ch.chunkSize();height += ch.height;ch.parent = this;}this.size = size;this.height = height;this.parent = null;}BranchChunk.prototype = {chunkSize:function chunkSize(){return this.size;}, removeInner:function removeInner(at, n){this.size -= n;for(var i=0; i < this.children.length; ++i) {var child=this.children[i], sz=child.chunkSize();if(at < sz){var rm=Math.min(n, sz - at), oldHeight=child.height;child.removeInner(at, rm);this.height -= oldHeight - child.height;if(sz == rm){this.children.splice(i--, 1);child.parent = null;}if((n -= rm) == 0)break;at = 0;}else at -= sz;}if(this.size - n < 25 && (this.children.length > 1 || !(this.children[0] instanceof LeafChunk))){var lines=[];this.collapse(lines);this.children = [new LeafChunk(lines)];this.children[0].parent = this;}}, collapse:function collapse(lines){for(var i=0; i < this.children.length; ++i) this.children[i].collapse(lines);}, insertInner:function insertInner(at, lines, height){this.size += lines.length;this.height += height;for(var i=0; i < this.children.length; ++i) {var child=this.children[i], sz=child.chunkSize();if(at <= sz){child.insertInner(at, lines, height);if(child.lines && child.lines.length > 50){while(child.lines.length > 50) {var spilled=child.lines.splice(child.lines.length - 25, 25);var newleaf=new LeafChunk(spilled);child.height -= newleaf.height;this.children.splice(i + 1, 0, newleaf);newleaf.parent = this;}this.maybeSpill();}break;}at -= sz;}}, maybeSpill:function maybeSpill(){if(this.children.length <= 10){return;}var me=this;do{var spilled=me.children.splice(me.children.length - 5, 5);var sibling=new BranchChunk(spilled);if(!me.parent){var copy=new BranchChunk(me.children);copy.parent = me;me.children = [copy, sibling];me = copy;}else {me.size -= sibling.size;me.height -= sibling.height;var myIndex=indexOf(me.parent.children, me);me.parent.children.splice(myIndex + 1, 0, sibling);}sibling.parent = me.parent;}while(me.children.length > 10);me.parent.maybeSpill();}, iterN:function iterN(at, n, op){for(var i=0; i < this.children.length; ++i) {var child=this.children[i], sz=child.chunkSize();if(at < sz){var used=Math.min(n, sz - at);if(child.iterN(at, used, op)){return true;}if((n -= used) == 0)break;at = 0;}else at -= sz;}}};var nextDocId=0;var Doc=CodeMirror.Doc = function(text, mode, firstLine){if(!(this instanceof Doc))return new Doc(text, mode, firstLine);if(firstLine == null)firstLine = 0;BranchChunk.call(this, [new LeafChunk([new Line("", null)])]);this.first = firstLine;this.scrollTop = this.scrollLeft = 0;this.cantEdit = false;this.cleanGeneration = 1;this.frontier = firstLine;var start=Pos(firstLine, 0);this.sel = simpleSelection(start);this.history = new History(null);this.id = ++nextDocId;this.modeOption = mode;if(typeof text == "string")text = splitLines(text);updateDoc(this, {from:start, to:start, text:text});setSelection(this, simpleSelection(start), sel_dontScroll);};Doc.prototype = createObj(BranchChunk.prototype, {constructor:Doc, iter:function iter(from, to, op){if(op)this.iterN(from - this.first, to - from, op);else this.iterN(this.first, this.first + this.size, from);}, insert:function insert(at, lines){var height=0;for(var i=0; i < lines.length; ++i) height += lines[i].height;this.insertInner(at - this.first, lines, height);}, remove:function remove(at, n){this.removeInner(at - this.first, n);}, getValue:function getValue(lineSep){var lines=getLines(this, this.first, this.first + this.size);if(lineSep === false){return lines;}return lines.join(lineSep || "\n");}, setValue:docMethodOp(function(code){var top=Pos(this.first, 0), last=this.first + this.size - 1;makeChange(this, {from:top, to:Pos(last, getLine(this, last).text.length), text:splitLines(code), origin:"setValue", full:true}, true);setSelection(this, simpleSelection(top));}), replaceRange:(function(_replaceRange){var _replaceRangeWrapper=function replaceRange(_x, _x2, _x3, _x4){return _replaceRange.apply(this, arguments);};_replaceRangeWrapper.toString = function(){return _replaceRange.toString();};return _replaceRangeWrapper;})(function(code, from, to, origin){from = clipPos(this, from);to = to?clipPos(this, to):from;replaceRange(this, code, from, to, origin);}), getRange:function getRange(from, to, lineSep){var lines=getBetween(this, clipPos(this, from), clipPos(this, to));if(lineSep === false){return lines;}return lines.join(lineSep || "\n");}, getLine:function getLine(line){var l=this.getLineHandle(line);return l && l.text;}, getLineHandle:function getLineHandle(line){if(isLine(this, line)){return getLine(this, line);}}, getLineNumber:function getLineNumber(line){return lineNo(line);}, getLineHandleVisualStart:function getLineHandleVisualStart(line){if(typeof line == "number")line = getLine(this, line);return visualLine(line);}, lineCount:function lineCount(){return this.size;}, firstLine:function firstLine(){return this.first;}, lastLine:function lastLine(){return this.first + this.size - 1;}, clipPos:(function(_clipPos){var _clipPosWrapper=function clipPos(_x){return _clipPos.apply(this, arguments);};_clipPosWrapper.toString = function(){return _clipPos.toString();};return _clipPosWrapper;})(function(pos){return clipPos(this, pos);}), getCursor:function getCursor(start){var range=this.sel.primary(), pos;if(start == null || start == "head")pos = range.head;else if(start == "anchor")pos = range.anchor;else if(start == "end" || start == "to" || start === false)pos = range.to();else pos = range.from();return pos;}, listSelections:function listSelections(){return this.sel.ranges;}, somethingSelected:function somethingSelected(){return this.sel.somethingSelected();}, setCursor:docMethodOp(function(line, ch, options){setSimpleSelection(this, clipPos(this, typeof line == "number"?Pos(line, ch || 0):line), null, options);}), setSelection:docMethodOp(function(anchor, head, options){setSimpleSelection(this, clipPos(this, anchor), clipPos(this, head || anchor), options);}), extendSelection:docMethodOp(function(head, other, options){extendSelection(this, clipPos(this, head), other && clipPos(this, other), options);}), extendSelections:docMethodOp(function(heads, options){extendSelections(this, clipPosArray(this, heads, options));}), extendSelectionsBy:docMethodOp(function(f, options){extendSelections(this, map(this.sel.ranges, f), options);}), setSelections:docMethodOp(function(ranges, primary, options){if(!ranges.length)return;for(var i=0, out=[]; i < ranges.length; i++) out[i] = new Range(clipPos(this, ranges[i].anchor), clipPos(this, ranges[i].head));if(primary == null)primary = Math.min(ranges.length - 1, this.sel.primIndex);setSelection(this, normalizeSelection(out, primary), options);}), addSelection:docMethodOp(function(anchor, head, options){var ranges=this.sel.ranges.slice(0);ranges.push(new Range(clipPos(this, anchor), clipPos(this, head || anchor)));setSelection(this, normalizeSelection(ranges, ranges.length - 1), options);}), getSelection:function getSelection(lineSep){var ranges=this.sel.ranges, lines;for(var i=0; i < ranges.length; i++) {var sel=getBetween(this, ranges[i].from(), ranges[i].to());lines = lines?lines.concat(sel):sel;}if(lineSep === false){return lines;}else {return lines.join(lineSep || "\n");}}, getSelections:function getSelections(lineSep){var parts=[], ranges=this.sel.ranges;for(var i=0; i < ranges.length; i++) {var sel=getBetween(this, ranges[i].from(), ranges[i].to());if(lineSep !== false)sel = sel.join(lineSep || "\n");parts[i] = sel;}return parts;}, replaceSelection:function replaceSelection(code, collapse, origin){var dup=[];for(var i=0; i < this.sel.ranges.length; i++) dup[i] = code;this.replaceSelections(dup, collapse, origin || "+input");}, replaceSelections:docMethodOp(function(code, collapse, origin){var changes=[], sel=this.sel;for(var i=0; i < sel.ranges.length; i++) {var range=sel.ranges[i];changes[i] = {from:range.from(), to:range.to(), text:splitLines(code[i]), origin:origin};}var newSel=collapse && collapse != "end" && computeReplacedSel(this, changes, collapse);for(var i=changes.length - 1; i >= 0; i--) makeChange(this, changes[i]);if(newSel)setSelectionReplaceHistory(this, newSel);else if(this.cm)ensureCursorVisible(this.cm);}), undo:docMethodOp(function(){makeChangeFromHistory(this, "undo");}), redo:docMethodOp(function(){makeChangeFromHistory(this, "redo");}), undoSelection:docMethodOp(function(){makeChangeFromHistory(this, "undo", true);}), redoSelection:docMethodOp(function(){makeChangeFromHistory(this, "redo", true);}), setExtending:function setExtending(val){this.extend = val;}, getExtending:function getExtending(){return this.extend;}, historySize:function historySize(){var hist=this.history, done=0, undone=0;for(var i=0; i < hist.done.length; i++) if(!hist.done[i].ranges)++done;for(var i=0; i < hist.undone.length; i++) if(!hist.undone[i].ranges)++undone;return {undo:done, redo:undone};}, clearHistory:function clearHistory(){this.history = new History(this.history.maxGeneration);}, markClean:function markClean(){this.cleanGeneration = this.changeGeneration(true);}, changeGeneration:function changeGeneration(forceSplit){if(forceSplit)this.history.lastOp = this.history.lastSelOp = this.history.lastOrigin = null;return this.history.generation;}, isClean:function isClean(gen){return this.history.generation == (gen || this.cleanGeneration);}, getHistory:function getHistory(){return {done:copyHistoryArray(this.history.done), undone:copyHistoryArray(this.history.undone)};}, setHistory:function setHistory(histData){var hist=this.history = new History(this.history.maxGeneration);hist.done = copyHistoryArray(histData.done.slice(0), null, true);hist.undone = copyHistoryArray(histData.undone.slice(0), null, true);}, addLineClass:docMethodOp(function(handle, where, cls){return changeLine(this, handle, where == "gutter"?"gutter":"class", function(line){var prop=where == "text"?"textClass":where == "background"?"bgClass":where == "gutter"?"gutterClass":"wrapClass";if(!line[prop])line[prop] = cls;else if(classTest(cls).test(line[prop]))return false;else line[prop] += " " + cls;return true;});}), removeLineClass:docMethodOp(function(handle, where, cls){return changeLine(this, handle, where == "gutter"?"gutter":"class", function(line){var prop=where == "text"?"textClass":where == "background"?"bgClass":where == "gutter"?"gutterClass":"wrapClass";var cur=line[prop];if(!cur)return false;else if(cls == null)line[prop] = null;else {var found=cur.match(classTest(cls));if(!found)return false;var end=found.index + found[0].length;line[prop] = cur.slice(0, found.index) + (!found.index || end == cur.length?"":" ") + cur.slice(end) || null;}return true;});}), addLineWidget:docMethodOp(function(handle, node, options){return addLineWidget(this, handle, node, options);}), removeLineWidget:function removeLineWidget(widget){widget.clear();}, markText:(function(_markText){var _markTextWrapper=function markText(_x, _x2, _x3){return _markText.apply(this, arguments);};_markTextWrapper.toString = function(){return _markText.toString();};return _markTextWrapper;})(function(from, to, options){return markText(this, clipPos(this, from), clipPos(this, to), options, "range");}), setBookmark:function setBookmark(pos, options){var realOpts={replacedWith:options && (options.nodeType == null?options.widget:options), insertLeft:options && options.insertLeft, clearWhenEmpty:false, shared:options && options.shared, handleMouseEvents:options && options.handleMouseEvents};pos = clipPos(this, pos);return markText(this, pos, pos, realOpts, "bookmark");}, findMarksAt:function findMarksAt(pos){pos = clipPos(this, pos);var markers=[], spans=getLine(this, pos.line).markedSpans;if(spans)for(var i=0; i < spans.length; ++i) {var span=spans[i];if((span.from == null || span.from <= pos.ch) && (span.to == null || span.to >= pos.ch))markers.push(span.marker.parent || span.marker);}return markers;}, findMarks:function findMarks(from, to, filter){from = clipPos(this, from);to = clipPos(this, to);var found=[], lineNo=from.line;this.iter(from.line, to.line + 1, function(line){var spans=line.markedSpans;if(spans)for(var i=0; i < spans.length; i++) {var span=spans[i];if(!(lineNo == from.line && from.ch > span.to || span.from == null && lineNo != from.line || lineNo == to.line && span.from > to.ch) && (!filter || filter(span.marker)))found.push(span.marker.parent || span.marker);}++lineNo;});return found;}, getAllMarks:function getAllMarks(){var markers=[];this.iter(function(line){var sps=line.markedSpans;if(sps)for(var i=0; i < sps.length; ++i) if(sps[i].from != null)markers.push(sps[i].marker);});return markers;}, posFromIndex:function posFromIndex(off){var ch, lineNo=this.first;this.iter(function(line){var sz=line.text.length + 1;if(sz > off){ch = off;return true;}off -= sz;++lineNo;});return clipPos(this, Pos(lineNo, ch));}, indexFromPos:function indexFromPos(coords){coords = clipPos(this, coords);var index=coords.ch;if(coords.line < this.first || coords.ch < 0){return 0;}this.iter(this.first, coords.line, function(line){index += line.text.length + 1;});return index;}, copy:function copy(copyHistory){var doc=new Doc(getLines(this, this.first, this.first + this.size), this.modeOption, this.first);doc.scrollTop = this.scrollTop;doc.scrollLeft = this.scrollLeft;doc.sel = this.sel;doc.extend = false;if(copyHistory){doc.history.undoDepth = this.history.undoDepth;doc.setHistory(this.getHistory());}return doc;}, linkedDoc:function linkedDoc(options){if(!options)options = {};var from=this.first, to=this.first + this.size;if(options.from != null && options.from > from)from = options.from;if(options.to != null && options.to < to)to = options.to;var copy=new Doc(getLines(this, from, to), options.mode || this.modeOption, from);if(options.sharedHist)copy.history = this.history;(this.linked || (this.linked = [])).push({doc:copy, sharedHist:options.sharedHist});copy.linked = [{doc:this, isParent:true, sharedHist:options.sharedHist}];copySharedMarkers(copy, findSharedMarkers(this));return copy;}, unlinkDoc:function unlinkDoc(other){if(other instanceof CodeMirror)other = other.doc;if(this.linked)for(var i=0; i < this.linked.length; ++i) {var link=this.linked[i];if(link.doc != other)continue;this.linked.splice(i, 1);other.unlinkDoc(this);detachSharedMarkers(findSharedMarkers(this));break;}if(other.history == this.history){var splitIds=[other.id];linkedDocs(other, function(doc){splitIds.push(doc.id);}, true);other.history = new History(null);other.history.done = copyHistoryArray(this.history.done, splitIds);other.history.undone = copyHistoryArray(this.history.undone, splitIds);}}, iterLinkedDocs:function iterLinkedDocs(f){linkedDocs(this, f);}, getMode:function getMode(){return this.mode;}, getEditor:function getEditor(){return this.cm;}});Doc.prototype.eachLine = Doc.prototype.iter;var dontDelegate="iter insert remove copy getEditor".split(" ");for(var prop in Doc.prototype) if(Doc.prototype.hasOwnProperty(prop) && indexOf(dontDelegate, prop) < 0)CodeMirror.prototype[prop] = (function(method){return function(){return method.apply(this.doc, arguments);};})(Doc.prototype[prop]);eventMixin(Doc);function linkedDocs(doc, f, sharedHistOnly){function propagate(doc, skip, sharedHist){if(doc.linked)for(var i=0; i < doc.linked.length; ++i) {var rel=doc.linked[i];if(rel.doc == skip)continue;var shared=sharedHist && rel.sharedHist;if(sharedHistOnly && !shared)continue;f(rel.doc, shared);propagate(rel.doc, doc, shared);}}propagate(doc, null, true);}function attachDoc(cm, doc){if(doc.cm)throw new Error("This document is already in use.");cm.doc = doc;doc.cm = cm;estimateLineHeights(cm);loadMode(cm);if(!cm.options.lineWrapping)findMaxLine(cm);cm.options.mode = doc.modeOption;regChange(cm);}function getLine(doc, n){n -= doc.first;if(n < 0 || n >= doc.size)throw new Error("There is no line " + (n + doc.first) + " in the document.");for(var chunk=doc; !chunk.lines;) {for(var i=0;; ++i) {var child=chunk.children[i], sz=child.chunkSize();if(n < sz){chunk = child;break;}n -= sz;}}return chunk.lines[n];}function getBetween(doc, start, end){var out=[], n=start.line;doc.iter(start.line, end.line + 1, function(line){var text=line.text;if(n == end.line)text = text.slice(0, end.ch);if(n == start.line)text = text.slice(start.ch);out.push(text);++n;});return out;}function getLines(doc, from, to){var out=[];doc.iter(from, to, function(line){out.push(line.text);});return out;}function updateLineHeight(line, height){var diff=height - line.height;if(diff)for(var n=line; n; n = n.parent) n.height += diff;}function lineNo(line){if(line.parent == null){return null;}var cur=line.parent, no=indexOf(cur.lines, line);for(var chunk=cur.parent; chunk; cur = chunk, chunk = chunk.parent) {for(var i=0;; ++i) {if(chunk.children[i] == cur)break;no += chunk.children[i].chunkSize();}}return no + cur.first;}function lineAtHeight(chunk, h){var n=chunk.first;outer: do{for(var i=0; i < chunk.children.length; ++i) {var child=chunk.children[i], ch=child.height;if(h < ch){chunk = child;continue outer;}h -= ch;n += child.chunkSize();}return n;}while(!chunk.lines);for(var i=0; i < chunk.lines.length; ++i) {var line=chunk.lines[i], lh=line.height;if(h < lh)break;h -= lh;}return n + i;}function heightAtLine(lineObj){lineObj = visualLine(lineObj);var h=0, chunk=lineObj.parent;for(var i=0; i < chunk.lines.length; ++i) {var line=chunk.lines[i];if(line == lineObj)break;else h += line.height;}for(var p=chunk.parent; p; chunk = p, p = chunk.parent) {for(var i=0; i < p.children.length; ++i) {var cur=p.children[i];if(cur == chunk)break;else h += cur.height;}}return h;}function getOrder(line){var order=line.order;if(order == null)order = line.order = bidiOrdering(line.text);return order;}function History(startGen){this.done = [];this.undone = [];this.undoDepth = Infinity;this.lastModTime = this.lastSelTime = 0;this.lastOp = this.lastSelOp = null;this.lastOrigin = this.lastSelOrigin = null;this.generation = this.maxGeneration = startGen || 1;}function historyChangeFromChange(doc, change){var histChange={from:copyPos(change.from), to:changeEnd(change), text:getBetween(doc, change.from, change.to)};attachLocalSpans(doc, histChange, change.from.line, change.to.line + 1);linkedDocs(doc, function(doc){attachLocalSpans(doc, histChange, change.from.line, change.to.line + 1);}, true);return histChange;}function clearSelectionEvents(array){while(array.length) {var last=lst(array);if(last.ranges)array.pop();else break;}}function lastChangeEvent(hist, force){if(force){clearSelectionEvents(hist.done);return lst(hist.done);}else if(hist.done.length && !lst(hist.done).ranges){return lst(hist.done);}else if(hist.done.length > 1 && !hist.done[hist.done.length - 2].ranges){hist.done.pop();return lst(hist.done);}}function addChangeToHistory(doc, change, selAfter, opId){var hist=doc.history;hist.undone.length = 0;var time=+new Date(), cur;if((hist.lastOp == opId || hist.lastOrigin == change.origin && change.origin && (change.origin.charAt(0) == "+" && doc.cm && hist.lastModTime > time - doc.cm.options.historyEventDelay || change.origin.charAt(0) == "*")) && (cur = lastChangeEvent(hist, hist.lastOp == opId))){var last=lst(cur.changes);if(cmp(change.from, change.to) == 0 && cmp(change.from, last.to) == 0){last.to = changeEnd(change);}else {cur.changes.push(historyChangeFromChange(doc, change));}}else {var before=lst(hist.done);if(!before || !before.ranges)pushSelectionToHistory(doc.sel, hist.done);cur = {changes:[historyChangeFromChange(doc, change)], generation:hist.generation};hist.done.push(cur);while(hist.done.length > hist.undoDepth) {hist.done.shift();if(!hist.done[0].ranges)hist.done.shift();}}hist.done.push(selAfter);hist.generation = ++hist.maxGeneration;hist.lastModTime = hist.lastSelTime = time;hist.lastOp = hist.lastSelOp = opId;hist.lastOrigin = hist.lastSelOrigin = change.origin;if(!last)signal(doc, "historyAdded");}function selectionEventCanBeMerged(doc, origin, prev, sel){var ch=origin.charAt(0);return ch == "*" || ch == "+" && prev.ranges.length == sel.ranges.length && prev.somethingSelected() == sel.somethingSelected() && new Date() - doc.history.lastSelTime <= (doc.cm?doc.cm.options.historyEventDelay:500);}function addSelectionToHistory(doc, sel, opId, options){var hist=doc.history, origin=options && options.origin;if(opId == hist.lastSelOp || origin && hist.lastSelOrigin == origin && (hist.lastModTime == hist.lastSelTime && hist.lastOrigin == origin || selectionEventCanBeMerged(doc, origin, lst(hist.done), sel)))hist.done[hist.done.length - 1] = sel;else pushSelectionToHistory(sel, hist.done);hist.lastSelTime = +new Date();hist.lastSelOrigin = origin;hist.lastSelOp = opId;if(options && options.clearRedo !== false)clearSelectionEvents(hist.undone);}function pushSelectionToHistory(sel, dest){var top=lst(dest);if(!(top && top.ranges && top.equals(sel)))dest.push(sel);}function attachLocalSpans(doc, change, from, to){var existing=change["spans_" + doc.id], n=0;doc.iter(Math.max(doc.first, from), Math.min(doc.first + doc.size, to), function(line){if(line.markedSpans)(existing || (existing = change["spans_" + doc.id] = {}))[n] = line.markedSpans;++n;});}function removeClearedSpans(spans){if(!spans){return null;}for(var i=0, out; i < spans.length; ++i) {if(spans[i].marker.explicitlyCleared){if(!out)out = spans.slice(0, i);}else if(out)out.push(spans[i]);}return !out?spans:out.length?out:null;}function getOldSpans(doc, change){var found=change["spans_" + doc.id];if(!found){return null;}for(var i=0, nw=[]; i < change.text.length; ++i) nw.push(removeClearedSpans(found[i]));return nw;}function copyHistoryArray(events, newGroup, instantiateSel){for(var i=0, copy=[]; i < events.length; ++i) {var event=events[i];if(event.ranges){copy.push(instantiateSel?Selection.prototype.deepCopy.call(event):event);continue;}var changes=event.changes, newChanges=[];copy.push({changes:newChanges});for(var j=0; j < changes.length; ++j) {var change=changes[j], m;newChanges.push({from:change.from, to:change.to, text:change.text});if(newGroup)for(var prop in change) if(m = prop.match(/^spans_(\d+)$/)){if(indexOf(newGroup, Number(m[1])) > -1){lst(newChanges)[prop] = change[prop];delete change[prop];}}}}return copy;}function rebaseHistSelSingle(pos, from, to, diff){if(to < pos.line){pos.line += diff;}else if(from < pos.line){pos.line = from;pos.ch = 0;}}function rebaseHistArray(array, from, to, diff){for(var i=0; i < array.length; ++i) {var sub=array[i], ok=true;if(sub.ranges){if(!sub.copied){sub = array[i] = sub.deepCopy();sub.copied = true;}for(var j=0; j < sub.ranges.length; j++) {rebaseHistSelSingle(sub.ranges[j].anchor, from, to, diff);rebaseHistSelSingle(sub.ranges[j].head, from, to, diff);}continue;}for(var j=0; j < sub.changes.length; ++j) {var cur=sub.changes[j];if(to < cur.from.line){cur.from = Pos(cur.from.line + diff, cur.from.ch);cur.to = Pos(cur.to.line + diff, cur.to.ch);}else if(from <= cur.to.line){ok = false;break;}}if(!ok){array.splice(0, i + 1);i = 0;}}}function rebaseHist(hist, change){var from=change.from.line, to=change.to.line, diff=change.text.length - (to - from) - 1;rebaseHistArray(hist.done, from, to, diff);rebaseHistArray(hist.undone, from, to, diff);}var e_preventDefault=CodeMirror.e_preventDefault = function(e){if(e.preventDefault)e.preventDefault();else e.returnValue = false;};var e_stopPropagation=CodeMirror.e_stopPropagation = function(e){if(e.stopPropagation)e.stopPropagation();else e.cancelBubble = true;};function e_defaultPrevented(e){return e.defaultPrevented != null?e.defaultPrevented:e.returnValue == false;}var e_stop=CodeMirror.e_stop = function(e){e_preventDefault(e);e_stopPropagation(e);};function e_target(e){return e.target || e.srcElement;}function e_button(e){var b=e.which;if(b == null){if(e.button & 1)b = 1;else if(e.button & 2)b = 3;else if(e.button & 4)b = 2;}if(mac && e.ctrlKey && b == 1)b = 3;return b;}var on=CodeMirror.on = function(emitter, type, f){if(emitter.addEventListener)emitter.addEventListener(type, f, false);else if(emitter.attachEvent)emitter.attachEvent("on" + type, f);else {var map=emitter._handlers || (emitter._handlers = {});var arr=map[type] || (map[type] = []);arr.push(f);}};var off=CodeMirror.off = function(emitter, type, f){if(emitter.removeEventListener)emitter.removeEventListener(type, f, false);else if(emitter.detachEvent)emitter.detachEvent("on" + type, f);else {var arr=emitter._handlers && emitter._handlers[type];if(!arr)return;for(var i=0; i < arr.length; ++i) if(arr[i] == f){arr.splice(i, 1);break;}}};var signal=CodeMirror.signal = function(emitter, type){var arr=emitter._handlers && emitter._handlers[type];if(!arr)return;var args=Array.prototype.slice.call(arguments, 2);for(var i=0; i < arr.length; ++i) arr[i].apply(null, args);};var orphanDelayedCallbacks=null;function signalLater(emitter, type){var arr=emitter._handlers && emitter._handlers[type];if(!arr){return;}var args=Array.prototype.slice.call(arguments, 2), list;if(operationGroup){list = operationGroup.delayedCallbacks;}else if(orphanDelayedCallbacks){list = orphanDelayedCallbacks;}else {list = orphanDelayedCallbacks = [];setTimeout(fireOrphanDelayed, 0);}function bnd(f){return function(){f.apply(null, args);};};for(var i=0; i < arr.length; ++i) list.push(bnd(arr[i]));}function fireOrphanDelayed(){var delayed=orphanDelayedCallbacks;orphanDelayedCallbacks = null;for(var i=0; i < delayed.length; ++i) delayed[i]();}function signalDOMEvent(cm, e, override){if(typeof e == "string")e = {type:e, preventDefault:function preventDefault(){this.defaultPrevented = true;}};signal(cm, override || e.type, cm, e);return e_defaultPrevented(e) || e.codemirrorIgnore;}function signalCursorActivity(cm){var arr=cm._handlers && cm._handlers.cursorActivity;if(!arr){return;}var set=cm.curOp.cursorActivityHandlers || (cm.curOp.cursorActivityHandlers = []);for(var i=0; i < arr.length; ++i) if(indexOf(set, arr[i]) == -1)set.push(arr[i]);}function hasHandler(emitter, type){var arr=emitter._handlers && emitter._handlers[type];return arr && arr.length > 0;}function eventMixin(ctor){ctor.prototype.on = function(type, f){on(this, type, f);};ctor.prototype.off = function(type, f){off(this, type, f);};}var scrollerGap=30;var Pass=CodeMirror.Pass = {toString:function toString(){return "CodeMirror.Pass";}};var sel_dontScroll={scroll:false}, sel_mouse={origin:"*mouse"}, sel_move={origin:"+move"};function Delayed(){this.id = null;}Delayed.prototype.set = function(ms, f){clearTimeout(this.id);this.id = setTimeout(f, ms);};var countColumn=CodeMirror.countColumn = function(string, end, tabSize, startIndex, startValue){if(end == null){end = string.search(/[^\s\u00a0]/);if(end == -1)end = string.length;}for(var i=startIndex || 0, n=startValue || 0;;) {var nextTab=string.indexOf("\t", i);if(nextTab < 0 || nextTab >= end)return n + (end - i);n += nextTab - i;n += tabSize - n % tabSize;i = nextTab + 1;}};function findColumn(string, goal, tabSize){for(var pos=0, col=0;;) {var nextTab=string.indexOf("\t", pos);if(nextTab == -1)nextTab = string.length;var skipped=nextTab - pos;if(nextTab == string.length || col + skipped >= goal){return pos + Math.min(skipped, goal - col);}col += nextTab - pos;col += tabSize - col % tabSize;pos = nextTab + 1;if(col >= goal){return pos;}}}var spaceStrs=[""];function spaceStr(n){while(spaceStrs.length <= n) spaceStrs.push(lst(spaceStrs) + " ");return spaceStrs[n];}function lst(arr){return arr[arr.length - 1];}var selectInput=function selectInput(node){node.select();};if(ios)selectInput = function(node){node.selectionStart = 0;node.selectionEnd = node.value.length;};else if(ie)selectInput = function(node){try{node.select();}catch(_e) {}};function indexOf(array, elt){for(var i=0; i < array.length; ++i) if(array[i] == elt){return i;}return -1;}function map(array, f){var out=[];for(var i=0; i < array.length; i++) out[i] = f(array[i], i);return out;}function nothing(){}function createObj(base, props){var inst;if(Object.create){inst = Object.create(base);}else {nothing.prototype = base;inst = new nothing();}if(props)copyObj(props, inst);return inst;};function copyObj(obj, target, overwrite){if(!target)target = {};for(var prop in obj) if(obj.hasOwnProperty(prop) && (overwrite !== false || !target.hasOwnProperty(prop)))target[prop] = obj[prop];return target;}function bind(f){var args=Array.prototype.slice.call(arguments, 1);return function(){return f.apply(null, args);};}var nonASCIISingleCaseWordChar=/[\u00df\u0587\u0590-\u05f4\u0600-\u06ff\u3040-\u309f\u30a0-\u30ff\u3400-\u4db5\u4e00-\u9fcc\uac00-\ud7af]/;var isWordCharBasic=CodeMirror.isWordChar = function(ch){return /\w/.test(ch) || ch > "" && (ch.toUpperCase() != ch.toLowerCase() || nonASCIISingleCaseWordChar.test(ch));};function isWordChar(ch, helper){if(!helper){return isWordCharBasic(ch);}if(helper.source.indexOf("\\w") > -1 && isWordCharBasic(ch)){return true;}return helper.test(ch);}function isEmpty(obj){for(var n in obj) if(obj.hasOwnProperty(n) && obj[n]){return false;}return true;}var extendingChars=/[\u0300-\u036f\u0483-\u0489\u0591-\u05bd\u05bf\u05c1\u05c2\u05c4\u05c5\u05c7\u0610-\u061a\u064b-\u065e\u0670\u06d6-\u06dc\u06de-\u06e4\u06e7\u06e8\u06ea-\u06ed\u0711\u0730-\u074a\u07a6-\u07b0\u07eb-\u07f3\u0816-\u0819\u081b-\u0823\u0825-\u0827\u0829-\u082d\u0900-\u0902\u093c\u0941-\u0948\u094d\u0951-\u0955\u0962\u0963\u0981\u09bc\u09be\u09c1-\u09c4\u09cd\u09d7\u09e2\u09e3\u0a01\u0a02\u0a3c\u0a41\u0a42\u0a47\u0a48\u0a4b-\u0a4d\u0a51\u0a70\u0a71\u0a75\u0a81\u0a82\u0abc\u0ac1-\u0ac5\u0ac7\u0ac8\u0acd\u0ae2\u0ae3\u0b01\u0b3c\u0b3e\u0b3f\u0b41-\u0b44\u0b4d\u0b56\u0b57\u0b62\u0b63\u0b82\u0bbe\u0bc0\u0bcd\u0bd7\u0c3e-\u0c40\u0c46-\u0c48\u0c4a-\u0c4d\u0c55\u0c56\u0c62\u0c63\u0cbc\u0cbf\u0cc2\u0cc6\u0ccc\u0ccd\u0cd5\u0cd6\u0ce2\u0ce3\u0d3e\u0d41-\u0d44\u0d4d\u0d57\u0d62\u0d63\u0dca\u0dcf\u0dd2-\u0dd4\u0dd6\u0ddf\u0e31\u0e34-\u0e3a\u0e47-\u0e4e\u0eb1\u0eb4-\u0eb9\u0ebb\u0ebc\u0ec8-\u0ecd\u0f18\u0f19\u0f35\u0f37\u0f39\u0f71-\u0f7e\u0f80-\u0f84\u0f86\u0f87\u0f90-\u0f97\u0f99-\u0fbc\u0fc6\u102d-\u1030\u1032-\u1037\u1039\u103a\u103d\u103e\u1058\u1059\u105e-\u1060\u1071-\u1074\u1082\u1085\u1086\u108d\u109d\u135f\u1712-\u1714\u1732-\u1734\u1752\u1753\u1772\u1773\u17b7-\u17bd\u17c6\u17c9-\u17d3\u17dd\u180b-\u180d\u18a9\u1920-\u1922\u1927\u1928\u1932\u1939-\u193b\u1a17\u1a18\u1a56\u1a58-\u1a5e\u1a60\u1a62\u1a65-\u1a6c\u1a73-\u1a7c\u1a7f\u1b00-\u1b03\u1b34\u1b36-\u1b3a\u1b3c\u1b42\u1b6b-\u1b73\u1b80\u1b81\u1ba2-\u1ba5\u1ba8\u1ba9\u1c2c-\u1c33\u1c36\u1c37\u1cd0-\u1cd2\u1cd4-\u1ce0\u1ce2-\u1ce8\u1ced\u1dc0-\u1de6\u1dfd-\u1dff\u200c\u200d\u20d0-\u20f0\u2cef-\u2cf1\u2de0-\u2dff\u302a-\u302f\u3099\u309a\ua66f-\ua672\ua67c\ua67d\ua6f0\ua6f1\ua802\ua806\ua80b\ua825\ua826\ua8c4\ua8e0-\ua8f1\ua926-\ua92d\ua947-\ua951\ua980-\ua982\ua9b3\ua9b6-\ua9b9\ua9bc\uaa29-\uaa2e\uaa31\uaa32\uaa35\uaa36\uaa43\uaa4c\uaab0\uaab2-\uaab4\uaab7\uaab8\uaabe\uaabf\uaac1\uabe5\uabe8\uabed\udc00-\udfff\ufb1e\ufe00-\ufe0f\ufe20-\ufe26\uff9e\uff9f]/;function isExtendingChar(ch){return ch.charCodeAt(0) >= 768 && extendingChars.test(ch);}function elt(tag, content, className, style){var e=document.createElement(tag);if(className)e.className = className;if(style)e.style.cssText = style;if(typeof content == "string")e.appendChild(document.createTextNode(content));else if(content)for(var i=0; i < content.length; ++i) e.appendChild(content[i]);return e;}var range;if(document.createRange)range = function(node, start, end, endNode){var r=document.createRange();r.setEnd(endNode || node, end);r.setStart(node, start);return r;};else range = function(node, start, end){var r=document.body.createTextRange();try{r.moveToElementText(node.parentNode);}catch(e) {return r;}r.collapse(true);r.moveEnd("character", end);r.moveStart("character", start);return r;};function removeChildren(e){for(var count=e.childNodes.length; count > 0; --count) e.removeChild(e.firstChild);return e;}function removeChildrenAndAdd(parent, e){return removeChildren(parent).appendChild(e);}var contains=CodeMirror.contains = function(parent, child){if(child.nodeType == 3)child = child.parentNode;if(parent.contains)return parent.contains(child);do{if(child.nodeType == 11)child = child.host;if(child == parent)return true;}while(child = child.parentNode);};function activeElt(){return document.activeElement;}if(ie && ie_version < 11)activeElt = function(){try{return document.activeElement;}catch(e) {return document.body;}};function classTest(cls){return new RegExp("(^|\\s)" + cls + "(?:$|\\s)\\s*");}var rmClass=CodeMirror.rmClass = function(node, cls){var current=node.className;var match=classTest(cls).exec(current);if(match){var after=current.slice(match.index + match[0].length);node.className = current.slice(0, match.index) + (after?match[1] + after:"");}};var addClass=CodeMirror.addClass = function(node, cls){var current=node.className;if(!classTest(cls).test(current))node.className += (current?" ":"") + cls;};function joinClasses(a, b){var as=a.split(" ");for(var i=0; i < as.length; i++) if(as[i] && !classTest(as[i]).test(b))b += " " + as[i];return b;}function forEachCodeMirror(f){if(!document.body.getElementsByClassName){return;}var byClass=document.body.getElementsByClassName("CodeMirror");for(var i=0; i < byClass.length; i++) {var cm=byClass[i].CodeMirror;if(cm)f(cm);}}var globalsRegistered=false;function ensureGlobalHandlers(){if(globalsRegistered){return;}registerGlobalHandlers();globalsRegistered = true;}function registerGlobalHandlers(){var resizeTimer;on(window, "resize", function(){if(resizeTimer == null)resizeTimer = setTimeout(function(){resizeTimer = null;forEachCodeMirror(onResize);}, 100);});on(window, "blur", function(){forEachCodeMirror(onBlur);});}var dragAndDrop=(function(){if(ie && ie_version < 9)return false;var div=elt("div");return "draggable" in div || "dragDrop" in div;})();var zwspSupported;function zeroWidthElement(measure){if(zwspSupported == null){var test=elt("span", "​");removeChildrenAndAdd(measure, elt("span", [test, document.createTextNode("x")]));if(measure.firstChild.offsetHeight != 0)zwspSupported = test.offsetWidth <= 1 && test.offsetHeight > 2 && !(ie && ie_version < 8);}var node=zwspSupported?elt("span", "​"):elt("span", " ", null, "display: inline-block; width: 1px; margin-right: -1px");node.setAttribute("cm-text", "");return node;}var badBidiRects;function hasBadBidiRects(measure){if(badBidiRects != null){return badBidiRects;}var txt=removeChildrenAndAdd(measure, document.createTextNode("AخA"));var r0=range(txt, 0, 1).getBoundingClientRect();if(!r0 || r0.left == r0.right){return false;}var r1=range(txt, 1, 2).getBoundingClientRect();return badBidiRects = r1.right - r0.right < 3;}var splitLines=CodeMirror.splitLines = "\n\nb".split(/\n/).length != 3?function(string){var pos=0, result=[], l=string.length;while(pos <= l) {var nl=string.indexOf("\n", pos);if(nl == -1)nl = string.length;var line=string.slice(pos, string.charAt(nl - 1) == "\r"?nl - 1:nl);var rt=line.indexOf("\r");if(rt != -1){result.push(line.slice(0, rt));pos += rt + 1;}else {result.push(line);pos = nl + 1;}}return result;}:function(string){return string.split(/\r\n?|\n/);};var hasSelection=window.getSelection?function(te){try{return te.selectionStart != te.selectionEnd;}catch(e) {return false;}}:function(te){try{var range=te.ownerDocument.selection.createRange();}catch(e) {}if(!range || range.parentElement() != te)return false;return range.compareEndPoints("StartToEnd", range) != 0;};var hasCopyEvent=(function(){var e=elt("div");if("oncopy" in e)return true;e.setAttribute("oncopy", "return;");return typeof e.oncopy == "function";})();var badZoomedRects=null;function hasBadZoomedRects(measure){if(badZoomedRects != null){return badZoomedRects;}var node=removeChildrenAndAdd(measure, elt("span", "x"));var normal=node.getBoundingClientRect();var fromRange=range(node, 0, 1).getBoundingClientRect();return badZoomedRects = Math.abs(normal.left - fromRange.left) > 1;}var keyNames={3:"Enter", 8:"Backspace", 9:"Tab", 13:"Enter", 16:"Shift", 17:"Ctrl", 18:"Alt", 19:"Pause", 20:"CapsLock", 27:"Esc", 32:"Space", 33:"PageUp", 34:"PageDown", 35:"End", 36:"Home", 37:"Left", 38:"Up", 39:"Right", 40:"Down", 44:"PrintScrn", 45:"Insert", 46:"Delete", 59:";", 61:"=", 91:"Mod", 92:"Mod", 93:"Mod", 107:"=", 109:"-", 127:"Delete", 173:"-", 186:";", 187:"=", 188:",", 189:"-", 190:".", 191:"/", 192:"`", 219:"[", 220:"\\", 221:"]", 222:"'", 63232:"Up", 63233:"Down", 63234:"Left", 63235:"Right", 63272:"Delete", 63273:"Home", 63275:"End", 63276:"PageUp", 63277:"PageDown", 63302:"Insert"};CodeMirror.keyNames = keyNames;(function(){for(var i=0; i < 10; i++) keyNames[i + 48] = keyNames[i + 96] = String(i);for(var i=65; i <= 90; i++) keyNames[i] = String.fromCharCode(i);for(var i=1; i <= 12; i++) keyNames[i + 111] = keyNames[i + 63235] = "F" + i;})();function iterateBidiSections(order, from, to, f){if(!order){return f(from, to, "ltr");}var found=false;for(var i=0; i < order.length; ++i) {var part=order[i];if(part.from < to && part.to > from || from == to && part.to == from){f(Math.max(part.from, from), Math.min(part.to, to), part.level == 1?"rtl":"ltr");found = true;}}if(!found)f(from, to, "ltr");}function bidiLeft(part){return part.level % 2?part.to:part.from;}function bidiRight(part){return part.level % 2?part.from:part.to;}function lineLeft(line){var order=getOrder(line);return order?bidiLeft(order[0]):0;}function lineRight(line){var order=getOrder(line);if(!order){return line.text.length;}return bidiRight(lst(order));}function lineStart(cm, lineN){var line=getLine(cm.doc, lineN);var visual=visualLine(line);if(visual != line)lineN = lineNo(visual);var order=getOrder(visual);var ch=!order?0:order[0].level % 2?lineRight(visual):lineLeft(visual);return Pos(lineN, ch);}function lineEnd(cm, lineN){var merged, line=getLine(cm.doc, lineN);while(merged = collapsedSpanAtEnd(line)) {line = merged.find(1, true).line;lineN = null;}var order=getOrder(line);var ch=!order?line.text.length:order[0].level % 2?lineLeft(line):lineRight(line);return Pos(lineN == null?lineNo(line):lineN, ch);}function lineStartSmart(cm, pos){var start=lineStart(cm, pos.line);var line=getLine(cm.doc, start.line);var order=getOrder(line);if(!order || order[0].level == 0){var firstNonWS=Math.max(0, line.text.search(/\S/));var inWS=pos.line == start.line && pos.ch <= firstNonWS && pos.ch;return Pos(start.line, inWS?0:firstNonWS);}return start;}function compareBidiLevel(order, a, b){var linedir=order[0].level;if(a == linedir){return true;}if(b == linedir){return false;}return a < b;}var bidiOther;function getBidiPartAt(order, pos){bidiOther = null;for(var i=0, found; i < order.length; ++i) {var cur=order[i];if(cur.from < pos && cur.to > pos){return i;}if(cur.from == pos || cur.to == pos){if(found == null){found = i;}else if(compareBidiLevel(order, cur.level, order[found].level)){if(cur.from != cur.to)bidiOther = found;return i;}else {if(cur.from != cur.to)bidiOther = i;return found;}}}return found;}function moveInLine(line, pos, dir, byUnit){if(!byUnit){return pos + dir;}dopos += dir;while(pos > 0 && isExtendingChar(line.text.charAt(pos)));return pos;}function moveVisually(line, start, dir, byUnit){var bidi=getOrder(line);if(!bidi){return moveLogically(line, start, dir, byUnit);}var pos=getBidiPartAt(bidi, start), part=bidi[pos];var target=moveInLine(line, start, part.level % 2?-dir:dir, byUnit);for(;;) {if(target > part.from && target < part.to){return target;}if(target == part.from || target == part.to){if(getBidiPartAt(bidi, target) == pos){return target;}part = bidi[pos += dir];return dir > 0 == part.level % 2?part.to:part.from;}else {part = bidi[pos += dir];if(!part){return null;}if(dir > 0 == part.level % 2)target = moveInLine(line, part.to, -1, byUnit);else target = moveInLine(line, part.from, 1, byUnit);}}}function moveLogically(line, start, dir, byUnit){var target=start + dir;if(byUnit)while(target > 0 && isExtendingChar(line.text.charAt(target))) target += dir;return target < 0 || target > line.text.length?null:target;}var bidiOrdering=(function(){var lowTypes="bbbbbbbbbtstwsbbbbbbbbbbbbbbssstwNN%%%NNNNNN,N,N1111111111NNNNNNNLLLLLLLLLLLLLLLLLLLLLLLLLLNNNNNNLLLLLLLLLLLLLLLLLLLLLLLLLLNNNNbbbbbbsbbbbbbbbbbbbbbbbbbbbbbbbbb,N%%%%NNNNLNNNNN%%11NLNNN1LNNNNNLLLLLLLLLLLLLLLLLLLLLLLNLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLN";var arabicTypes="rrrrrrrrrrrr,rNNmmmmmmrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrmmmmmmmmmmmmmmrrrrrrrnnnnnnnnnn%nnrrrmrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrmmmmmmmmmmmmmmmmmmmNmmmm";function charType(code){if(code <= 247){return lowTypes.charAt(code);}else if(1424 <= code && code <= 1524){return "R";}else if(1536 <= code && code <= 1773){return arabicTypes.charAt(code - 1536);}else if(1774 <= code && code <= 2220){return "r";}else if(8192 <= code && code <= 8203){return "w";}else if(code == 8204){return "b";}else {return "L";}}var bidiRE=/[\u0590-\u05f4\u0600-\u06ff\u0700-\u08ac]/;var isNeutral=/[stwN]/, isStrong=/[LRr]/, countsAsLeft=/[Lb1n]/, countsAsNum=/[1n]/;var outerType="L";function BidiSpan(level, from, to){this.level = level;this.from = from;this.to = to;}return function(str){if(!bidiRE.test(str))return false;var len=str.length, types=[];for(var i=0, type; i < len; ++i) types.push(type = charType(str.charCodeAt(i)));for(var i=0, prev=outerType; i < len; ++i) {var type=types[i];if(type == "m")types[i] = prev;else prev = type;}for(var i=0, cur=outerType; i < len; ++i) {var type=types[i];if(type == "1" && cur == "r")types[i] = "n";else if(isStrong.test(type)){cur = type;if(type == "r")types[i] = "R";}}for(var i=1, prev=types[0]; i < len - 1; ++i) {var type=types[i];if(type == "+" && prev == "1" && types[i + 1] == "1")types[i] = "1";else if(type == "," && prev == types[i + 1] && (prev == "1" || prev == "n"))types[i] = prev;prev = type;}for(var i=0; i < len; ++i) {var type=types[i];if(type == ",")types[i] = "N";else if(type == "%"){for(var end=i + 1; end < len && types[end] == "%"; ++end) {}var replace=i && types[i - 1] == "!" || end < len && types[end] == "1"?"1":"N";for(var j=i; j < end; ++j) types[j] = replace;i = end - 1;}}for(var i=0, cur=outerType; i < len; ++i) {var type=types[i];if(cur == "L" && type == "1")types[i] = "L";else if(isStrong.test(type))cur = type;}for(var i=0; i < len; ++i) {if(isNeutral.test(types[i])){for(var end=i + 1; end < len && isNeutral.test(types[end]); ++end) {}var before=(i?types[i - 1]:outerType) == "L";var after=(end < len?types[end]:outerType) == "L";var replace=before || after?"L":"R";for(var j=i; j < end; ++j) types[j] = replace;i = end - 1;}}var order=[], m;for(var i=0; i < len;) {if(countsAsLeft.test(types[i])){var start=i;for(++i; i < len && countsAsLeft.test(types[i]); ++i) {}order.push(new BidiSpan(0, start, i));}else {var pos=i, at=order.length;for(++i; i < len && types[i] != "L"; ++i) {}for(var j=pos; j < i;) {if(countsAsNum.test(types[j])){if(pos < j)order.splice(at, 0, new BidiSpan(1, pos, j));var nstart=j;for(++j; j < i && countsAsNum.test(types[j]); ++j) {}order.splice(at, 0, new BidiSpan(2, nstart, j));pos = j;}else ++j;}if(pos < i)order.splice(at, 0, new BidiSpan(1, pos, i));}}if(order[0].level == 1 && (m = str.match(/^\s+/))){order[0].from = m[0].length;order.unshift(new BidiSpan(0, 0, m[0].length));}if(lst(order).level == 1 && (m = str.match(/\s+$/))){lst(order).to -= m[0].length;order.push(new BidiSpan(0, len - m[0].length, len));}if(order[0].level == 2)order.unshift(new BidiSpan(1, order[0].to, order[0].to));if(order[0].level != lst(order).level)order.push(new BidiSpan(order[0].level, len, len));return order;};})();CodeMirror.version = "5.3.0";return CodeMirror;});"use strict";

(function () {
  "use strict";

  var globals = typeof window !== "undefined" ? window : global;
  if (typeof globals.require === "function") return;

  var modules = {};
  var cache = {};

  var has = function has(object, name) {
    return ({}).hasOwnProperty.call(object, name);
  };

  var expand = function expand(root, name) {
    var results = [],
        parts,
        part;
    if (/^\.\.?(\/|$)/.test(name)) {
      parts = [root, name].join("/").split("/");
    } else {
      parts = name.split("/");
    }
    for (var i = 0, length = parts.length; i < length; i++) {
      part = parts[i];
      if (part === "..") {
        results.pop();
      } else if (part !== "." && part !== "") {
        results.push(part);
      }
    }
    return results.join("/");
  };

  var dirname = function dirname(path) {
    return path.split("/").slice(0, -1).join("/");
  };

  var localRequire = function localRequire(path) {
    return function (name) {
      var dir = dirname(path);
      var absolute = expand(dir, name);
      return globals.require(absolute, path);
    };
  };

  var initModule = function initModule(name, definition) {
    var module = { id: name, exports: {} };
    cache[name] = module;
    definition(module.exports, localRequire(name), module);
    return module.exports;
  };

  var require = function require(name, loaderPath) {
    var path = expand(name, ".");
    if (loaderPath == null) loaderPath = "/";

    if (has(cache, path)) {
      return cache[path].exports;
    }if (has(modules, path)) {
      return initModule(path, modules[path]);
    }var dirIndex = expand(path, "./index");
    if (has(cache, dirIndex)) {
      return cache[dirIndex].exports;
    }if (has(modules, dirIndex)) {
      return initModule(dirIndex, modules[dirIndex]);
    }throw new Error("Cannot find module \"" + name + "\" from " + "\"" + loaderPath + "\"");
  };

  var define = function define(bundle, fn) {
    if (typeof bundle === "object") {
      for (var key in bundle) {
        if (has(bundle, key)) {
          modules[key] = bundle[key];
        }
      }
    } else {
      modules[bundle] = fn;
    }
  };

  var list = function list() {
    var result = [];
    for (var item in modules) {
      if (has(modules, item)) {
        result.push(item);
      }
    }
    return result;
  };

  globals.require = require;
  globals.require.define = define;
  globals.require.register = define;
  globals.require.list = list;
  globals.require.brunch = true;
})();
require.define({ phoenix: function (exports, require, module) {
    "use strict";

    var _prototypeProperties = function _prototypeProperties(child, staticProps, instanceProps) {
      if (staticProps) Object.defineProperties(child, staticProps);if (instanceProps) Object.defineProperties(child.prototype, instanceProps);
    };

    var _classCallCheck = function _classCallCheck(instance, Constructor) {
      if (!(instance instanceof Constructor)) {
        throw new TypeError("Cannot call a class as a function");
      }
    };

    // Phoenix Channels JavaScript client
    //
    // ## Socket Connection
    //
    // A single connection is established to the server and
    // channels are mulitplexed over the connection.
    // Connect to the server using the `Socket` class:
    //
    //     let socket = new Socket("/ws")
    //     socket.connect()
    //
    // The `Socket` constructor takes the mount point of the socket
    // as well as options that can be found in the Socket docs,
    // such as configuring the `LongPoller` transport, and heartbeat.
    //
    //
    // ## Channels
    //
    // Channels are isolated, concurrent processes on the server that
    // subscribe to topics and broker events between the client and server.
    // To join a channel, you must provide the topic, and channel params for
    // authorization. Here's an example chat room example where `"new_msg"`
    // events are listened for, messages are pushed to the server, and
    // the channel is joined with ok/error matches, and `after` hook:
    //
    //     let chan = socket.chan("rooms:123", {token: roomToken})
    //     chan.on("new_msg", msg => console.log("Got message", msg) )
    //     $input.onEnter( e => {
    //       chan.push("new_msg", {body: e.target.val})
    //           .receive("ok", (message) => console.log("created message", message) )
    //           .receive("error", (reasons) => console.log("create failed", reasons) )
    //           .after(10000, () => console.log("Networking issue. Still waiting...") )
    //     })
    //     chan.join()
    //         .receive("ok", ({messages}) => console.log("catching up", messages) )
    //         .receive("error", ({reason}) => console.log("failed join", reason) )
    //         .after(10000, () => console.log("Networking issue. Still waiting...") )
    //
    //
    // ## Joining
    //
    // Joining a channel with `chan.join(topic, params)`, binds the params to
    // `chan.params`. Subsequent rejoins will send up the modified params for
    // updating authorization params, or passing up last_message_id information.
    // Successful joins receive an "ok" status, while unsuccessful joins
    // receive "error".
    //
    //
    // ## Pushing Messages
    //
    // From the prevoius example, we can see that pushing messages to the server
    // can be done with `chan.push(eventName, payload)` and we can optionally
    // receive responses from the push. Additionally, we can use
    // `after(millsec, callback)` to abort waiting for our `receive` hooks and
    // take action after some period of waiting.
    //
    //
    // ## Socket Hooks
    //
    // Lifecycle events of the multiplexed connection can be hooked into via
    // `socket.onError()` and `socket.onClose()` events, ie:
    //
    //     socket.onError( () => console.log("there was an error with the connection!") )
    //     socket.onClose( () => console.log("the connection dropped") )
    //
    //
    // ## Channel Hooks
    //
    // For each joined channel, you can bind to `onError` and `onClose` events
    // to monitor the channel lifecycle, ie:
    //
    //     chan.onError( () => console.log("there was an error!") )
    //     chan.onClose( () => console.log("the channel has gone away gracefully") )
    //
    // ### onError hooks
    //
    // `onError` hooks are invoked if the socket connection drops, or the channel
    // crashes on the server. In either case, a channel rejoin is attemtped
    // automatically in an exponential backoff manner.
    //
    // ### onClose hooks
    //
    // `onClose` hooks are invoked only in two cases. 1) the channel explicitly
    // closed on the server, or 2). The client explicitly closed, by calling
    // `chan.leave()`
    //

    var SOCKET_STATES = { connecting: 0, open: 1, closing: 2, closed: 3 };
    var CHAN_STATES = {
      closed: "closed",
      errored: "errored",
      joined: "joined",
      joining: "joining" };
    var CHAN_EVENTS = {
      close: "phx_close",
      error: "phx_error",
      join: "phx_join",
      reply: "phx_reply",
      leave: "phx_leave"
    };

    var Push = (function () {

      // Initializes the Push
      //
      // chan - The Channel
      // event - The event, ie `"phx_join"`
      // payload - The payload, ie `{user_id: 123}`
      //

      function Push(chan, event, payload) {
        _classCallCheck(this, Push);

        this.chan = chan;
        this.event = event;
        this.payload = payload || {};
        this.receivedResp = null;
        this.afterHook = null;
        this.recHooks = [];
        this.sent = false;
      }

      _prototypeProperties(Push, null, {
        send: {
          value: function send() {
            var _this = this;

            var ref = this.chan.socket.makeRef();
            this.refEvent = this.chan.replyEventName(ref);
            this.receivedResp = null;
            this.sent = false;

            this.chan.on(this.refEvent, function (payload) {
              _this.receivedResp = payload;
              _this.matchReceive(payload);
              _this.cancelRefEvent();
              _this.cancelAfter();
            });

            this.startAfter();
            this.sent = true;
            this.chan.socket.push({
              topic: this.chan.topic,
              event: this.event,
              payload: this.payload,
              ref: ref
            });
          },
          writable: true,
          configurable: true
        },
        receive: {
          value: function receive(status, callback) {
            if (this.receivedResp && this.receivedResp.status === status) {
              callback(this.receivedResp.response);
            }

            this.recHooks.push({ status: status, callback: callback });
            return this;
          },
          writable: true,
          configurable: true
        },
        after: {
          value: function after(ms, callback) {
            if (this.afterHook) {
              throw "only a single after hook can be applied to a push";
            }
            var timer = null;
            if (this.sent) {
              timer = setTimeout(callback, ms);
            }
            this.afterHook = { ms: ms, callback: callback, timer: timer };
            return this;
          },
          writable: true,
          configurable: true
        },
        matchReceive: {

          // private

          value: function matchReceive(_ref) {
            var status = _ref.status;
            var response = _ref.response;
            var ref = _ref.ref;

            this.recHooks.filter(function (h) {
              return h.status === status;
            }).forEach(function (h) {
              return h.callback(response);
            });
          },
          writable: true,
          configurable: true
        },
        cancelRefEvent: {
          value: function cancelRefEvent() {
            this.chan.off(this.refEvent);
          },
          writable: true,
          configurable: true
        },
        cancelAfter: {
          value: function cancelAfter() {
            if (!this.afterHook) {
              return;
            }
            clearTimeout(this.afterHook.timer);
            this.afterHook.timer = null;
          },
          writable: true,
          configurable: true
        },
        startAfter: {
          value: function startAfter() {
            var _this = this;

            if (!this.afterHook) {
              return;
            }
            var callback = function callback() {
              _this.cancelRefEvent();
              _this.afterHook.callback();
            };
            this.afterHook.timer = setTimeout(callback, this.afterHook.ms);
          },
          writable: true,
          configurable: true
        }
      });

      return Push;
    })();

    var Channel = exports.Channel = (function () {
      function Channel(topic, params, socket) {
        var _this = this;

        _classCallCheck(this, Channel);

        this.state = CHAN_STATES.closed;
        this.topic = topic;
        this.params = params || {};
        this.socket = socket;
        this.bindings = [];
        this.joinedOnce = false;
        this.joinPush = new Push(this, CHAN_EVENTS.join, this.params);
        this.pushBuffer = [];

        this.joinPush.receive("ok", function () {
          _this.state = CHAN_STATES.joined;
        });
        this.onClose(function () {
          _this.state = CHAN_STATES.closed;
          _this.socket.remove(_this);
        });
        this.onError(function (reason) {
          _this.state = CHAN_STATES.errored;
          setTimeout(function () {
            return _this.rejoinUntilConnected();
          }, _this.socket.reconnectAfterMs);
        });
        this.on(CHAN_EVENTS.reply, function (payload) {
          _this.trigger(_this.replyEventName(payload.ref), payload);
        });
      }

      _prototypeProperties(Channel, null, {
        rejoinUntilConnected: {
          value: function rejoinUntilConnected() {
            var _this = this;

            if (this.state !== CHAN_STATES.errored) {
              return;
            }
            if (this.socket.isConnected()) {
              this.rejoin();
            } else {
              setTimeout(function () {
                return _this.rejoinUntilConnected();
              }, this.socket.reconnectAfterMs);
            }
          },
          writable: true,
          configurable: true
        },
        join: {
          value: function join() {
            if (this.joinedOnce) {
              throw "tried to join mulitple times. 'join' can only be called a singe time per channel instance";
            } else {
              this.joinedOnce = true;
            }
            this.sendJoin();
            return this.joinPush;
          },
          writable: true,
          configurable: true
        },
        onClose: {
          value: function onClose(callback) {
            this.on(CHAN_EVENTS.close, callback);
          },
          writable: true,
          configurable: true
        },
        onError: {
          value: function onError(callback) {
            this.on(CHAN_EVENTS.error, function (reason) {
              return callback(reason);
            });
          },
          writable: true,
          configurable: true
        },
        on: {
          value: function on(event, callback) {
            this.bindings.push({ event: event, callback: callback });
          },
          writable: true,
          configurable: true
        },
        off: {
          value: function off(event) {
            this.bindings = this.bindings.filter(function (bind) {
              return bind.event !== event;
            });
          },
          writable: true,
          configurable: true
        },
        canPush: {
          value: function canPush() {
            return this.socket.isConnected() && this.state === CHAN_STATES.joined;
          },
          writable: true,
          configurable: true
        },
        push: {
          value: function push(event, payload) {
            if (!this.joinedOnce) {
              throw "tried to push '" + event + "' to '" + this.topic + "' before joining. Use chan.join() before pushing events";
            }
            var pushEvent = new Push(this, event, payload);
            if (this.canPush()) {
              pushEvent.send();
            } else {
              this.pushBuffer.push(pushEvent);
            }

            return pushEvent;
          },
          writable: true,
          configurable: true
        },
        leave: {

          // Leaves the channel
          //
          // Unsubscribes from server events, and
          // instructs channel to terminate on server
          //
          // Triggers onClose() hooks
          //
          // To receive leave acknowledgements, use the a `receive`
          // hook to bind to the server ack, ie:
          //
          //     chan.leave().receive("ok", () => alert("left!") )
          //

          value: function leave() {
            var _this = this;

            return this.push(CHAN_EVENTS.leave).receive("ok", function () {
              _this.trigger(CHAN_EVENTS.close, "leave");
            });
          },
          writable: true,
          configurable: true
        },
        isMember: {

          // private

          value: function isMember(topic) {
            return this.topic === topic;
          },
          writable: true,
          configurable: true
        },
        sendJoin: {
          value: function sendJoin() {
            this.state = CHAN_STATES.joining;
            this.joinPush.send();
          },
          writable: true,
          configurable: true
        },
        rejoin: {
          value: function rejoin() {
            this.sendJoin();
            this.pushBuffer.forEach(function (pushEvent) {
              return pushEvent.send();
            });
            this.pushBuffer = [];
          },
          writable: true,
          configurable: true
        },
        trigger: {
          value: function trigger(triggerEvent, msg) {
            this.bindings.filter(function (bind) {
              return bind.event === triggerEvent;
            }).map(function (bind) {
              return bind.callback(msg);
            });
          },
          writable: true,
          configurable: true
        },
        replyEventName: {
          value: function replyEventName(ref) {
            return "chan_reply_" + ref;
          },
          writable: true,
          configurable: true
        }
      });

      return Channel;
    })();

    var Socket = exports.Socket = (function () {

      // Initializes the Socket
      //
      // endPoint - The string WebSocket endpoint, ie, "ws://example.com/ws",
      //                                               "wss://example.com"
      //                                               "/ws" (inherited host & protocol)
      // opts - Optional configuration
      //   transport - The Websocket Transport, ie WebSocket, Phoenix.LongPoller.
      //               Defaults to WebSocket with automatic LongPoller fallback.
      //   heartbeatIntervalMs - The millisec interval to send a heartbeat message
      //   reconnectAfterMs - The millisec interval to reconnect after connection loss
      //   logger - The optional function for specialized logging, ie:
      //            `logger: function(msg){ console.log(msg) }`
      //   longpoller_timeout - The maximum timeout of a long poll AJAX request.
      //                        Defaults to 20s (double the server long poll timer).
      //
      // For IE8 support use an ES5-shim (https://github.com/es-shims/es5-shim)
      //

      function Socket(endPoint) {
        var opts = arguments[1] === undefined ? {} : arguments[1];

        _classCallCheck(this, Socket);

        this.stateChangeCallbacks = { open: [], close: [], error: [], message: [] };
        this.reconnectTimer = null;
        this.channels = [];
        this.sendBuffer = [];
        this.ref = 0;
        this.transport = opts.transport || window.WebSocket || LongPoller;
        this.heartbeatIntervalMs = opts.heartbeatIntervalMs || 30000;
        this.reconnectAfterMs = opts.reconnectAfterMs || 5000;
        this.logger = opts.logger || function () {}; // noop
        this.longpoller_timeout = opts.longpoller_timeout || 20000;
        this.endPoint = this.expandEndpoint(endPoint);
      }

      _prototypeProperties(Socket, null, {
        protocol: {
          value: function protocol() {
            return location.protocol.match(/^https/) ? "wss" : "ws";
          },
          writable: true,
          configurable: true
        },
        expandEndpoint: {
          value: function expandEndpoint(endPoint) {
            if (endPoint.charAt(0) !== "/") {
              return endPoint;
            }
            if (endPoint.charAt(1) === "/") {
              return "" + this.protocol() + ":" + endPoint;
            }

            return "" + this.protocol() + "://" + location.host + "" + endPoint;
          },
          writable: true,
          configurable: true
        },
        disconnect: {
          value: function disconnect(callback, code, reason) {
            if (this.conn) {
              this.conn.onclose = function () {}; // noop
              if (code) {
                this.conn.close(code, reason || "");
              } else {
                this.conn.close();
              }
              this.conn = null;
            }
            callback && callback();
          },
          writable: true,
          configurable: true
        },
        connect: {
          value: function connect() {
            var _this = this;

            this.disconnect(function () {
              _this.conn = new _this.transport(_this.endPoint);
              _this.conn.timeout = _this.longpoller_timeout;
              _this.conn.onopen = function () {
                return _this.onConnOpen();
              };
              _this.conn.onerror = function (error) {
                return _this.onConnError(error);
              };
              _this.conn.onmessage = function (event) {
                return _this.onConnMessage(event);
              };
              _this.conn.onclose = function (event) {
                return _this.onConnClose(event);
              };
            });
          },
          writable: true,
          configurable: true
        },
        log: {

          // Logs the message. Override `this.logger` for specialized logging. noops by default

          value: function log(msg) {
            this.logger(msg);
          },
          writable: true,
          configurable: true
        },
        onOpen: {

          // Registers callbacks for connection state change events
          //
          // Examples
          //
          //    socket.onError(function(error){ alert("An error occurred") })
          //

          value: function onOpen(callback) {
            this.stateChangeCallbacks.open.push(callback);
          },
          writable: true,
          configurable: true
        },
        onClose: {
          value: function onClose(callback) {
            this.stateChangeCallbacks.close.push(callback);
          },
          writable: true,
          configurable: true
        },
        onError: {
          value: function onError(callback) {
            this.stateChangeCallbacks.error.push(callback);
          },
          writable: true,
          configurable: true
        },
        onMessage: {
          value: function onMessage(callback) {
            this.stateChangeCallbacks.message.push(callback);
          },
          writable: true,
          configurable: true
        },
        onConnOpen: {
          value: function onConnOpen() {
            var _this = this;

            this.flushSendBuffer();
            clearInterval(this.reconnectTimer);
            if (!this.conn.skipHeartbeat) {
              clearInterval(this.heartbeatTimer);
              this.heartbeatTimer = setInterval(function () {
                return _this.sendHeartbeat();
              }, this.heartbeatIntervalMs);
            }
            this.stateChangeCallbacks.open.forEach(function (callback) {
              return callback();
            });
          },
          writable: true,
          configurable: true
        },
        onConnClose: {
          value: function onConnClose(event) {
            var _this = this;

            this.log("WS close:");
            this.log(event);
            this.triggerChanError();
            clearInterval(this.reconnectTimer);
            clearInterval(this.heartbeatTimer);
            this.reconnectTimer = setInterval(function () {
              return _this.connect();
            }, this.reconnectAfterMs);
            this.stateChangeCallbacks.close.forEach(function (callback) {
              return callback(event);
            });
          },
          writable: true,
          configurable: true
        },
        onConnError: {
          value: function onConnError(error) {
            this.log("WS error:");
            this.log(error);
            this.triggerChanError();
            this.stateChangeCallbacks.error.forEach(function (callback) {
              return callback(error);
            });
          },
          writable: true,
          configurable: true
        },
        triggerChanError: {
          value: function triggerChanError() {
            this.channels.forEach(function (chan) {
              return chan.trigger(CHAN_EVENTS.error);
            });
          },
          writable: true,
          configurable: true
        },
        connectionState: {
          value: function connectionState() {
            switch (this.conn && this.conn.readyState) {
              case SOCKET_STATES.connecting:
                return "connecting";
              case SOCKET_STATES.open:
                return "open";
              case SOCKET_STATES.closing:
                return "closing";
              default:
                return "closed";
            }
          },
          writable: true,
          configurable: true
        },
        isConnected: {
          value: function isConnected() {
            return this.connectionState() === "open";
          },
          writable: true,
          configurable: true
        },
        remove: {
          value: function remove(chan) {
            this.channels = this.channels.filter(function (c) {
              return !c.isMember(chan.topic);
            });
          },
          writable: true,
          configurable: true
        },
        chan: {
          value: function chan(topic, params) {
            var chan = new Channel(topic, params, this);
            this.channels.push(chan);
            return chan;
          },
          writable: true,
          configurable: true
        },
        push: {
          value: function push(data) {
            var _this = this;

            var callback = function callback() {
              return _this.conn.send(JSON.stringify(data));
            };
            if (this.isConnected()) {
              callback();
            } else {
              this.sendBuffer.push(callback);
            }
          },
          writable: true,
          configurable: true
        },
        makeRef: {

          // Return the next message ref, accounting for overflows

          value: function makeRef() {
            var newRef = this.ref + 1;
            if (newRef === this.ref) {
              this.ref = 0;
            } else {
              this.ref = newRef;
            }

            return this.ref.toString();
          },
          writable: true,
          configurable: true
        },
        sendHeartbeat: {
          value: function sendHeartbeat() {
            this.push({ topic: "phoenix", event: "heartbeat", payload: {}, ref: this.makeRef() });
          },
          writable: true,
          configurable: true
        },
        flushSendBuffer: {
          value: function flushSendBuffer() {
            if (this.isConnected() && this.sendBuffer.length > 0) {
              this.sendBuffer.forEach(function (callback) {
                return callback();
              });
              this.sendBuffer = [];
            }
          },
          writable: true,
          configurable: true
        },
        onConnMessage: {
          value: function onConnMessage(rawMessage) {
            this.log("message received:");
            this.log(rawMessage);

            var _JSON$parse = JSON.parse(rawMessage.data);

            var topic = _JSON$parse.topic;
            var event = _JSON$parse.event;
            var payload = _JSON$parse.payload;

            this.channels.filter(function (chan) {
              return chan.isMember(topic);
            }).forEach(function (chan) {
              return chan.trigger(event, payload);
            });
            this.stateChangeCallbacks.message.forEach(function (callback) {
              callback(topic, event, payload);
            });
          },
          writable: true,
          configurable: true
        }
      });

      return Socket;
    })();

    var LongPoller = exports.LongPoller = (function () {
      function LongPoller(endPoint) {
        _classCallCheck(this, LongPoller);

        this.retryInMs = 5000;
        this.endPoint = null;
        this.token = null;
        this.sig = null;
        this.skipHeartbeat = true;
        this.onopen = function () {}; // noop
        this.onerror = function () {}; // noop
        this.onmessage = function () {}; // noop
        this.onclose = function () {}; // noop
        this.upgradeEndpoint = this.normalizeEndpoint(endPoint);
        this.pollEndpoint = this.upgradeEndpoint + (/\/$/.test(endPoint) ? "poll" : "/poll");
        this.readyState = SOCKET_STATES.connecting;

        this.poll();
      }

      _prototypeProperties(LongPoller, null, {
        normalizeEndpoint: {
          value: function normalizeEndpoint(endPoint) {
            return endPoint.replace("ws://", "http://").replace("wss://", "https://");
          },
          writable: true,
          configurable: true
        },
        endpointURL: {
          value: function endpointURL() {
            return this.pollEndpoint + ("?token=" + encodeURIComponent(this.token) + "&sig=" + encodeURIComponent(this.sig));
          },
          writable: true,
          configurable: true
        },
        closeAndRetry: {
          value: function closeAndRetry() {
            this.close();
            this.readyState = SOCKET_STATES.connecting;
          },
          writable: true,
          configurable: true
        },
        ontimeout: {
          value: function ontimeout() {
            this.onerror("timeout");
            this.closeAndRetry();
          },
          writable: true,
          configurable: true
        },
        poll: {
          value: function poll() {
            var _this = this;

            if (!(this.readyState === SOCKET_STATES.open || this.readyState === SOCKET_STATES.connecting)) {
              return;
            }

            Ajax.request("GET", this.endpointURL(), "application/json", null, this.timeout, this.ontimeout.bind(this), function (resp) {
              if (resp) {
                var status = resp.status;
                var token = resp.token;
                var sig = resp.sig;
                var messages = resp.messages;

                _this.token = token;
                _this.sig = sig;
              } else {
                var status = 0;
              }

              switch (status) {
                case 200:
                  messages.forEach(function (msg) {
                    return _this.onmessage({ data: JSON.stringify(msg) });
                  });
                  _this.poll();
                  break;
                case 204:
                  _this.poll();
                  break;
                case 410:
                  _this.readyState = SOCKET_STATES.open;
                  _this.onopen();
                  _this.poll();
                  break;
                case 0:
                case 500:
                  _this.onerror();
                  _this.closeAndRetry();
                  break;
                default:
                  throw "unhandled poll status " + status;
              }
            });
          },
          writable: true,
          configurable: true
        },
        send: {
          value: function send(body) {
            var _this = this;

            Ajax.request("POST", this.endpointURL(), "application/json", body, this.timeout, this.onerror.bind(this, "timeout"), function (resp) {
              if (!resp || resp.status !== 200) {
                _this.onerror(status);
                _this.closeAndRetry();
              }
            });
          },
          writable: true,
          configurable: true
        },
        close: {
          value: function close(code, reason) {
            this.readyState = SOCKET_STATES.closed;
            this.onclose();
          },
          writable: true,
          configurable: true
        }
      });

      return LongPoller;
    })();

    var Ajax = exports.Ajax = (function () {
      function Ajax() {
        _classCallCheck(this, Ajax);
      }

      _prototypeProperties(Ajax, {
        request: {
          value: function request(method, endPoint, accept, body, timeout, ontimeout, callback) {
            if (window.XDomainRequest) {
              var req = new XDomainRequest(); // IE8, IE9
              this.xdomainRequest(req, method, endPoint, body, timeout, ontimeout, callback);
            } else {
              var req = window.XMLHttpRequest ? new XMLHttpRequest() : // IE7+, Firefox, Chrome, Opera, Safari
              new ActiveXObject("Microsoft.XMLHTTP"); // IE6, IE5
              this.xhrRequest(req, method, endPoint, accept, body, timeout, ontimeout, callback);
            }
          },
          writable: true,
          configurable: true
        },
        xdomainRequest: {
          value: function xdomainRequest(req, method, endPoint, body, timeout, ontimeout, callback) {
            var _this = this;

            req.timeout = timeout;
            req.open(method, endPoint);
            req.onload = function () {
              var response = _this.parseJSON(req.responseText);
              callback && callback(response);
            };
            if (ontimeout) {
              req.ontimeout = ontimeout;
            }

            // Work around bug in IE9 that requires an attached onprogress handler
            req.onprogress = function () {};

            req.send(body);
          },
          writable: true,
          configurable: true
        },
        xhrRequest: {
          value: function xhrRequest(req, method, endPoint, accept, body, timeout, ontimeout, callback) {
            var _this = this;

            req.timeout = timeout;
            req.open(method, endPoint, true);
            req.setRequestHeader("Content-Type", accept);
            req.onerror = function () {
              callback && callback(null);
            };
            req.onreadystatechange = function () {
              if (req.readyState === _this.states.complete && callback) {
                var response = _this.parseJSON(req.responseText);
                callback(response);
              }
            };
            if (ontimeout) {
              req.ontimeout = ontimeout;
            }

            req.send(body);
          },
          writable: true,
          configurable: true
        },
        parseJSON: {
          value: function parseJSON(resp) {
            return resp && resp !== "" ? JSON.parse(resp) : null;
          },
          writable: true,
          configurable: true
        }
      });

      return Ajax;
    })();

    Ajax.states = { complete: 4 };
    Object.defineProperty(exports, "__esModule", {
      value: true
    });
  } });
if (typeof window === "object" && !window.Phoenix) {
  window.Phoenix = require("phoenix");
};
/*! Brunch !*/require.register("web/static/js/app", function(exports, require, module) {
"use strict";

var addToScroll, adjustScrollTop, clearScroll, command_history, disableField, focus, focusNext, focusPrevious, history_marker, setFocus, updateRoom, socket, push;
focus = null;
$("html").on("click", function (event) {
  if (window.getSelection().type !== "Range") {
    return setFocus("#command");
  }
});

updateRoom = function (data) {
  $("#room .title").html(data.name);
  $("#room .description").html(data.description);
  if (data.entities.length > 0) {
    $("#room .entities").html("<span class='dark-magenta'>Also here:</span> <span class='magenta'>" + data.entities.join(", ") + "</span><span class='dark-magenta'>.</span>");
  } else {
    $("#room .entities").html("");
  }
  $("#room .exits").html("Obvious exits: " + (data.exits.join(", ") || "NONE"));
  return adjustScrollTop();
};

clearScroll = function () {
  return $("#scroll").html("");
};

adjustScrollTop = function () {
  if ($(window).scrollTop() + $(window).height() > $(document).height() - 250) {
    return window.scrollTo(0, $("#scroll")[0].scrollHeight);
  }
};

setFocus = function (selector) {
  focus = selector;
  selector = $(selector);
  var x = window.scrollX,
      y = window.scrollY;
  selector.focus();
  window.scrollTo(x, y);
  return selector;
};
adjustScrollTop();

var socket = new Phoenix.Socket("" + window.location.origin.replace("http", "ws") + "/ws");
socket.connect();
var chan = socket.chan("mud", { spirit: spiritID });

chan.join();

chan.on("room", function (message) {
  updateRoom(message.html);
});

chan.on("clear scroll", function (message) {
  clearScroll();
});

chan.on("focus", function (message) {
  setFocus(message.html).select();
});

chan.on("disable", function (message) {
  disableField(message.html);
});

chan.on("update prompt", function (message) {
  $("#prompt").text(message.html);
});

chan.on("redirect", function (message) {
  window.location = "" + window.location.origin + message.url;
});

chan.on("up", function (message) {
  command_history("up");
});

chan.on("scroll", function (message) {
  addToScroll("#scroll", message.html);
});

push = function (event, message) {
  chan.push(event, message);
};

addToScroll = function (elem, text) {
  $(elem).append(text);
  $(elem).append($("#prompt").parent().detach());
  setFocus(focus);
  return adjustScrollTop();
};

focusNext = function (elem) {
  var field, fields;
  fields = $("#scroll").find("input:not([disabled])");
  field = fields.eq(fields.index(elem) + 1)[0];
  if (field) {
    return setFocus("#" + field.id).select();
  }
};

focusPrevious = function (elem) {
  var field, fields;
  fields = $("#scroll").find(":input");
  field = fields.eq(fields.index(elem) - 1)[0];
  if (field) {
    return setFocus("#" + field.id).select();
  }
};

disableField = function (selector) {
  return $(selector).prop("disabled", true).removeAttr("id");
};

history_marker = null;
command_history = function (direction) {
  var history;
  history = $(".prompt:disabled");
  if (history.length === 0) {
    return;
  }
  if (history_marker === null) {
    history_marker = history.length;
  }
  if (direction === "up") {
    history_marker = Math.max(0, history_marker - 1);
  } else if (direction === "down") {
    history_marker = Math.min(history.length - 1, history_marker + 1);
  }
  $("#command").val(history[history_marker].value);
  return setFocus("#command").select();
};

$(document).on("keydown", "input", function (event) {
  if (event.which === 9 && !event.shiftKey) {
    return event.preventDefault();
  }
});

$(document).on("keydown", function (event) {
  if (!(event.ctrlKey || event.shiftKey || event.metaKey)) {
    setFocus("#command");
  } else if (event.which === 75 && event.metaKey) {
    var prompt = $("#prompt").parent().detach();
    clearScroll();
    $("#scroll").append(prompt);
    setFocus("#command");
  }
});

$(document).on("keyup", function (event) {
  setFocus("#command");
});

$(document).on("keyup", "input", function (event) {
  var command, params;
  event.preventDefault();
  if (event.which === 13 || event.which === 9 && !event.shiftKey) {
    history_marker = null;
    command = $(event.target).val();
    if (command === "reroll") {
      if (confirm("Rerolling will allow you to change your name and/or faction, but you will only retain 10% of your current experience. Are you sure you wish to reroll?") === true) {
        return push(event.target.id, command);
      } else {
        $(event.target).val("");
      }
    } else {
      return push(event.target.id, command);
    }
  } else if (event.which === 38) {
    return command_history("up");
  } else if (event.which === 40) {
    return command_history("down");
  }
});});

require.register("web/static/js/index", function(exports, require, module) {
"use strict";

var update_war_status = function update_war_status(war_status) {
  $("#war-status").html(war_status.stats.join("\n    "));
};

var socket = new Phoenix.Socket("" + window.location.origin.replace("http", "ws") + "/ws");
socket.connect();
var chan = socket.chan("index", {});

chan.join().receive("ok", function (message) {
  update_war_status(message);
});

chan.on("war-status", function (message) {
  update_war_status(message);
});});

require.register("web/static/js/json-edit", function(exports, require, module) {
"use strict";

$("textarea.json").each(function () {
  var textarea = $(this);

  var myCodeMirror = CodeMirror.fromTextArea(textarea.get(0), {
    mode: { name: "javascript", json: true },
    lineNumbers: true,
    smartIndent: true,
    theme: "twilight",
    matchBrackets: true
  });

  myCodeMirror.setValue(JSON.stringify(JSON.parse(myCodeMirror.getValue()), null, "  "));
});

$(document).on("keyup", "#search", function (event) {
  if (event.which === 13) {
    var query = $("#search").val();
    window.location = "" + window.location.origin + window.location.pathname + "?q=" + query;
  }
});});

require.register("web/static/js/nav", function(exports, require, module) {
"use strict";

$(document).ready(function () {

  // Variables
  var $nav = $(".navbar"),
      $body = $("body"),
      $window = $(window),
      $popoverLink = $("[data-popover]"),
      navOffsetTop = $nav.offset().top,
      $document = $(document),
      entityMap = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    "\"": "&quot;",
    "'": "&#39;",
    "/": "&#x2F;"
  },
      $target = "";

  function init() {
    $window.on("scroll", onScroll);
    $window.on("resize", resize);
    $popoverLink.on("click", openPopover);
    $document.on("click", closePopover);
    $("a[href^=\"#\"]").on("click", smoothScroll);
    onScroll();
  }

  function smoothScroll(e) {
    e.preventDefault();
    $(document).off("scroll");
    var target = this.hash,
        menu = target;
    $target = $(target);
    $("html, body").stop().animate({
      scrollTop: $target.offset().top - 80
    }, 0, "swing", function () {
      window.location.hash = target;
      $(document).on("scroll", onScroll);
    });
  }

  function openPopover(e) {
    e.preventDefault();
    closePopover();
    var popover = $($(this).data("popover"));
    popover.toggleClass("open");
    e.stopImmediatePropagation();
  }

  function closePopover(e) {
    if ($(".popover.open").length > 0) {
      $(".popover").removeClass("open");
    }
  }

  $("#button").click(function () {
    $("html, body").animate({
      scrollTop: $("#elementtoScrollToID").offset().top
    }, 2000);
  });

  function resize() {
    $body.removeClass("has-docked-nav");
    navOffsetTop = $nav.offset().top;
    onScroll();
  }

  function onScroll() {
    if (navOffsetTop <= $window.scrollTop() && !$body.hasClass("has-docked-nav")) {
      $("#nav-title").show();
      $body.addClass("has-docked-nav");
    }
    if (navOffsetTop > $window.scrollTop() && $body.hasClass("has-docked-nav")) {
      $body.removeClass("has-docked-nav");
      $("#nav-title").hide();
    }
  }

  init();
});});


//# sourceMappingURL=app.js.map