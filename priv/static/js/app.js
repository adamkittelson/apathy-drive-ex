(function() {
  'use strict';

  var globals = typeof window === 'undefined' ? global : window;
  if (typeof globals.require === 'function') return;

  var modules = {};
  var cache = {};
  var has = ({}).hasOwnProperty;

  var aliases = {};

  var endsWith = function(str, suffix) {
    return str.indexOf(suffix, str.length - suffix.length) !== -1;
  };

  var unalias = function(alias, loaderPath) {
    var start = 0;
    if (loaderPath) {
      if (loaderPath.indexOf('components/' === 0)) {
        start = 'components/'.length;
      }
      if (loaderPath.indexOf('/', start) > 0) {
        loaderPath = loaderPath.substring(start, loaderPath.indexOf('/', start));
      }
    }
    var result = aliases[alias + '/index.js'] || aliases[loaderPath + '/deps/' + alias + '/index.js'];
    if (result) {
      return 'components/' + result.substring(0, result.length - '.js'.length);
    }
    return alias;
  };

  var expand = (function() {
    var reg = /^\.\.?(\/|$)/;
    return function(root, name) {
      var results = [], parts, part;
      parts = (reg.test(name) ? root + '/' + name : name).split('/');
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
  })();
  var dirname = function(path) {
    return path.split('/').slice(0, -1).join('/');
  };

  var localRequire = function(path) {
    return function(name) {
      var absolute = expand(dirname(path), name);
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
    path = unalias(name, loaderPath);

    if (has.call(cache, path)) return cache[path].exports;
    if (has.call(modules, path)) return initModule(path, modules[path]);

    var dirIndex = expand(path, './index');
    if (has.call(cache, dirIndex)) return cache[dirIndex].exports;
    if (has.call(modules, dirIndex)) return initModule(dirIndex, modules[dirIndex]);

    throw new Error('Cannot find module "' + name + '" from '+ '"' + loaderPath + '"');
  };

  require.alias = function(from, to) {
    aliases[to] = from;
  };

  require.register = require.define = function(bundle, fn) {
    if (typeof bundle === 'object') {
      for (var key in bundle) {
        if (has.call(bundle, key)) {
          modules[key] = bundle[key];
        }
      }
    } else {
      modules[bundle] = fn;
    }
  };

  require.list = function() {
    var result = [];
    for (var item in modules) {
      if (has.call(modules, item)) {
        result.push(item);
      }
    }
    return result;
  };

  require.brunch = true;
  globals.require = require;
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
})(window);"use strict";(function(f){if(typeof exports === "object" && typeof module !== "undefined"){module.exports = f();}else if(typeof define === "function" && define.amd){define([], f);}else {var g;if(typeof window !== "undefined"){g = window;}else if(typeof global !== "undefined"){g = global;}else if(typeof self !== "undefined"){g = self;}else {g = this;}g.PIXI = f();}})(function(){var define, module, exports;return (function e(t, n, r){function s(o, u){if(!n[o]){if(!t[o]){var a=typeof require == "function" && require;if(!u && a){return a(o, !0);}if(i){return i(o, !0);}var f=new Error("Cannot find module '" + o + "'");throw (f.code = "MODULE_NOT_FOUND", f);}var l=n[o] = {exports:{}};t[o][0].call(l.exports, function(e){var n=t[o][1][e];return s(n?n:e);}, l, l.exports, e, t, n, r);}return n[o].exports;}var i=typeof require == "function" && require;for(var o=0; o < r.length; o++) s(r[o]);return s;})({1:[function(require, module, exports){(function(process, global){(function(){var async={};function noop(){}function identity(v){return v;}function toBool(v){return !!v;}function notId(v){return !v;}var previous_async;var root=typeof self === "object" && self.self === self && self || typeof global === "object" && global.global === global && global || this;if(root != null){previous_async = root.async;}async.noConflict = function(){root.async = previous_async;return async;};function only_once(fn){return function(){if(fn === null)throw new Error("Callback was already called.");fn.apply(this, arguments);fn = null;};}function _once(fn){return function(){if(fn === null)return;fn.apply(this, arguments);fn = null;};}var _toString=Object.prototype.toString;var _isArray=Array.isArray || function(obj){return _toString.call(obj) === "[object Array]";};var _isObject=function _isObject(obj){var type=typeof obj;return type === "function" || type === "object" && !!obj;};function _isArrayLike(arr){return _isArray(arr) || typeof arr.length === "number" && arr.length >= 0 && arr.length % 1 === 0;}function _arrayEach(arr, iterator){var index=-1, length=arr.length;while(++index < length) {iterator(arr[index], index, arr);}}function _map(arr, iterator){var index=-1, length=arr.length, result=Array(length);while(++index < length) {result[index] = iterator(arr[index], index, arr);}return result;}function _range(count){return _map(Array(count), function(v, i){return i;});}function _reduce(arr, iterator, memo){_arrayEach(arr, function(x, i, a){memo = iterator(memo, x, i, a);});return memo;}function _forEachOf(object, iterator){_arrayEach(_keys(object), function(key){iterator(object[key], key);});}function _indexOf(arr, item){for(var i=0; i < arr.length; i++) {if(arr[i] === item){return i;}}return -1;}var _keys=Object.keys || function(obj){var keys=[];for(var k in obj) {if(obj.hasOwnProperty(k)){keys.push(k);}}return keys;};function _keyIterator(coll){var i=-1;var len;var keys;if(_isArrayLike(coll)){len = coll.length;return function next(){i++;return i < len?i:null;};}else {keys = _keys(coll);len = keys.length;return function next(){i++;return i < len?keys[i]:null;};}}function _restParam(func, startIndex){startIndex = startIndex == null?func.length - 1:+startIndex;return function(){var length=Math.max(arguments.length - startIndex, 0);var rest=Array(length);for(var index=0; index < length; index++) {rest[index] = arguments[index + startIndex];}switch(startIndex){case 0:return func.call(this, rest);case 1:return func.call(this, arguments[0], rest);}};}function _withoutIndex(iterator){return function(value, index, callback){return iterator(value, callback);};}var _setImmediate=typeof setImmediate === "function" && setImmediate;var _delay=_setImmediate?function(fn){_setImmediate(fn);}:function(fn){setTimeout(fn, 0);};if(typeof process === "object" && typeof process.nextTick === "function"){async.nextTick = process.nextTick;}else {async.nextTick = _delay;}async.setImmediate = _setImmediate?_delay:async.nextTick;async.forEach = async.each = function(arr, iterator, callback){return async.eachOf(arr, _withoutIndex(iterator), callback);};async.forEachSeries = async.eachSeries = function(arr, iterator, callback){return async.eachOfSeries(arr, _withoutIndex(iterator), callback);};async.forEachLimit = async.eachLimit = function(arr, limit, iterator, callback){return _eachOfLimit(limit)(arr, _withoutIndex(iterator), callback);};async.forEachOf = async.eachOf = function(object, iterator, callback){callback = _once(callback || noop);object = object || [];var iter=_keyIterator(object);var key, completed=0;while((key = iter()) != null) {completed += 1;iterator(object[key], key, only_once(done));}if(completed === 0)callback(null);function done(err){completed--;if(err){callback(err);}else if(key === null && completed <= 0){callback(null);}}};async.forEachOfSeries = async.eachOfSeries = function(obj, iterator, callback){callback = _once(callback || noop);obj = obj || [];var nextKey=_keyIterator(obj);var key=nextKey();function iterate(){var sync=true;if(key === null){return callback(null);}iterator(obj[key], key, only_once(function(err){if(err){callback(err);}else {key = nextKey();if(key === null){return callback(null);}else {if(sync){async.setImmediate(iterate);}else {iterate();}}}}));sync = false;}iterate();};async.forEachOfLimit = async.eachOfLimit = function(obj, limit, iterator, callback){_eachOfLimit(limit)(obj, iterator, callback);};function _eachOfLimit(limit){return function(obj, iterator, callback){callback = _once(callback || noop);obj = obj || [];var nextKey=_keyIterator(obj);if(limit <= 0){return callback(null);}var done=false;var running=0;var errored=false;(function replenish(){if(done && running <= 0){return callback(null);}while(running < limit && !errored) {var key=nextKey();if(key === null){done = true;if(running <= 0){callback(null);}return;}running += 1;iterator(obj[key], key, only_once(function(err){running -= 1;if(err){callback(err);errored = true;}else {replenish();}}));}})();};}function doParallel(fn){return function(obj, iterator, callback){return fn(async.eachOf, obj, iterator, callback);};}function doParallelLimit(fn){return function(obj, limit, iterator, callback){return fn(_eachOfLimit(limit), obj, iterator, callback);};}function doSeries(fn){return function(obj, iterator, callback){return fn(async.eachOfSeries, obj, iterator, callback);};}function _asyncMap(eachfn, arr, iterator, callback){callback = _once(callback || noop);arr = arr || [];var results=_isArrayLike(arr)?[]:{};eachfn(arr, function(value, index, callback){iterator(value, function(err, v){results[index] = v;callback(err);});}, function(err){callback(err, results);});}async.map = doParallel(_asyncMap);async.mapSeries = doSeries(_asyncMap);async.mapLimit = doParallelLimit(_asyncMap);async.inject = async.foldl = async.reduce = function(arr, memo, iterator, callback){async.eachOfSeries(arr, function(x, i, callback){iterator(memo, x, function(err, v){memo = v;callback(err);});}, function(err){callback(err, memo);});};async.foldr = async.reduceRight = function(arr, memo, iterator, callback){var reversed=_map(arr, identity).reverse();async.reduce(reversed, memo, iterator, callback);};async.transform = function(arr, memo, iterator, callback){if(arguments.length === 3){callback = iterator;iterator = memo;memo = _isArray(arr)?[]:{};}async.eachOf(arr, function(v, k, cb){iterator(memo, v, k, cb);}, function(err){callback(err, memo);});};function _filter(eachfn, arr, iterator, callback){var results=[];eachfn(arr, function(x, index, callback){iterator(x, function(v){if(v){results.push({index:index, value:x});}callback();});}, function(){callback(_map(results.sort(function(a, b){return a.index - b.index;}), function(x){return x.value;}));});}async.select = async.filter = doParallel(_filter);async.selectLimit = async.filterLimit = doParallelLimit(_filter);async.selectSeries = async.filterSeries = doSeries(_filter);function _reject(eachfn, arr, iterator, callback){_filter(eachfn, arr, function(value, cb){iterator(value, function(v){cb(!v);});}, callback);}async.reject = doParallel(_reject);async.rejectLimit = doParallelLimit(_reject);async.rejectSeries = doSeries(_reject);function _createTester(eachfn, check, getResult){return function(arr, limit, iterator, cb){function done(){if(cb)cb(getResult(false, void 0));}function iteratee(x, _, callback){if(!cb){return callback();}iterator(x, function(v){if(cb && check(v)){cb(getResult(true, x));cb = iterator = false;}callback();});}if(arguments.length > 3){eachfn(arr, limit, iteratee, done);}else {cb = iterator;iterator = limit;eachfn(arr, iteratee, done);}};}async.any = async.some = _createTester(async.eachOf, toBool, identity);async.someLimit = _createTester(async.eachOfLimit, toBool, identity);async.all = async.every = _createTester(async.eachOf, notId, notId);async.everyLimit = _createTester(async.eachOfLimit, notId, notId);function _findGetResult(v, x){return x;}async.detect = _createTester(async.eachOf, identity, _findGetResult);async.detectSeries = _createTester(async.eachOfSeries, identity, _findGetResult);async.detectLimit = _createTester(async.eachOfLimit, identity, _findGetResult);async.sortBy = function(arr, iterator, callback){async.map(arr, function(x, callback){iterator(x, function(err, criteria){if(err){callback(err);}else {callback(null, {value:x, criteria:criteria});}});}, function(err, results){if(err){return callback(err);}else {callback(null, _map(results.sort(comparator), function(x){return x.value;}));}});function comparator(left, right){var a=left.criteria, b=right.criteria;return a < b?-1:a > b?1:0;}};async.auto = function(tasks, concurrency, callback){if(typeof arguments[1] === "function"){callback = concurrency;concurrency = null;}callback = _once(callback || noop);var keys=_keys(tasks);var remainingTasks=keys.length;if(!remainingTasks){return callback(null);}if(!concurrency){concurrency = remainingTasks;}var results={};var runningTasks=0;var hasError=false;var listeners=[];function addListener(fn){listeners.unshift(fn);}function removeListener(fn){var idx=_indexOf(listeners, fn);if(idx >= 0)listeners.splice(idx, 1);}function taskComplete(){remainingTasks--;_arrayEach(listeners.slice(0), function(fn){fn();});}addListener(function(){if(!remainingTasks){callback(null, results);}});_arrayEach(keys, function(k){if(hasError)return;var task=_isArray(tasks[k])?tasks[k]:[tasks[k]];var taskCallback=_restParam(function(err, args){runningTasks--;if(args.length <= 1){args = args[0];}if(err){var safeResults={};_forEachOf(results, function(val, rkey){safeResults[rkey] = val;});safeResults[k] = args;hasError = true;callback(err, safeResults);}else {results[k] = args;async.setImmediate(taskComplete);}});var requires=task.slice(0, task.length - 1);var len=requires.length;var dep;while(len--) {if(!(dep = tasks[requires[len]])){throw new Error("Has nonexistent dependency in " + requires.join(", "));}if(_isArray(dep) && _indexOf(dep, k) >= 0){throw new Error("Has cyclic dependencies");}}function ready(){return runningTasks < concurrency && _reduce(requires, function(a, x){return a && results.hasOwnProperty(x);}, true) && !results.hasOwnProperty(k);}if(ready()){runningTasks++;task[task.length - 1](taskCallback, results);}else {addListener(listener);}function listener(){if(ready()){runningTasks++;removeListener(listener);task[task.length - 1](taskCallback, results);}}});};async.retry = function(times, task, callback){var DEFAULT_TIMES=5;var DEFAULT_INTERVAL=0;var attempts=[];var opts={times:DEFAULT_TIMES, interval:DEFAULT_INTERVAL};function parseTimes(acc, t){if(typeof t === "number"){acc.times = parseInt(t, 10) || DEFAULT_TIMES;}else if(typeof t === "object"){acc.times = parseInt(t.times, 10) || DEFAULT_TIMES;acc.interval = parseInt(t.interval, 10) || DEFAULT_INTERVAL;}else {throw new Error("Unsupported argument type for 'times': " + typeof t);}}var length=arguments.length;if(length < 1 || length > 3){throw new Error("Invalid arguments - must be either (task), (task, callback), (times, task) or (times, task, callback)");}else if(length <= 2 && typeof times === "function"){callback = task;task = times;}if(typeof times !== "function"){parseTimes(opts, times);}opts.callback = callback;opts.task = task;function wrappedTask(wrappedCallback, wrappedResults){function retryAttempt(task, finalAttempt){return function(seriesCallback){task(function(err, result){seriesCallback(!err || finalAttempt, {err:err, result:result});}, wrappedResults);};}function retryInterval(interval){return function(seriesCallback){setTimeout(function(){seriesCallback(null);}, interval);};}while(opts.times) {var finalAttempt=!(opts.times -= 1);attempts.push(retryAttempt(opts.task, finalAttempt));if(!finalAttempt && opts.interval > 0){attempts.push(retryInterval(opts.interval));}}async.series(attempts, function(done, data){data = data[data.length - 1];(wrappedCallback || opts.callback)(data.err, data.result);});}return opts.callback?wrappedTask():wrappedTask;};async.waterfall = function(tasks, callback){callback = _once(callback || noop);if(!_isArray(tasks)){var err=new Error("First argument to waterfall must be an array of functions");return callback(err);}if(!tasks.length){return callback();}function wrapIterator(iterator){return _restParam(function(err, args){if(err){callback.apply(null, [err].concat(args));}else {var next=iterator.next();if(next){args.push(wrapIterator(next));}else {args.push(callback);}ensureAsync(iterator).apply(null, args);}});}wrapIterator(async.iterator(tasks))();};function _parallel(eachfn, tasks, callback){callback = callback || noop;var results=_isArrayLike(tasks)?[]:{};eachfn(tasks, function(task, key, callback){task(_restParam(function(err, args){if(args.length <= 1){args = args[0];}results[key] = args;callback(err);}));}, function(err){callback(err, results);});}async.parallel = function(tasks, callback){_parallel(async.eachOf, tasks, callback);};async.parallelLimit = function(tasks, limit, callback){_parallel(_eachOfLimit(limit), tasks, callback);};async.series = function(tasks, callback){_parallel(async.eachOfSeries, tasks, callback);};async.iterator = function(tasks){function makeCallback(index){function fn(){if(tasks.length){tasks[index].apply(null, arguments);}return fn.next();}fn.next = function(){return index < tasks.length - 1?makeCallback(index + 1):null;};return fn;}return makeCallback(0);};async.apply = _restParam(function(fn, args){return _restParam(function(callArgs){return fn.apply(null, args.concat(callArgs));});});function _concat(eachfn, arr, fn, callback){var result=[];eachfn(arr, function(x, index, cb){fn(x, function(err, y){result = result.concat(y || []);cb(err);});}, function(err){callback(err, result);});}async.concat = doParallel(_concat);async.concatSeries = doSeries(_concat);async.whilst = function(test, iterator, callback){callback = callback || noop;if(test()){var next=_restParam(function(err, args){if(err){callback(err);}else if(test.apply(this, args)){iterator(next);}else {callback.apply(null, [null].concat(args));}});iterator(next);}else {callback(null);}};async.doWhilst = function(iterator, test, callback){var calls=0;return async.whilst(function(){return ++calls <= 1 || test.apply(this, arguments);}, iterator, callback);};async.until = function(test, iterator, callback){return async.whilst(function(){return !test.apply(this, arguments);}, iterator, callback);};async.doUntil = function(iterator, test, callback){return async.doWhilst(iterator, function(){return !test.apply(this, arguments);}, callback);};async.during = function(test, iterator, callback){callback = callback || noop;var next=_restParam(function(err, args){if(err){callback(err);}else {args.push(check);test.apply(this, args);}});var check=function check(err, truth){if(err){callback(err);}else if(truth){iterator(next);}else {callback(null);}};test(check);};async.doDuring = function(iterator, test, callback){var calls=0;async.during(function(next){if(calls++ < 1){next(null, true);}else {test.apply(this, arguments);}}, iterator, callback);};function _queue(worker, concurrency, payload){if(concurrency == null){concurrency = 1;}else if(concurrency === 0){throw new Error("Concurrency must not be zero");}function _insert(q, data, pos, callback){if(callback != null && typeof callback !== "function"){throw new Error("task callback must be a function");}q.started = true;if(!_isArray(data)){data = [data];}if(data.length === 0 && q.idle()){return async.setImmediate(function(){q.drain();});}_arrayEach(data, function(task){var item={data:task, callback:callback || noop};if(pos){q.tasks.unshift(item);}else {q.tasks.push(item);}if(q.tasks.length === q.concurrency){q.saturated();}});async.setImmediate(q.process);}function _next(q, tasks){return function(){workers -= 1;var removed=false;var args=arguments;_arrayEach(tasks, function(task){_arrayEach(workersList, function(worker, index){if(worker === task && !removed){workersList.splice(index, 1);removed = true;}});task.callback.apply(task, args);});if(q.tasks.length + workers === 0){q.drain();}q.process();};}var workers=0;var workersList=[];var q={tasks:[], concurrency:concurrency, payload:payload, saturated:noop, empty:noop, drain:noop, started:false, paused:false, push:function push(data, callback){_insert(q, data, false, callback);}, kill:function kill(){q.drain = noop;q.tasks = [];}, unshift:function unshift(data, callback){_insert(q, data, true, callback);}, process:function process(){while(!q.paused && workers < q.concurrency && q.tasks.length) {var tasks=q.payload?q.tasks.splice(0, q.payload):q.tasks.splice(0, q.tasks.length);var data=_map(tasks, function(task){return task.data;});if(q.tasks.length === 0){q.empty();}workers += 1;workersList.push(tasks[0]);var cb=only_once(_next(q, tasks));worker(data, cb);}}, length:function length(){return q.tasks.length;}, running:function running(){return workers;}, workersList:(function(_workersList){var _workersListWrapper=function workersList(){return _workersList.apply(this, arguments);};_workersListWrapper.toString = function(){return _workersList.toString();};return _workersListWrapper;})(function(){return workersList;}), idle:function idle(){return q.tasks.length + workers === 0;}, pause:function pause(){q.paused = true;}, resume:function resume(){if(q.paused === false){return;}q.paused = false;var resumeCount=Math.min(q.concurrency, q.tasks.length);for(var w=1; w <= resumeCount; w++) {async.setImmediate(q.process);}}};return q;}async.queue = function(worker, concurrency){var q=_queue(function(items, cb){worker(items[0], cb);}, concurrency, 1);return q;};async.priorityQueue = function(worker, concurrency){function _compareTasks(a, b){return a.priority - b.priority;}function _binarySearch(sequence, item, compare){var beg=-1, end=sequence.length - 1;while(beg < end) {var mid=beg + (end - beg + 1 >>> 1);if(compare(item, sequence[mid]) >= 0){beg = mid;}else {end = mid - 1;}}return beg;}function _insert(q, data, priority, callback){if(callback != null && typeof callback !== "function"){throw new Error("task callback must be a function");}q.started = true;if(!_isArray(data)){data = [data];}if(data.length === 0){return async.setImmediate(function(){q.drain();});}_arrayEach(data, function(task){var item={data:task, priority:priority, callback:typeof callback === "function"?callback:noop};q.tasks.splice(_binarySearch(q.tasks, item, _compareTasks) + 1, 0, item);if(q.tasks.length === q.concurrency){q.saturated();}async.setImmediate(q.process);});}var q=async.queue(worker, concurrency);q.push = function(data, priority, callback){_insert(q, data, priority, callback);};delete q.unshift;return q;};async.cargo = function(worker, payload){return _queue(worker, 1, payload);};function _console_fn(name){return _restParam(function(fn, args){fn.apply(null, args.concat([_restParam(function(err, args){if(typeof console === "object"){if(err){if(console.error){console.error(err);}}else if(console[name]){_arrayEach(args, function(x){console[name](x);});}}})]));});}async.log = _console_fn("log");async.dir = _console_fn("dir");async.memoize = function(fn, hasher){var memo={};var queues={};var has=Object.prototype.hasOwnProperty;hasher = hasher || identity;var memoized=_restParam(function memoized(args){var callback=args.pop();var key=hasher.apply(null, args);if(has.call(memo, key)){async.setImmediate(function(){callback.apply(null, memo[key]);});}else if(has.call(queues, key)){queues[key].push(callback);}else {queues[key] = [callback];fn.apply(null, args.concat([_restParam(function(args){memo[key] = args;var q=queues[key];delete queues[key];for(var i=0, l=q.length; i < l; i++) {q[i].apply(null, args);}})]));}});memoized.memo = memo;memoized.unmemoized = fn;return memoized;};async.unmemoize = function(fn){return function(){return (fn.unmemoized || fn).apply(null, arguments);};};function _times(mapper){return function(count, iterator, callback){mapper(_range(count), iterator, callback);};}async.times = _times(async.map);async.timesSeries = _times(async.mapSeries);async.timesLimit = function(count, limit, iterator, callback){return async.mapLimit(_range(count), limit, iterator, callback);};async.seq = function(){var fns=arguments;return _restParam(function(args){var that=this;var callback=args[args.length - 1];if(typeof callback == "function"){args.pop();}else {callback = noop;}async.reduce(fns, args, function(newargs, fn, cb){fn.apply(that, newargs.concat([_restParam(function(err, nextargs){cb(err, nextargs);})]));}, function(err, results){callback.apply(that, [err].concat(results));});});};async.compose = function(){return async.seq.apply(null, Array.prototype.reverse.call(arguments));};function _applyEach(eachfn){return _restParam(function(fns, args){var go=_restParam(function(args){var that=this;var callback=args.pop();return eachfn(fns, function(fn, _, cb){fn.apply(that, args.concat([cb]));}, callback);});if(args.length){return go.apply(this, args);}else {return go;}});}async.applyEach = _applyEach(async.eachOf);async.applyEachSeries = _applyEach(async.eachOfSeries);async.forever = function(fn, callback){var done=only_once(callback || noop);var task=ensureAsync(fn);function next(err){if(err){return done(err);}task(next);}next();};function ensureAsync(fn){return _restParam(function(args){var callback=args.pop();args.push(function(){var innerArgs=arguments;if(sync){async.setImmediate(function(){callback.apply(null, innerArgs);});}else {callback.apply(null, innerArgs);}});var sync=true;fn.apply(this, args);sync = false;});}async.ensureAsync = ensureAsync;async.constant = _restParam(function(values){var args=[null].concat(values);return function(callback){return callback.apply(this, args);};});async.wrapSync = async.asyncify = function asyncify(func){return _restParam(function(args){var callback=args.pop();var result;try{result = func.apply(this, args);}catch(e) {return callback(e);}if(_isObject(result) && typeof result.then === "function"){result.then(function(value){callback(null, value);})["catch"](function(err){callback(err.message?err:new Error(err));});}else {callback(null, result);}});};if(typeof module === "object" && module.exports){module.exports = async;}else if(typeof define === "function" && define.amd){define([], function(){return async;});}else {root.async = async;}})();}).call(this, require("_process"), typeof global !== "undefined"?global:typeof self !== "undefined"?self:typeof window !== "undefined"?window:{});}, {_process:3}], 2:[function(require, module, exports){(function(process){function normalizeArray(parts, allowAboveRoot){var up=0;for(var i=parts.length - 1; i >= 0; i--) {var last=parts[i];if(last === "."){parts.splice(i, 1);}else if(last === ".."){parts.splice(i, 1);up++;}else if(up){parts.splice(i, 1);up--;}}if(allowAboveRoot){for(; up--; up) {parts.unshift("..");}}return parts;}var splitPathRe=/^(\/?|)([\s\S]*?)((?:\.{1,2}|[^\/]+?|)(\.[^.\/]*|))(?:[\/]*)$/;var splitPath=function splitPath(filename){return splitPathRe.exec(filename).slice(1);};exports.resolve = function(){var resolvedPath="", resolvedAbsolute=false;for(var i=arguments.length - 1; i >= -1 && !resolvedAbsolute; i--) {var path=i >= 0?arguments[i]:process.cwd();if(typeof path !== "string"){throw new TypeError("Arguments to path.resolve must be strings");}else if(!path){continue;}resolvedPath = path + "/" + resolvedPath;resolvedAbsolute = path.charAt(0) === "/";}resolvedPath = normalizeArray(filter(resolvedPath.split("/"), function(p){return !!p;}), !resolvedAbsolute).join("/");return (resolvedAbsolute?"/":"") + resolvedPath || ".";};exports.normalize = function(path){var isAbsolute=exports.isAbsolute(path), trailingSlash=substr(path, -1) === "/";path = normalizeArray(filter(path.split("/"), function(p){return !!p;}), !isAbsolute).join("/");if(!path && !isAbsolute){path = ".";}if(path && trailingSlash){path += "/";}return (isAbsolute?"/":"") + path;};exports.isAbsolute = function(path){return path.charAt(0) === "/";};exports.join = function(){var paths=Array.prototype.slice.call(arguments, 0);return exports.normalize(filter(paths, function(p, index){if(typeof p !== "string"){throw new TypeError("Arguments to path.join must be strings");}return p;}).join("/"));};exports.relative = function(from, to){from = exports.resolve(from).substr(1);to = exports.resolve(to).substr(1);function trim(arr){var start=0;for(; start < arr.length; start++) {if(arr[start] !== "")break;}var end=arr.length - 1;for(; end >= 0; end--) {if(arr[end] !== "")break;}if(start > end){return [];}return arr.slice(start, end - start + 1);}var fromParts=trim(from.split("/"));var toParts=trim(to.split("/"));var length=Math.min(fromParts.length, toParts.length);var samePartsLength=length;for(var i=0; i < length; i++) {if(fromParts[i] !== toParts[i]){samePartsLength = i;break;}}var outputParts=[];for(var i=samePartsLength; i < fromParts.length; i++) {outputParts.push("..");}outputParts = outputParts.concat(toParts.slice(samePartsLength));return outputParts.join("/");};exports.sep = "/";exports.delimiter = ":";exports.dirname = function(path){var result=splitPath(path), root=result[0], dir=result[1];if(!root && !dir){return ".";}if(dir){dir = dir.substr(0, dir.length - 1);}return root + dir;};exports.basename = function(path, ext){var f=splitPath(path)[2];if(ext && f.substr(-1 * ext.length) === ext){f = f.substr(0, f.length - ext.length);}return f;};exports.extname = function(path){return splitPath(path)[3];};function filter(xs, f){if(xs.filter){return xs.filter(f);}var res=[];for(var i=0; i < xs.length; i++) {if(f(xs[i], i, xs))res.push(xs[i]);}return res;}var substr="ab".substr(-1) === "b"?function(str, start, len){return str.substr(start, len);}:function(str, start, len){if(start < 0)start = str.length + start;return str.substr(start, len);};}).call(this, require("_process"));}, {_process:3}], 3:[function(require, module, exports){var process=module.exports = {};var queue=[];var draining=false;var currentQueue;var queueIndex=-1;function cleanUpNextTick(){draining = false;if(currentQueue.length){queue = currentQueue.concat(queue);}else {queueIndex = -1;}if(queue.length){drainQueue();}}function drainQueue(){if(draining){return;}var timeout=setTimeout(cleanUpNextTick);draining = true;var len=queue.length;while(len) {currentQueue = queue;queue = [];while(++queueIndex < len) {if(currentQueue){currentQueue[queueIndex].run();}}queueIndex = -1;len = queue.length;}currentQueue = null;draining = false;clearTimeout(timeout);}process.nextTick = function(fun){var args=new Array(arguments.length - 1);if(arguments.length > 1){for(var i=1; i < arguments.length; i++) {args[i - 1] = arguments[i];}}queue.push(new Item(fun, args));if(queue.length === 1 && !draining){setTimeout(drainQueue, 0);}};function Item(fun, array){this.fun = fun;this.array = array;}Item.prototype.run = function(){this.fun.apply(null, this.array);};process.title = "browser";process.browser = true;process.env = {};process.argv = [];process.version = "";process.versions = {};function noop(){}process.on = noop;process.addListener = noop;process.once = noop;process.off = noop;process.removeListener = noop;process.removeAllListeners = noop;process.emit = noop;process.binding = function(name){throw new Error("process.binding is not supported");};process.cwd = function(){return "/";};process.chdir = function(dir){throw new Error("process.chdir is not supported");};process.umask = function(){return 0;};}, {}], 4:[function(require, module, exports){(function(global){;(function(root){var freeExports=typeof exports == "object" && exports && !exports.nodeType && exports;var freeModule=typeof module == "object" && module && !module.nodeType && module;var freeGlobal=typeof global == "object" && global;if(freeGlobal.global === freeGlobal || freeGlobal.window === freeGlobal || freeGlobal.self === freeGlobal){root = freeGlobal;}var punycode, maxInt=2147483647, base=36, tMin=1, tMax=26, skew=38, damp=700, initialBias=72, initialN=128, delimiter="-", regexPunycode=/^xn--/, regexNonASCII=/[^\x20-\x7E]/, regexSeparators=/[\x2E\u3002\uFF0E\uFF61]/g, errors={overflow:"Overflow: input needs wider integers to process", "not-basic":"Illegal input >= 0x80 (not a basic code point)", "invalid-input":"Invalid input"}, baseMinusTMin=base - tMin, floor=Math.floor, stringFromCharCode=String.fromCharCode, key;function error(type){throw new RangeError(errors[type]);}function map(array, fn){var length=array.length;var result=[];while(length--) {result[length] = fn(array[length]);}return result;}function mapDomain(string, fn){var parts=string.split("@");var result="";if(parts.length > 1){result = parts[0] + "@";string = parts[1];}string = string.replace(regexSeparators, ".");var labels=string.split(".");var encoded=map(labels, fn).join(".");return result + encoded;}function ucs2decode(string){var output=[], counter=0, length=string.length, value, extra;while(counter < length) {value = string.charCodeAt(counter++);if(value >= 55296 && value <= 56319 && counter < length){extra = string.charCodeAt(counter++);if((extra & 64512) == 56320){output.push(((value & 1023) << 10) + (extra & 1023) + 65536);}else {output.push(value);counter--;}}else {output.push(value);}}return output;}function ucs2encode(array){return map(array, function(value){var output="";if(value > 65535){value -= 65536;output += stringFromCharCode(value >>> 10 & 1023 | 55296);value = 56320 | value & 1023;}output += stringFromCharCode(value);return output;}).join("");}function basicToDigit(codePoint){if(codePoint - 48 < 10){return codePoint - 22;}if(codePoint - 65 < 26){return codePoint - 65;}if(codePoint - 97 < 26){return codePoint - 97;}return base;}function digitToBasic(digit, flag){return digit + 22 + 75 * (digit < 26) - ((flag != 0) << 5);}function adapt(delta, numPoints, firstTime){var k=0;delta = firstTime?floor(delta / damp):delta >> 1;delta += floor(delta / numPoints);for(; delta > baseMinusTMin * tMax >> 1; k += base) {delta = floor(delta / baseMinusTMin);}return floor(k + (baseMinusTMin + 1) * delta / (delta + skew));}function decode(input){var output=[], inputLength=input.length, out, i=0, n=initialN, bias=initialBias, basic, j, index, oldi, w, k, digit, t, baseMinusT;basic = input.lastIndexOf(delimiter);if(basic < 0){basic = 0;}for(j = 0; j < basic; ++j) {if(input.charCodeAt(j) >= 128){error("not-basic");}output.push(input.charCodeAt(j));}for(index = basic > 0?basic + 1:0; index < inputLength;) {for(oldi = i, w = 1, k = base;; k += base) {if(index >= inputLength){error("invalid-input");}digit = basicToDigit(input.charCodeAt(index++));if(digit >= base || digit > floor((maxInt - i) / w)){error("overflow");}i += digit * w;t = k <= bias?tMin:k >= bias + tMax?tMax:k - bias;if(digit < t){break;}baseMinusT = base - t;if(w > floor(maxInt / baseMinusT)){error("overflow");}w *= baseMinusT;}out = output.length + 1;bias = adapt(i - oldi, out, oldi == 0);if(floor(i / out) > maxInt - n){error("overflow");}n += floor(i / out);i %= out;output.splice(i++, 0, n);}return ucs2encode(output);}function encode(input){var n, delta, handledCPCount, basicLength, bias, j, m, q, k, t, currentValue, output=[], inputLength, handledCPCountPlusOne, baseMinusT, qMinusT;input = ucs2decode(input);inputLength = input.length;n = initialN;delta = 0;bias = initialBias;for(j = 0; j < inputLength; ++j) {currentValue = input[j];if(currentValue < 128){output.push(stringFromCharCode(currentValue));}}handledCPCount = basicLength = output.length;if(basicLength){output.push(delimiter);}while(handledCPCount < inputLength) {for(m = maxInt, j = 0; j < inputLength; ++j) {currentValue = input[j];if(currentValue >= n && currentValue < m){m = currentValue;}}handledCPCountPlusOne = handledCPCount + 1;if(m - n > floor((maxInt - delta) / handledCPCountPlusOne)){error("overflow");}delta += (m - n) * handledCPCountPlusOne;n = m;for(j = 0; j < inputLength; ++j) {currentValue = input[j];if(currentValue < n && ++delta > maxInt){error("overflow");}if(currentValue == n){for(q = delta, k = base;; k += base) {t = k <= bias?tMin:k >= bias + tMax?tMax:k - bias;if(q < t){break;}qMinusT = q - t;baseMinusT = base - t;output.push(stringFromCharCode(digitToBasic(t + qMinusT % baseMinusT, 0)));q = floor(qMinusT / baseMinusT);}output.push(stringFromCharCode(digitToBasic(q, 0)));bias = adapt(delta, handledCPCountPlusOne, handledCPCount == basicLength);delta = 0;++handledCPCount;}}++delta;++n;}return output.join("");}function toUnicode(input){return mapDomain(input, function(string){return regexPunycode.test(string)?decode(string.slice(4).toLowerCase()):string;});}function toASCII(input){return mapDomain(input, function(string){return regexNonASCII.test(string)?"xn--" + encode(string):string;});}punycode = {version:"1.3.2", ucs2:{decode:ucs2decode, encode:ucs2encode}, decode:decode, encode:encode, toASCII:toASCII, toUnicode:toUnicode};if(typeof define == "function" && typeof define.amd == "object" && define.amd){define("punycode", function(){return punycode;});}else if(freeExports && freeModule){if(module.exports == freeExports){freeModule.exports = punycode;}else {for(key in punycode) {punycode.hasOwnProperty(key) && (freeExports[key] = punycode[key]);}}}else {root.punycode = punycode;}})(this);}).call(this, typeof global !== "undefined"?global:typeof self !== "undefined"?self:typeof window !== "undefined"?window:{});}, {}], 5:[function(require, module, exports){"use strict";function hasOwnProperty(obj, prop){return Object.prototype.hasOwnProperty.call(obj, prop);}module.exports = function(qs, sep, eq, options){sep = sep || "&";eq = eq || "=";var obj={};if(typeof qs !== "string" || qs.length === 0){return obj;}var regexp=/\+/g;qs = qs.split(sep);var maxKeys=1000;if(options && typeof options.maxKeys === "number"){maxKeys = options.maxKeys;}var len=qs.length;if(maxKeys > 0 && len > maxKeys){len = maxKeys;}for(var i=0; i < len; ++i) {var x=qs[i].replace(regexp, "%20"), idx=x.indexOf(eq), kstr, vstr, k, v;if(idx >= 0){kstr = x.substr(0, idx);vstr = x.substr(idx + 1);}else {kstr = x;vstr = "";}k = decodeURIComponent(kstr);v = decodeURIComponent(vstr);if(!hasOwnProperty(obj, k)){obj[k] = v;}else if(isArray(obj[k])){obj[k].push(v);}else {obj[k] = [obj[k], v];}}return obj;};var isArray=Array.isArray || function(xs){return Object.prototype.toString.call(xs) === "[object Array]";};}, {}], 6:[function(require, module, exports){"use strict";var stringifyPrimitive=function stringifyPrimitive(v){switch(typeof v){case "string":return v;case "boolean":return v?"true":"false";case "number":return isFinite(v)?v:"";default:return "";}};module.exports = function(obj, sep, eq, name){sep = sep || "&";eq = eq || "=";if(obj === null){obj = undefined;}if(typeof obj === "object"){return map(objectKeys(obj), function(k){var ks=encodeURIComponent(stringifyPrimitive(k)) + eq;if(isArray(obj[k])){return map(obj[k], function(v){return ks + encodeURIComponent(stringifyPrimitive(v));}).join(sep);}else {return ks + encodeURIComponent(stringifyPrimitive(obj[k]));}}).join(sep);}if(!name)return "";return encodeURIComponent(stringifyPrimitive(name)) + eq + encodeURIComponent(stringifyPrimitive(obj));};var isArray=Array.isArray || function(xs){return Object.prototype.toString.call(xs) === "[object Array]";};function map(xs, f){if(xs.map){return xs.map(f);}var res=[];for(var i=0; i < xs.length; i++) {res.push(f(xs[i], i));}return res;}var objectKeys=Object.keys || function(obj){var res=[];for(var key in obj) {if(Object.prototype.hasOwnProperty.call(obj, key))res.push(key);}return res;};}, {}], 7:[function(require, module, exports){"use strict";exports.decode = exports.parse = require("./decode");exports.encode = exports.stringify = require("./encode");}, {"./decode":5, "./encode":6}], 8:[function(require, module, exports){var punycode=require("punycode");exports.parse = urlParse;exports.resolve = urlResolve;exports.resolveObject = urlResolveObject;exports.format = urlFormat;exports.Url = Url;function Url(){this.protocol = null;this.slashes = null;this.auth = null;this.host = null;this.port = null;this.hostname = null;this.hash = null;this.search = null;this.query = null;this.pathname = null;this.path = null;this.href = null;}var protocolPattern=/^([a-z0-9.+-]+:)/i, portPattern=/:[0-9]*$/, delims=["<", ">", "\"", "`", " ", "\r", "\n", "\t"], unwise=["{", "}", "|", "\\", "^", "`"].concat(delims), autoEscape=["'"].concat(unwise), nonHostChars=["%", "/", "?", ";", "#"].concat(autoEscape), hostEndingChars=["/", "?", "#"], hostnameMaxLen=255, hostnamePartPattern=/^[a-z0-9A-Z_-]{0,63}$/, hostnamePartStart=/^([a-z0-9A-Z_-]{0,63})(.*)$/, unsafeProtocol={javascript:true, "javascript:":true}, hostlessProtocol={javascript:true, "javascript:":true}, slashedProtocol={http:true, https:true, ftp:true, gopher:true, file:true, "http:":true, "https:":true, "ftp:":true, "gopher:":true, "file:":true}, querystring=require("querystring");function urlParse(url, parseQueryString, slashesDenoteHost){if(url && isObject(url) && url instanceof Url){return url;}var u=new Url();u.parse(url, parseQueryString, slashesDenoteHost);return u;}Url.prototype.parse = function(url, parseQueryString, slashesDenoteHost){if(!isString(url)){throw new TypeError("Parameter 'url' must be a string, not " + typeof url);}var rest=url;rest = rest.trim();var proto=protocolPattern.exec(rest);if(proto){proto = proto[0];var lowerProto=proto.toLowerCase();this.protocol = lowerProto;rest = rest.substr(proto.length);}if(slashesDenoteHost || proto || rest.match(/^\/\/[^@\/]+@[^@\/]+/)){var slashes=rest.substr(0, 2) === "//";if(slashes && !(proto && hostlessProtocol[proto])){rest = rest.substr(2);this.slashes = true;}}if(!hostlessProtocol[proto] && (slashes || proto && !slashedProtocol[proto])){var hostEnd=-1;for(var i=0; i < hostEndingChars.length; i++) {var hec=rest.indexOf(hostEndingChars[i]);if(hec !== -1 && (hostEnd === -1 || hec < hostEnd))hostEnd = hec;}var auth, atSign;if(hostEnd === -1){atSign = rest.lastIndexOf("@");}else {atSign = rest.lastIndexOf("@", hostEnd);}if(atSign !== -1){auth = rest.slice(0, atSign);rest = rest.slice(atSign + 1);this.auth = decodeURIComponent(auth);}hostEnd = -1;for(var i=0; i < nonHostChars.length; i++) {var hec=rest.indexOf(nonHostChars[i]);if(hec !== -1 && (hostEnd === -1 || hec < hostEnd))hostEnd = hec;}if(hostEnd === -1)hostEnd = rest.length;this.host = rest.slice(0, hostEnd);rest = rest.slice(hostEnd);this.parseHost();this.hostname = this.hostname || "";var ipv6Hostname=this.hostname[0] === "[" && this.hostname[this.hostname.length - 1] === "]";if(!ipv6Hostname){var hostparts=this.hostname.split(/\./);for(var i=0, l=hostparts.length; i < l; i++) {var part=hostparts[i];if(!part)continue;if(!part.match(hostnamePartPattern)){var newpart="";for(var j=0, k=part.length; j < k; j++) {if(part.charCodeAt(j) > 127){newpart += "x";}else {newpart += part[j];}}if(!newpart.match(hostnamePartPattern)){var validParts=hostparts.slice(0, i);var notHost=hostparts.slice(i + 1);var bit=part.match(hostnamePartStart);if(bit){validParts.push(bit[1]);notHost.unshift(bit[2]);}if(notHost.length){rest = "/" + notHost.join(".") + rest;}this.hostname = validParts.join(".");break;}}}}if(this.hostname.length > hostnameMaxLen){this.hostname = "";}else {this.hostname = this.hostname.toLowerCase();}if(!ipv6Hostname){var domainArray=this.hostname.split(".");var newOut=[];for(var i=0; i < domainArray.length; ++i) {var s=domainArray[i];newOut.push(s.match(/[^A-Za-z0-9_-]/)?"xn--" + punycode.encode(s):s);}this.hostname = newOut.join(".");}var p=this.port?":" + this.port:"";var h=this.hostname || "";this.host = h + p;this.href += this.host;if(ipv6Hostname){this.hostname = this.hostname.substr(1, this.hostname.length - 2);if(rest[0] !== "/"){rest = "/" + rest;}}}if(!unsafeProtocol[lowerProto]){for(var i=0, l=autoEscape.length; i < l; i++) {var ae=autoEscape[i];var esc=encodeURIComponent(ae);if(esc === ae){esc = escape(ae);}rest = rest.split(ae).join(esc);}}var hash=rest.indexOf("#");if(hash !== -1){this.hash = rest.substr(hash);rest = rest.slice(0, hash);}var qm=rest.indexOf("?");if(qm !== -1){this.search = rest.substr(qm);this.query = rest.substr(qm + 1);if(parseQueryString){this.query = querystring.parse(this.query);}rest = rest.slice(0, qm);}else if(parseQueryString){this.search = "";this.query = {};}if(rest)this.pathname = rest;if(slashedProtocol[lowerProto] && this.hostname && !this.pathname){this.pathname = "/";}if(this.pathname || this.search){var p=this.pathname || "";var s=this.search || "";this.path = p + s;}this.href = this.format();return this;};function urlFormat(obj){if(isString(obj))obj = urlParse(obj);if(!(obj instanceof Url)){return Url.prototype.format.call(obj);}return obj.format();}Url.prototype.format = function(){var auth=this.auth || "";if(auth){auth = encodeURIComponent(auth);auth = auth.replace(/%3A/i, ":");auth += "@";}var protocol=this.protocol || "", pathname=this.pathname || "", hash=this.hash || "", host=false, query="";if(this.host){host = auth + this.host;}else if(this.hostname){host = auth + (this.hostname.indexOf(":") === -1?this.hostname:"[" + this.hostname + "]");if(this.port){host += ":" + this.port;}}if(this.query && isObject(this.query) && Object.keys(this.query).length){query = querystring.stringify(this.query);}var search=this.search || query && "?" + query || "";if(protocol && protocol.substr(-1) !== ":")protocol += ":";if(this.slashes || (!protocol || slashedProtocol[protocol]) && host !== false){host = "//" + (host || "");if(pathname && pathname.charAt(0) !== "/")pathname = "/" + pathname;}else if(!host){host = "";}if(hash && hash.charAt(0) !== "#")hash = "#" + hash;if(search && search.charAt(0) !== "?")search = "?" + search;pathname = pathname.replace(/[?#]/g, function(match){return encodeURIComponent(match);});search = search.replace("#", "%23");return protocol + host + pathname + search + hash;};function urlResolve(source, relative){return urlParse(source, false, true).resolve(relative);}Url.prototype.resolve = function(relative){return this.resolveObject(urlParse(relative, false, true)).format();};function urlResolveObject(source, relative){if(!source){return relative;}return urlParse(source, false, true).resolveObject(relative);}Url.prototype.resolveObject = function(relative){if(isString(relative)){var rel=new Url();rel.parse(relative, false, true);relative = rel;}var result=new Url();Object.keys(this).forEach(function(k){result[k] = this[k];}, this);result.hash = relative.hash;if(relative.href === ""){result.href = result.format();return result;}if(relative.slashes && !relative.protocol){Object.keys(relative).forEach(function(k){if(k !== "protocol")result[k] = relative[k];});if(slashedProtocol[result.protocol] && result.hostname && !result.pathname){result.path = result.pathname = "/";}result.href = result.format();return result;}if(relative.protocol && relative.protocol !== result.protocol){if(!slashedProtocol[relative.protocol]){Object.keys(relative).forEach(function(k){result[k] = relative[k];});result.href = result.format();return result;}result.protocol = relative.protocol;if(!relative.host && !hostlessProtocol[relative.protocol]){var relPath=(relative.pathname || "").split("/");while(relPath.length && !(relative.host = relPath.shift()));if(!relative.host)relative.host = "";if(!relative.hostname)relative.hostname = "";if(relPath[0] !== "")relPath.unshift("");if(relPath.length < 2)relPath.unshift("");result.pathname = relPath.join("/");}else {result.pathname = relative.pathname;}result.search = relative.search;result.query = relative.query;result.host = relative.host || "";result.auth = relative.auth;result.hostname = relative.hostname || relative.host;result.port = relative.port;if(result.pathname || result.search){var p=result.pathname || "";var s=result.search || "";result.path = p + s;}result.slashes = result.slashes || relative.slashes;result.href = result.format();return result;}var isSourceAbs=result.pathname && result.pathname.charAt(0) === "/", isRelAbs=relative.host || relative.pathname && relative.pathname.charAt(0) === "/", mustEndAbs=isRelAbs || isSourceAbs || result.host && relative.pathname, removeAllDots=mustEndAbs, srcPath=result.pathname && result.pathname.split("/") || [], relPath=relative.pathname && relative.pathname.split("/") || [], psychotic=result.protocol && !slashedProtocol[result.protocol];if(psychotic){result.hostname = "";result.port = null;if(result.host){if(srcPath[0] === "")srcPath[0] = result.host;else srcPath.unshift(result.host);}result.host = "";if(relative.protocol){relative.hostname = null;relative.port = null;if(relative.host){if(relPath[0] === "")relPath[0] = relative.host;else relPath.unshift(relative.host);}relative.host = null;}mustEndAbs = mustEndAbs && (relPath[0] === "" || srcPath[0] === "");}if(isRelAbs){result.host = relative.host || relative.host === ""?relative.host:result.host;result.hostname = relative.hostname || relative.hostname === ""?relative.hostname:result.hostname;result.search = relative.search;result.query = relative.query;srcPath = relPath;}else if(relPath.length){if(!srcPath)srcPath = [];srcPath.pop();srcPath = srcPath.concat(relPath);result.search = relative.search;result.query = relative.query;}else if(!isNullOrUndefined(relative.search)){if(psychotic){result.hostname = result.host = srcPath.shift();var authInHost=result.host && result.host.indexOf("@") > 0?result.host.split("@"):false;if(authInHost){result.auth = authInHost.shift();result.host = result.hostname = authInHost.shift();}}result.search = relative.search;result.query = relative.query;if(!isNull(result.pathname) || !isNull(result.search)){result.path = (result.pathname?result.pathname:"") + (result.search?result.search:"");}result.href = result.format();return result;}if(!srcPath.length){result.pathname = null;if(result.search){result.path = "/" + result.search;}else {result.path = null;}result.href = result.format();return result;}var last=srcPath.slice(-1)[0];var hasTrailingSlash=(result.host || relative.host) && (last === "." || last === "..") || last === "";var up=0;for(var i=srcPath.length; i >= 0; i--) {last = srcPath[i];if(last == "."){srcPath.splice(i, 1);}else if(last === ".."){srcPath.splice(i, 1);up++;}else if(up){srcPath.splice(i, 1);up--;}}if(!mustEndAbs && !removeAllDots){for(; up--; up) {srcPath.unshift("..");}}if(mustEndAbs && srcPath[0] !== "" && (!srcPath[0] || srcPath[0].charAt(0) !== "/")){srcPath.unshift("");}if(hasTrailingSlash && srcPath.join("/").substr(-1) !== "/"){srcPath.push("");}var isAbsolute=srcPath[0] === "" || srcPath[0] && srcPath[0].charAt(0) === "/";if(psychotic){result.hostname = result.host = isAbsolute?"":srcPath.length?srcPath.shift():"";var authInHost=result.host && result.host.indexOf("@") > 0?result.host.split("@"):false;if(authInHost){result.auth = authInHost.shift();result.host = result.hostname = authInHost.shift();}}mustEndAbs = mustEndAbs || result.host && srcPath.length;if(mustEndAbs && !isAbsolute){srcPath.unshift("");}if(!srcPath.length){result.pathname = null;result.path = null;}else {result.pathname = srcPath.join("/");}if(!isNull(result.pathname) || !isNull(result.search)){result.path = (result.pathname?result.pathname:"") + (result.search?result.search:"");}result.auth = relative.auth || result.auth;result.slashes = result.slashes || relative.slashes;result.href = result.format();return result;};Url.prototype.parseHost = function(){var host=this.host;var port=portPattern.exec(host);if(port){port = port[0];if(port !== ":"){this.port = port.substr(1);}host = host.substr(0, host.length - port.length);}if(host)this.hostname = host;};function isString(arg){return typeof arg === "string";}function isObject(arg){return typeof arg === "object" && arg !== null;}function isNull(arg){return arg === null;}function isNullOrUndefined(arg){return arg == null;}}, {punycode:4, querystring:7}], 9:[function(require, module, exports){"use strict";module.exports = earcut;function earcut(data, holeIndices, dim){dim = dim || 2;var hasHoles=holeIndices && holeIndices.length, outerLen=hasHoles?holeIndices[0] * dim:data.length, outerNode=linkedList(data, 0, outerLen, dim, true), triangles=[];if(!outerNode){return triangles;}var minX, minY, maxX, maxY, x, y, size;if(hasHoles)outerNode = eliminateHoles(data, holeIndices, outerNode, dim);if(data.length > 80 * dim){minX = maxX = data[0];minY = maxY = data[1];for(var i=dim; i < outerLen; i += dim) {x = data[i];y = data[i + 1];if(x < minX)minX = x;if(y < minY)minY = y;if(x > maxX)maxX = x;if(y > maxY)maxY = y;}size = Math.max(maxX - minX, maxY - minY);}earcutLinked(outerNode, triangles, dim, minX, minY, size);return triangles;}function linkedList(data, start, end, dim, clockwise){var sum=0, i, j, last;for(i = start, j = end - dim; i < end; i += dim) {sum += (data[j] - data[i]) * (data[i + 1] + data[j + 1]);j = i;}if(clockwise === sum > 0){for(i = start; i < end; i += dim) last = insertNode(i, data[i], data[i + 1], last);}else {for(i = end - dim; i >= start; i -= dim) last = insertNode(i, data[i], data[i + 1], last);}return last;}function filterPoints(start, end){if(!start){return start;}if(!end)end = start;var p=start, again;do{again = false;if(!p.steiner && (equals(p, p.next) || area(p.prev, p, p.next) === 0)){removeNode(p);p = end = p.prev;if(p === p.next){return null;}again = true;}else {p = p.next;}}while(again || p !== end);return end;}function earcutLinked(ear, triangles, dim, minX, minY, size, pass){if(!ear){return;}if(!pass && size)indexCurve(ear, minX, minY, size);var stop=ear, prev, next;while(ear.prev !== ear.next) {prev = ear.prev;next = ear.next;if(size?isEarHashed(ear, minX, minY, size):isEar(ear)){triangles.push(prev.i / dim);triangles.push(ear.i / dim);triangles.push(next.i / dim);removeNode(ear);ear = next.next;stop = next.next;continue;}ear = next;if(ear === stop){if(!pass){earcutLinked(filterPoints(ear), triangles, dim, minX, minY, size, 1);}else if(pass === 1){ear = cureLocalIntersections(ear, triangles, dim);earcutLinked(ear, triangles, dim, minX, minY, size, 2);}else if(pass === 2){splitEarcut(ear, triangles, dim, minX, minY, size);}break;}}}function isEar(ear){var a=ear.prev, b=ear, c=ear.next;if(area(a, b, c) >= 0){return false;}var p=ear.next.next;while(p !== ear.prev) {if(pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, p.x, p.y) && area(p.prev, p, p.next) >= 0){return false;}p = p.next;}return true;}function isEarHashed(ear, minX, minY, size){var a=ear.prev, b=ear, c=ear.next;if(area(a, b, c) >= 0){return false;}var minTX=a.x < b.x?a.x < c.x?a.x:c.x:b.x < c.x?b.x:c.x, minTY=a.y < b.y?a.y < c.y?a.y:c.y:b.y < c.y?b.y:c.y, maxTX=a.x > b.x?a.x > c.x?a.x:c.x:b.x > c.x?b.x:c.x, maxTY=a.y > b.y?a.y > c.y?a.y:c.y:b.y > c.y?b.y:c.y;var minZ=zOrder(minTX, minTY, minX, minY, size), maxZ=zOrder(maxTX, maxTY, minX, minY, size);var p=ear.nextZ;while(p && p.z <= maxZ) {if(p !== ear.prev && p !== ear.next && pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, p.x, p.y) && area(p.prev, p, p.next) >= 0){return false;}p = p.nextZ;}p = ear.prevZ;while(p && p.z >= minZ) {if(p !== ear.prev && p !== ear.next && pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, p.x, p.y) && area(p.prev, p, p.next) >= 0){return false;}p = p.prevZ;}return true;}function cureLocalIntersections(start, triangles, dim){var p=start;do{var a=p.prev, b=p.next.next;if(intersects(a, p, p.next, b) && locallyInside(a, b) && locallyInside(b, a)){triangles.push(a.i / dim);triangles.push(p.i / dim);triangles.push(b.i / dim);removeNode(p);removeNode(p.next);p = start = b;}p = p.next;}while(p !== start);return p;}function splitEarcut(start, triangles, dim, minX, minY, size){var a=start;do{var b=a.next.next;while(b !== a.prev) {if(a.i !== b.i && isValidDiagonal(a, b)){var c=splitPolygon(a, b);a = filterPoints(a, a.next);c = filterPoints(c, c.next);earcutLinked(a, triangles, dim, minX, minY, size);earcutLinked(c, triangles, dim, minX, minY, size);return;}b = b.next;}a = a.next;}while(a !== start);}function eliminateHoles(data, holeIndices, outerNode, dim){var queue=[], i, len, start, end, list;for(i = 0, len = holeIndices.length; i < len; i++) {start = holeIndices[i] * dim;end = i < len - 1?holeIndices[i + 1] * dim:data.length;list = linkedList(data, start, end, dim, false);if(list === list.next)list.steiner = true;queue.push(getLeftmost(list));}queue.sort(compareX);for(i = 0; i < queue.length; i++) {eliminateHole(queue[i], outerNode);outerNode = filterPoints(outerNode, outerNode.next);}return outerNode;}function compareX(a, b){return a.x - b.x;}function eliminateHole(hole, outerNode){outerNode = findHoleBridge(hole, outerNode);if(outerNode){var b=splitPolygon(outerNode, hole);filterPoints(b, b.next);}}function findHoleBridge(hole, outerNode){var p=outerNode, hx=hole.x, hy=hole.y, qx=-Infinity, m;do{if(hy <= p.y && hy >= p.next.y){var x=p.x + (hy - p.y) * (p.next.x - p.x) / (p.next.y - p.y);if(x <= hx && x > qx){qx = x;m = p.x < p.next.x?p:p.next;}}p = p.next;}while(p !== outerNode);if(!m){return null;}if(hole.x === m.x){return m.prev;}var stop=m, tanMin=Infinity, tan;p = m.next;while(p !== stop) {if(hx >= p.x && p.x >= m.x && pointInTriangle(hy < m.y?hx:qx, hy, m.x, m.y, hy < m.y?qx:hx, hy, p.x, p.y)){tan = Math.abs(hy - p.y) / (hx - p.x);if((tan < tanMin || tan === tanMin && p.x > m.x) && locallyInside(p, hole)){m = p;tanMin = tan;}}p = p.next;}return m;}function indexCurve(start, minX, minY, size){var p=start;do{if(p.z === null)p.z = zOrder(p.x, p.y, minX, minY, size);p.prevZ = p.prev;p.nextZ = p.next;p = p.next;}while(p !== start);p.prevZ.nextZ = null;p.prevZ = null;sortLinked(p);}function sortLinked(list){var i, p, q, e, tail, numMerges, pSize, qSize, inSize=1;do{p = list;list = null;tail = null;numMerges = 0;while(p) {numMerges++;q = p;pSize = 0;for(i = 0; i < inSize; i++) {pSize++;q = q.nextZ;if(!q)break;}qSize = inSize;while(pSize > 0 || qSize > 0 && q) {if(pSize === 0){e = q;q = q.nextZ;qSize--;}else if(qSize === 0 || !q){e = p;p = p.nextZ;pSize--;}else if(p.z <= q.z){e = p;p = p.nextZ;pSize--;}else {e = q;q = q.nextZ;qSize--;}if(tail)tail.nextZ = e;else list = e;e.prevZ = tail;tail = e;}p = q;}tail.nextZ = null;inSize *= 2;}while(numMerges > 1);return list;}function zOrder(x, y, minX, minY, size){x = 32767 * (x - minX) / size;y = 32767 * (y - minY) / size;x = (x | x << 8) & 16711935;x = (x | x << 4) & 252645135;x = (x | x << 2) & 858993459;x = (x | x << 1) & 1431655765;y = (y | y << 8) & 16711935;y = (y | y << 4) & 252645135;y = (y | y << 2) & 858993459;y = (y | y << 1) & 1431655765;return x | y << 1;}function getLeftmost(start){var p=start, leftmost=start;do{if(p.x < leftmost.x)leftmost = p;p = p.next;}while(p !== start);return leftmost;}function pointInTriangle(ax, ay, bx, by, cx, cy, px, py){return (cx - px) * (ay - py) - (ax - px) * (cy - py) >= 0 && (ax - px) * (by - py) - (bx - px) * (ay - py) >= 0 && (bx - px) * (cy - py) - (cx - px) * (by - py) >= 0;}function isValidDiagonal(a, b){return equals(a, b) || a.next.i !== b.i && a.prev.i !== b.i && !intersectsPolygon(a, b) && locallyInside(a, b) && locallyInside(b, a) && middleInside(a, b);}function area(p, q, r){return (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y);}function equals(p1, p2){return p1.x === p2.x && p1.y === p2.y;}function intersects(p1, q1, p2, q2){return area(p1, q1, p2) > 0 !== area(p1, q1, q2) > 0 && area(p2, q2, p1) > 0 !== area(p2, q2, q1) > 0;}function intersectsPolygon(a, b){var p=a;do{if(p.i !== a.i && p.next.i !== a.i && p.i !== b.i && p.next.i !== b.i && intersects(p, p.next, a, b)){return true;}p = p.next;}while(p !== a);return false;}function locallyInside(a, b){return area(a.prev, a, a.next) < 0?area(a, b, a.next) >= 0 && area(a, a.prev, b) >= 0:area(a, b, a.prev) < 0 || area(a, a.next, b) < 0;}function middleInside(a, b){var p=a, inside=false, px=(a.x + b.x) / 2, py=(a.y + b.y) / 2;do{if(p.y > py !== p.next.y > py && px < (p.next.x - p.x) * (py - p.y) / (p.next.y - p.y) + p.x)inside = !inside;p = p.next;}while(p !== a);return inside;}function splitPolygon(a, b){var a2=new Node(a.i, a.x, a.y), b2=new Node(b.i, b.x, b.y), an=a.next, bp=b.prev;a.next = b;b.prev = a;a2.next = an;an.prev = a2;b2.next = a2;a2.prev = b2;bp.next = b2;b2.prev = bp;return b2;}function insertNode(i, x, y, last){var p=new Node(i, x, y);if(!last){p.prev = p;p.next = p;}else {p.next = last.next;p.prev = last;last.next.prev = p;last.next = p;}return p;}function removeNode(p){p.next.prev = p.prev;p.prev.next = p.next;if(p.prevZ)p.prevZ.nextZ = p.nextZ;if(p.nextZ)p.nextZ.prevZ = p.prevZ;}function Node(i, x, y){this.i = i;this.x = x;this.y = y;this.prev = null;this.next = null;this.z = null;this.prevZ = null;this.nextZ = null;this.steiner = false;}}, {}], 10:[function(require, module, exports){"use strict";var prefix=typeof Object.create !== "function"?"~":false;function EE(fn, context, once){this.fn = fn;this.context = context;this.once = once || false;}function EventEmitter(){}EventEmitter.prototype._events = undefined;EventEmitter.prototype.listeners = function listeners(event, exists){var evt=prefix?prefix + event:event, available=this._events && this._events[evt];if(exists){return !!available;}if(!available){return [];}if(available.fn){return [available.fn];}for(var i=0, l=available.length, ee=new Array(l); i < l; i++) {ee[i] = available[i].fn;}return ee;};EventEmitter.prototype.emit = function emit(event, a1, a2, a3, a4, a5){var evt=prefix?prefix + event:event;if(!this._events || !this._events[evt]){return false;}var listeners=this._events[evt], len=arguments.length, args, i;if("function" === typeof listeners.fn){if(listeners.once)this.removeListener(event, listeners.fn, undefined, true);switch(len){case 1:return (listeners.fn.call(listeners.context), true);case 2:return (listeners.fn.call(listeners.context, a1), true);case 3:return (listeners.fn.call(listeners.context, a1, a2), true);case 4:return (listeners.fn.call(listeners.context, a1, a2, a3), true);case 5:return (listeners.fn.call(listeners.context, a1, a2, a3, a4), true);case 6:return (listeners.fn.call(listeners.context, a1, a2, a3, a4, a5), true);}for(i = 1, args = new Array(len - 1); i < len; i++) {args[i - 1] = arguments[i];}listeners.fn.apply(listeners.context, args);}else {var length=listeners.length, j;for(i = 0; i < length; i++) {if(listeners[i].once)this.removeListener(event, listeners[i].fn, undefined, true);switch(len){case 1:listeners[i].fn.call(listeners[i].context);break;case 2:listeners[i].fn.call(listeners[i].context, a1);break;case 3:listeners[i].fn.call(listeners[i].context, a1, a2);break;default:if(!args)for(j = 1, args = new Array(len - 1); j < len; j++) {args[j - 1] = arguments[j];}listeners[i].fn.apply(listeners[i].context, args);}}}return true;};EventEmitter.prototype.on = function on(event, fn, context){var listener=new EE(fn, context || this), evt=prefix?prefix + event:event;if(!this._events)this._events = prefix?{}:Object.create(null);if(!this._events[evt])this._events[evt] = listener;else {if(!this._events[evt].fn)this._events[evt].push(listener);else this._events[evt] = [this._events[evt], listener];}return this;};EventEmitter.prototype.once = function once(event, fn, context){var listener=new EE(fn, context || this, true), evt=prefix?prefix + event:event;if(!this._events)this._events = prefix?{}:Object.create(null);if(!this._events[evt])this._events[evt] = listener;else {if(!this._events[evt].fn)this._events[evt].push(listener);else this._events[evt] = [this._events[evt], listener];}return this;};EventEmitter.prototype.removeListener = function removeListener(event, fn, context, once){var evt=prefix?prefix + event:event;if(!this._events || !this._events[evt]){return this;}var listeners=this._events[evt], events=[];if(fn){if(listeners.fn){if(listeners.fn !== fn || once && !listeners.once || context && listeners.context !== context){events.push(listeners);}}else {for(var i=0, length=listeners.length; i < length; i++) {if(listeners[i].fn !== fn || once && !listeners[i].once || context && listeners[i].context !== context){events.push(listeners[i]);}}}}if(events.length){this._events[evt] = events.length === 1?events[0]:events;}else {delete this._events[evt];}return this;};EventEmitter.prototype.removeAllListeners = function removeAllListeners(event){if(!this._events){return this;}if(event)delete this._events[prefix?prefix + event:event];else this._events = prefix?{}:Object.create(null);return this;};EventEmitter.prototype.off = EventEmitter.prototype.removeListener;EventEmitter.prototype.addListener = EventEmitter.prototype.on;EventEmitter.prototype.setMaxListeners = function setMaxListeners(){return this;};EventEmitter.prefixed = prefix;if("undefined" !== typeof module){module.exports = EventEmitter;}}, {}], 11:[function(require, module, exports){"use strict";var hasOwnProperty=Object.prototype.hasOwnProperty;var propIsEnumerable=Object.prototype.propertyIsEnumerable;function toObject(val){if(val === null || val === undefined){throw new TypeError("Object.assign cannot be called with null or undefined");}return Object(val);}module.exports = Object.assign || function(target, source){var from;var to=toObject(target);var symbols;for(var s=1; s < arguments.length; s++) {from = Object(arguments[s]);for(var key in from) {if(hasOwnProperty.call(from, key)){to[key] = from[key];}}if(Object.getOwnPropertySymbols){symbols = Object.getOwnPropertySymbols(from);for(var i=0; i < symbols.length; i++) {if(propIsEnumerable.call(from, symbols[i])){to[symbols[i]] = from[symbols[i]];}}}}return to;};}, {}], 12:[function(require, module, exports){(function(process){(function(){var async={};var root, previous_async;root = this;if(root != null){previous_async = root.async;}async.noConflict = function(){root.async = previous_async;return async;};function only_once(fn){var called=false;return function(){if(called)throw new Error("Callback was already called.");called = true;fn.apply(root, arguments);};}var _toString=Object.prototype.toString;var _isArray=Array.isArray || function(obj){return _toString.call(obj) === "[object Array]";};var _each=function _each(arr, iterator){for(var i=0; i < arr.length; i += 1) {iterator(arr[i], i, arr);}};var _map=function _map(arr, iterator){if(arr.map){return arr.map(iterator);}var results=[];_each(arr, function(x, i, a){results.push(iterator(x, i, a));});return results;};var _reduce=function _reduce(arr, iterator, memo){if(arr.reduce){return arr.reduce(iterator, memo);}_each(arr, function(x, i, a){memo = iterator(memo, x, i, a);});return memo;};var _keys=function _keys(obj){if(Object.keys){return Object.keys(obj);}var keys=[];for(var k in obj) {if(obj.hasOwnProperty(k)){keys.push(k);}}return keys;};if(typeof process === "undefined" || !process.nextTick){if(typeof setImmediate === "function"){async.nextTick = function(fn){setImmediate(fn);};async.setImmediate = async.nextTick;}else {async.nextTick = function(fn){setTimeout(fn, 0);};async.setImmediate = async.nextTick;}}else {async.nextTick = process.nextTick;if(typeof setImmediate !== "undefined"){async.setImmediate = function(fn){setImmediate(fn);};}else {async.setImmediate = async.nextTick;}}async.each = function(arr, iterator, callback){callback = callback || function(){};if(!arr.length){return callback();}var completed=0;_each(arr, function(x){iterator(x, only_once(done));});function done(err){if(err){callback(err);callback = function(){};}else {completed += 1;if(completed >= arr.length){callback();}}}};async.forEach = async.each;async.eachSeries = function(arr, iterator, callback){callback = callback || function(){};if(!arr.length){return callback();}var completed=0;var iterate=(function(_iterate){var _iterateWrapper=function iterate(){return _iterate.apply(this, arguments);};_iterateWrapper.toString = function(){return _iterate.toString();};return _iterateWrapper;})(function(){iterator(arr[completed], function(err){if(err){callback(err);callback = function(){};}else {completed += 1;if(completed >= arr.length){callback();}else {iterate();}}});});iterate();};async.forEachSeries = async.eachSeries;async.eachLimit = function(arr, limit, iterator, callback){var fn=_eachLimit(limit);fn.apply(null, [arr, iterator, callback]);};async.forEachLimit = async.eachLimit;var _eachLimit=function _eachLimit(limit){return function(arr, iterator, callback){callback = callback || function(){};if(!arr.length || limit <= 0){return callback();}var completed=0;var started=0;var running=0;(function replenish(){if(completed >= arr.length){return callback();}while(running < limit && started < arr.length) {started += 1;running += 1;iterator(arr[started - 1], function(err){if(err){callback(err);callback = function(){};}else {completed += 1;running -= 1;if(completed >= arr.length){callback();}else {replenish();}}});}})();};};var doParallel=function doParallel(fn){return function(){var args=Array.prototype.slice.call(arguments);return fn.apply(null, [async.each].concat(args));};};var doParallelLimit=function doParallelLimit(limit, fn){return function(){var args=Array.prototype.slice.call(arguments);return fn.apply(null, [_eachLimit(limit)].concat(args));};};var doSeries=function doSeries(fn){return function(){var args=Array.prototype.slice.call(arguments);return fn.apply(null, [async.eachSeries].concat(args));};};var _asyncMap=function _asyncMap(eachfn, arr, iterator, callback){arr = _map(arr, function(x, i){return {index:i, value:x};});if(!callback){eachfn(arr, function(x, callback){iterator(x.value, function(err){callback(err);});});}else {var results=[];eachfn(arr, function(x, callback){iterator(x.value, function(err, v){results[x.index] = v;callback(err);});}, function(err){callback(err, results);});}};async.map = doParallel(_asyncMap);async.mapSeries = doSeries(_asyncMap);async.mapLimit = function(arr, limit, iterator, callback){return _mapLimit(limit)(arr, iterator, callback);};var _mapLimit=function _mapLimit(limit){return doParallelLimit(limit, _asyncMap);};async.reduce = function(arr, memo, iterator, callback){async.eachSeries(arr, function(x, callback){iterator(memo, x, function(err, v){memo = v;callback(err);});}, function(err){callback(err, memo);});};async.inject = async.reduce;async.foldl = async.reduce;async.reduceRight = function(arr, memo, iterator, callback){var reversed=_map(arr, function(x){return x;}).reverse();async.reduce(reversed, memo, iterator, callback);};async.foldr = async.reduceRight;var _filter=function _filter(eachfn, arr, iterator, callback){var results=[];arr = _map(arr, function(x, i){return {index:i, value:x};});eachfn(arr, function(x, callback){iterator(x.value, function(v){if(v){results.push(x);}callback();});}, function(err){callback(_map(results.sort(function(a, b){return a.index - b.index;}), function(x){return x.value;}));});};async.filter = doParallel(_filter);async.filterSeries = doSeries(_filter);async.select = async.filter;async.selectSeries = async.filterSeries;var _reject=function _reject(eachfn, arr, iterator, callback){var results=[];arr = _map(arr, function(x, i){return {index:i, value:x};});eachfn(arr, function(x, callback){iterator(x.value, function(v){if(!v){results.push(x);}callback();});}, function(err){callback(_map(results.sort(function(a, b){return a.index - b.index;}), function(x){return x.value;}));});};async.reject = doParallel(_reject);async.rejectSeries = doSeries(_reject);var _detect=function _detect(eachfn, arr, iterator, main_callback){eachfn(arr, function(x, callback){iterator(x, function(result){if(result){main_callback(x);main_callback = function(){};}else {callback();}});}, function(err){main_callback();});};async.detect = doParallel(_detect);async.detectSeries = doSeries(_detect);async.some = function(arr, iterator, main_callback){async.each(arr, function(x, callback){iterator(x, function(v){if(v){main_callback(true);main_callback = function(){};}callback();});}, function(err){main_callback(false);});};async.any = async.some;async.every = function(arr, iterator, main_callback){async.each(arr, function(x, callback){iterator(x, function(v){if(!v){main_callback(false);main_callback = function(){};}callback();});}, function(err){main_callback(true);});};async.all = async.every;async.sortBy = function(arr, iterator, callback){async.map(arr, function(x, callback){iterator(x, function(err, criteria){if(err){callback(err);}else {callback(null, {value:x, criteria:criteria});}});}, function(err, results){if(err){return callback(err);}else {var fn=function fn(left, right){var a=left.criteria, b=right.criteria;return a < b?-1:a > b?1:0;};callback(null, _map(results.sort(fn), function(x){return x.value;}));}});};async.auto = function(tasks, callback){callback = callback || function(){};var keys=_keys(tasks);var remainingTasks=keys.length;if(!remainingTasks){return callback();}var results={};var listeners=[];var addListener=function addListener(fn){listeners.unshift(fn);};var removeListener=function removeListener(fn){for(var i=0; i < listeners.length; i += 1) {if(listeners[i] === fn){listeners.splice(i, 1);return;}}};var taskComplete=function taskComplete(){remainingTasks--;_each(listeners.slice(0), function(fn){fn();});};addListener(function(){if(!remainingTasks){var theCallback=callback;callback = function(){};theCallback(null, results);}});_each(keys, function(k){var task=_isArray(tasks[k])?tasks[k]:[tasks[k]];var taskCallback=function taskCallback(err){var args=Array.prototype.slice.call(arguments, 1);if(args.length <= 1){args = args[0];}if(err){var safeResults={};_each(_keys(results), function(rkey){safeResults[rkey] = results[rkey];});safeResults[k] = args;callback(err, safeResults);callback = function(){};}else {results[k] = args;async.setImmediate(taskComplete);}};var requires=task.slice(0, Math.abs(task.length - 1)) || [];var ready=function ready(){return _reduce(requires, function(a, x){return a && results.hasOwnProperty(x);}, true) && !results.hasOwnProperty(k);};if(ready()){task[task.length - 1](taskCallback, results);}else {var listener=(function(_listener){var _listenerWrapper=function listener(){return _listener.apply(this, arguments);};_listenerWrapper.toString = function(){return _listener.toString();};return _listenerWrapper;})(function(){if(ready()){removeListener(listener);task[task.length - 1](taskCallback, results);}});addListener(listener);}});};async.retry = function(times, task, callback){var DEFAULT_TIMES=5;var attempts=[];if(typeof times === "function"){callback = task;task = times;times = DEFAULT_TIMES;}times = parseInt(times, 10) || DEFAULT_TIMES;var wrappedTask=function wrappedTask(wrappedCallback, wrappedResults){var retryAttempt=function retryAttempt(task, finalAttempt){return function(seriesCallback){task(function(err, result){seriesCallback(!err || finalAttempt, {err:err, result:result});}, wrappedResults);};};while(times) {attempts.push(retryAttempt(task, !(times -= 1)));}async.series(attempts, function(done, data){data = data[data.length - 1];(wrappedCallback || callback)(data.err, data.result);});};return callback?wrappedTask():wrappedTask;};async.waterfall = function(tasks, callback){callback = callback || function(){};if(!_isArray(tasks)){var err=new Error("First argument to waterfall must be an array of functions");return callback(err);}if(!tasks.length){return callback();}var wrapIterator=(function(_wrapIterator){var _wrapIteratorWrapper=function wrapIterator(_x){return _wrapIterator.apply(this, arguments);};_wrapIteratorWrapper.toString = function(){return _wrapIterator.toString();};return _wrapIteratorWrapper;})(function(iterator){return function(err){if(err){callback.apply(null, arguments);callback = function(){};}else {var args=Array.prototype.slice.call(arguments, 1);var next=iterator.next();if(next){args.push(wrapIterator(next));}else {args.push(callback);}async.setImmediate(function(){iterator.apply(null, args);});}};});wrapIterator(async.iterator(tasks))();};var _parallel=function _parallel(eachfn, tasks, callback){callback = callback || function(){};if(_isArray(tasks)){eachfn.map(tasks, function(fn, callback){if(fn){fn(function(err){var args=Array.prototype.slice.call(arguments, 1);if(args.length <= 1){args = args[0];}callback.call(null, err, args);});}}, callback);}else {var results={};eachfn.each(_keys(tasks), function(k, callback){tasks[k](function(err){var args=Array.prototype.slice.call(arguments, 1);if(args.length <= 1){args = args[0];}results[k] = args;callback(err);});}, function(err){callback(err, results);});}};async.parallel = function(tasks, callback){_parallel({map:async.map, each:async.each}, tasks, callback);};async.parallelLimit = function(tasks, limit, callback){_parallel({map:_mapLimit(limit), each:_eachLimit(limit)}, tasks, callback);};async.series = function(tasks, callback){callback = callback || function(){};if(_isArray(tasks)){async.mapSeries(tasks, function(fn, callback){if(fn){fn(function(err){var args=Array.prototype.slice.call(arguments, 1);if(args.length <= 1){args = args[0];}callback.call(null, err, args);});}}, callback);}else {var results={};async.eachSeries(_keys(tasks), function(k, callback){tasks[k](function(err){var args=Array.prototype.slice.call(arguments, 1);if(args.length <= 1){args = args[0];}results[k] = args;callback(err);});}, function(err){callback(err, results);});}};async.iterator = function(tasks){var makeCallback=(function(_makeCallback){var _makeCallbackWrapper=function makeCallback(_x){return _makeCallback.apply(this, arguments);};_makeCallbackWrapper.toString = function(){return _makeCallback.toString();};return _makeCallbackWrapper;})(function(index){var fn=(function(_fn){var _fnWrapper=function fn(){return _fn.apply(this, arguments);};_fnWrapper.toString = function(){return _fn.toString();};return _fnWrapper;})(function(){if(tasks.length){tasks[index].apply(null, arguments);}return fn.next();});fn.next = function(){return index < tasks.length - 1?makeCallback(index + 1):null;};return fn;});return makeCallback(0);};async.apply = function(fn){var args=Array.prototype.slice.call(arguments, 1);return function(){return fn.apply(null, args.concat(Array.prototype.slice.call(arguments)));};};var _concat=function _concat(eachfn, arr, fn, callback){var r=[];eachfn(arr, function(x, cb){fn(x, function(err, y){r = r.concat(y || []);cb(err);});}, function(err){callback(err, r);});};async.concat = doParallel(_concat);async.concatSeries = doSeries(_concat);async.whilst = function(test, iterator, callback){if(test()){iterator(function(err){if(err){return callback(err);}async.whilst(test, iterator, callback);});}else {callback();}};async.doWhilst = function(iterator, test, callback){iterator(function(err){if(err){return callback(err);}var args=Array.prototype.slice.call(arguments, 1);if(test.apply(null, args)){async.doWhilst(iterator, test, callback);}else {callback();}});};async.until = function(test, iterator, callback){if(!test()){iterator(function(err){if(err){return callback(err);}async.until(test, iterator, callback);});}else {callback();}};async.doUntil = function(iterator, test, callback){iterator(function(err){if(err){return callback(err);}var args=Array.prototype.slice.call(arguments, 1);if(!test.apply(null, args)){async.doUntil(iterator, test, callback);}else {callback();}});};async.queue = function(worker, concurrency){if(concurrency === undefined){concurrency = 1;}function _insert(q, data, pos, callback){if(!q.started){q.started = true;}if(!_isArray(data)){data = [data];}if(data.length == 0){return async.setImmediate(function(){if(q.drain){q.drain();}});}_each(data, function(task){var item={data:task, callback:typeof callback === "function"?callback:null};if(pos){q.tasks.unshift(item);}else {q.tasks.push(item);}if(q.saturated && q.tasks.length === q.concurrency){q.saturated();}async.setImmediate(q.process);});}var workers=0;var q={tasks:[], concurrency:concurrency, saturated:null, empty:null, drain:null, started:false, paused:false, push:function push(data, callback){_insert(q, data, false, callback);}, kill:function kill(){q.drain = null;q.tasks = [];}, unshift:function unshift(data, callback){_insert(q, data, true, callback);}, process:function process(){if(!q.paused && workers < q.concurrency && q.tasks.length){var task=q.tasks.shift();if(q.empty && q.tasks.length === 0){q.empty();}workers += 1;var next=function next(){workers -= 1;if(task.callback){task.callback.apply(task, arguments);}if(q.drain && q.tasks.length + workers === 0){q.drain();}q.process();};var cb=only_once(next);worker(task.data, cb);}}, length:function length(){return q.tasks.length;}, running:function running(){return workers;}, idle:function idle(){return q.tasks.length + workers === 0;}, pause:function pause(){if(q.paused === true){return;}q.paused = true;}, resume:function resume(){if(q.paused === false){return;}q.paused = false;for(var w=1; w <= q.concurrency; w++) {async.setImmediate(q.process);}}};return q;};async.priorityQueue = function(worker, concurrency){function _compareTasks(a, b){return a.priority - b.priority;};function _binarySearch(sequence, item, compare){var beg=-1, end=sequence.length - 1;while(beg < end) {var mid=beg + (end - beg + 1 >>> 1);if(compare(item, sequence[mid]) >= 0){beg = mid;}else {end = mid - 1;}}return beg;}function _insert(q, data, priority, callback){if(!q.started){q.started = true;}if(!_isArray(data)){data = [data];}if(data.length == 0){return async.setImmediate(function(){if(q.drain){q.drain();}});}_each(data, function(task){var item={data:task, priority:priority, callback:typeof callback === "function"?callback:null};q.tasks.splice(_binarySearch(q.tasks, item, _compareTasks) + 1, 0, item);if(q.saturated && q.tasks.length === q.concurrency){q.saturated();}async.setImmediate(q.process);});}var q=async.queue(worker, concurrency);q.push = function(data, priority, callback){_insert(q, data, priority, callback);};delete q.unshift;return q;};async.cargo = function(worker, payload){var working=false, tasks=[];var cargo={tasks:tasks, payload:payload, saturated:null, empty:null, drain:null, drained:true, push:function push(data, callback){if(!_isArray(data)){data = [data];}_each(data, function(task){tasks.push({data:task, callback:typeof callback === "function"?callback:null});cargo.drained = false;if(cargo.saturated && tasks.length === payload){cargo.saturated();}});async.setImmediate(cargo.process);}, process:function process(){if(working){return;}if(tasks.length === 0){if(cargo.drain && !cargo.drained)cargo.drain();cargo.drained = true;return;}var ts=typeof payload === "number"?tasks.splice(0, payload):tasks.splice(0, tasks.length);var ds=_map(ts, function(task){return task.data;});if(cargo.empty)cargo.empty();working = true;worker(ds, function(){working = false;var args=arguments;_each(ts, function(data){if(data.callback){data.callback.apply(null, args);}});process();});}, length:function length(){return tasks.length;}, running:function running(){return working;}};return cargo;};var _console_fn=function _console_fn(name){return function(fn){var args=Array.prototype.slice.call(arguments, 1);fn.apply(null, args.concat([function(err){var args=Array.prototype.slice.call(arguments, 1);if(typeof console !== "undefined"){if(err){if(console.error){console.error(err);}}else if(console[name]){_each(args, function(x){console[name](x);});}}}]));};};async.log = _console_fn("log");async.dir = _console_fn("dir");async.memoize = function(fn, hasher){var memo={};var queues={};hasher = hasher || function(x){return x;};var memoized=function memoized(){var args=Array.prototype.slice.call(arguments);var callback=args.pop();var key=hasher.apply(null, args);if(key in memo){async.nextTick(function(){callback.apply(null, memo[key]);});}else if(key in queues){queues[key].push(callback);}else {queues[key] = [callback];fn.apply(null, args.concat([function(){memo[key] = arguments;var q=queues[key];delete queues[key];for(var i=0, l=q.length; i < l; i++) {q[i].apply(null, arguments);}}]));}};memoized.memo = memo;memoized.unmemoized = fn;return memoized;};async.unmemoize = function(fn){return function(){return (fn.unmemoized || fn).apply(null, arguments);};};async.times = function(count, iterator, callback){var counter=[];for(var i=0; i < count; i++) {counter.push(i);}return async.map(counter, iterator, callback);};async.timesSeries = function(count, iterator, callback){var counter=[];for(var i=0; i < count; i++) {counter.push(i);}return async.mapSeries(counter, iterator, callback);};async.seq = function(){var fns=arguments;return function(){var that=this;var args=Array.prototype.slice.call(arguments);var callback=args.pop();async.reduce(fns, args, function(newargs, fn, cb){fn.apply(that, newargs.concat([function(){var err=arguments[0];var nextargs=Array.prototype.slice.call(arguments, 1);cb(err, nextargs);}]));}, function(err, results){callback.apply(that, [err].concat(results));});};};async.compose = function(){return async.seq.apply(null, Array.prototype.reverse.call(arguments));};var _applyEach=function _applyEach(eachfn, fns){var go=function go(){var that=this;var args=Array.prototype.slice.call(arguments);var callback=args.pop();return eachfn(fns, function(fn, cb){fn.apply(that, args.concat([cb]));}, callback);};if(arguments.length > 2){var args=Array.prototype.slice.call(arguments, 2);return go.apply(this, args);}else {return go;}};async.applyEach = doParallel(_applyEach);async.applyEachSeries = doSeries(_applyEach);async.forever = function(fn, callback){function next(err){if(err){if(callback){return callback(err);}throw err;}fn(next);}next();};if(typeof module !== "undefined" && module.exports){module.exports = async;}else if(typeof define !== "undefined" && define.amd){define([], function(){return async;});}else {root.async = async;}})();}).call(this, require("_process"));}, {_process:3}], 13:[function(require, module, exports){var async=require("async"), urlParser=require("url"), Resource=require("./Resource"), EventEmitter=require("eventemitter3");function Loader(baseUrl, concurrency){EventEmitter.call(this);concurrency = concurrency || 10;this.baseUrl = baseUrl || "";this.progress = 0;this.loading = false;this._progressChunk = 0;this._beforeMiddleware = [];this._afterMiddleware = [];this._boundLoadResource = this._loadResource.bind(this);this._boundOnLoad = this._onLoad.bind(this);this._buffer = [];this._numToLoad = 0;this._queue = async.queue(this._boundLoadResource, concurrency);this.resources = {};}Loader.prototype = Object.create(EventEmitter.prototype);Loader.prototype.constructor = Loader;module.exports = Loader;Loader.prototype.add = Loader.prototype.enqueue = function(name, url, options, cb){if(Array.isArray(name)){for(var i=0; i < name.length; ++i) {this.add(name[i]);}return this;}if(typeof name === "object"){cb = url || name.callback || name.onComplete;options = name;url = name.url;name = name.name || name.key || name.url;}if(typeof url !== "string"){cb = options;options = url;url = name;}if(typeof url !== "string"){throw new Error("No url passed to add resource to loader.");}if(typeof options === "function"){cb = options;options = null;}if(this.resources[name]){throw new Error("Resource with name \"" + name + "\" already exists.");}url = this._handleBaseUrl(url);this.resources[name] = new Resource(name, url, options);if(typeof cb === "function"){this.resources[name].once("afterMiddleware", cb);}this._numToLoad++;if(this._queue.started){this._queue.push(this.resources[name]);this._progressChunk = (100 - this.progress) / (this._queue.length() + this._queue.running());}else {this._buffer.push(this.resources[name]);this._progressChunk = 100 / this._buffer.length;}return this;};Loader.prototype._handleBaseUrl = function(url){var parsedUrl=urlParser.parse(url);if(parsedUrl.protocol || parsedUrl.pathname.indexOf("//") === 0){return url;}if(this.baseUrl.length && this.baseUrl.lastIndexOf("/") !== this.baseUrl.length - 1 && url.charAt(0) !== "/"){return this.baseUrl + "/" + url;}else {return this.baseUrl + url;}};Loader.prototype.before = Loader.prototype.pre = function(fn){this._beforeMiddleware.push(fn);return this;};Loader.prototype.after = Loader.prototype.use = function(fn){this._afterMiddleware.push(fn);return this;};Loader.prototype.reset = function(){this.progress = 0;this.loading = false;this._progressChunk = 0;this._buffer.length = 0;this._numToLoad = 0;this._queue.kill();this._queue.started = false;this.resources = {};};Loader.prototype.load = function(cb){if(typeof cb === "function"){this.once("complete", cb);}if(this._queue.started){return this;}this.emit("start", this);for(var i=0; i < this._buffer.length; ++i) {this._queue.push(this._buffer[i]);}this._buffer.length = 0;return this;};Loader.prototype._loadResource = function(resource, dequeue){var self=this;resource._dequeue = dequeue;this._runMiddleware(resource, this._beforeMiddleware, function(){resource.load(self._boundOnLoad);});};Loader.prototype._onComplete = function(){this.emit("complete", this, this.resources);};Loader.prototype._onLoad = function(resource){this.progress += this._progressChunk;this.emit("progress", this, resource);this._runMiddleware(resource, this._afterMiddleware, function(){resource.emit("afterMiddleware", resource);this._numToLoad--;if(this._numToLoad === 0){this.progress = 100;this._onComplete();}if(resource.error){this.emit("error", resource.error, this, resource);}else {this.emit("load", this, resource);}});resource._dequeue();};Loader.prototype._runMiddleware = function(resource, fns, cb){var self=this;async.eachSeries(fns, function(fn, next){fn.call(self, resource, next);}, cb.bind(this, resource));};Loader.LOAD_TYPE = Resource.LOAD_TYPE;Loader.XHR_READY_STATE = Resource.XHR_READY_STATE;Loader.XHR_RESPONSE_TYPE = Resource.XHR_RESPONSE_TYPE;}, {"./Resource":14, async:12, eventemitter3:10, url:8}], 14:[function(require, module, exports){var EventEmitter=require("eventemitter3"), _url=require("url"), useXdr=!!(window.XDomainRequest && !("withCredentials" in new XMLHttpRequest())), tempAnchor=null;function Resource(name, url, options){EventEmitter.call(this);options = options || {};if(typeof name !== "string" || typeof url !== "string"){throw new Error("Both name and url are required for constructing a resource.");}this.name = name;this.url = url;this.isDataUrl = this.url.indexOf("data:") === 0;this.data = null;this.crossOrigin = options.crossOrigin === true?"anonymous":options.crossOrigin;this.loadType = options.loadType || this._determineLoadType();this.xhrType = options.xhrType;this.metadata = options.metadata || {};this.error = null;this.xhr = null;this.isJson = false;this.isXml = false;this.isImage = false;this.isAudio = false;this.isVideo = false;this._dequeue = null;this._boundComplete = this.complete.bind(this);this._boundOnError = this._onError.bind(this);this._boundOnProgress = this._onProgress.bind(this);this._boundXhrOnError = this._xhrOnError.bind(this);this._boundXhrOnAbort = this._xhrOnAbort.bind(this);this._boundXhrOnLoad = this._xhrOnLoad.bind(this);this._boundXdrOnTimeout = this._xdrOnTimeout.bind(this);}Resource.prototype = Object.create(EventEmitter.prototype);Resource.prototype.constructor = Resource;module.exports = Resource;Resource.prototype.complete = function(){if(this.data && this.data.removeEventListener){this.data.removeEventListener("error", this._boundOnError);this.data.removeEventListener("load", this._boundComplete);this.data.removeEventListener("progress", this._boundOnProgress);this.data.removeEventListener("canplaythrough", this._boundComplete);}if(this.xhr){if(this.xhr.removeEventListener){this.xhr.removeEventListener("error", this._boundXhrOnError);this.xhr.removeEventListener("abort", this._boundXhrOnAbort);this.xhr.removeEventListener("progress", this._boundOnProgress);this.xhr.removeEventListener("load", this._boundXhrOnLoad);}else {this.xhr.onerror = null;this.xhr.ontimeout = null;this.xhr.onprogress = null;this.xhr.onload = null;}}this.emit("complete", this);};Resource.prototype.load = function(cb){this.emit("start", this);if(cb){this.once("complete", cb);}if(this.crossOrigin === false || typeof this.crossOrigin !== "string"){this.crossOrigin = this._determineCrossOrigin(this.url);}switch(this.loadType){case Resource.LOAD_TYPE.IMAGE:this._loadImage();break;case Resource.LOAD_TYPE.AUDIO:this._loadElement("audio");break;case Resource.LOAD_TYPE.VIDEO:this._loadElement("video");break;case Resource.LOAD_TYPE.XHR:default:if(useXdr && this.crossOrigin){this._loadXdr();}else {this._loadXhr();}break;}};Resource.prototype._loadImage = function(){this.data = new Image();if(this.crossOrigin){this.data.crossOrigin = this.crossOrigin;}this.data.src = this.url;this.isImage = true;this.data.addEventListener("error", this._boundOnError, false);this.data.addEventListener("load", this._boundComplete, false);this.data.addEventListener("progress", this._boundOnProgress, false);};Resource.prototype._loadElement = function(type){if(type === "audio" && typeof Audio !== "undefined"){this.data = new Audio();}else {this.data = document.createElement(type);}if(this.data === null){this.error = new Error("Unsupported element " + type);this.complete();return;}if(navigator.isCocoonJS){this.data.src = Array.isArray(this.url)?this.url[0]:this.url;}else {if(Array.isArray(this.url)){for(var i=0; i < this.url.length; ++i) {this.data.appendChild(this._createSource(type, this.url[i]));}}else {this.data.appendChild(this._createSource(type, this.url));}}this["is" + type[0].toUpperCase() + type.substring(1)] = true;this.data.addEventListener("error", this._boundOnError, false);this.data.addEventListener("load", this._boundComplete, false);this.data.addEventListener("progress", this._boundOnProgress, false);this.data.addEventListener("canplaythrough", this._boundComplete, false);this.data.load();};Resource.prototype._loadXhr = function(){if(typeof this.xhrType !== "string"){this.xhrType = this._determineXhrType();}var xhr=this.xhr = new XMLHttpRequest();xhr.open("GET", this.url, true);if(this.xhrType === Resource.XHR_RESPONSE_TYPE.JSON || this.xhrType === Resource.XHR_RESPONSE_TYPE.DOCUMENT){xhr.responseType = Resource.XHR_RESPONSE_TYPE.TEXT;}else {xhr.responseType = this.xhrType;}xhr.addEventListener("error", this._boundXhrOnError, false);xhr.addEventListener("abort", this._boundXhrOnAbort, false);xhr.addEventListener("progress", this._boundOnProgress, false);xhr.addEventListener("load", this._boundXhrOnLoad, false);xhr.send();};Resource.prototype._loadXdr = function(){if(typeof this.xhrType !== "string"){this.xhrType = this._determineXhrType();}var xdr=this.xhr = new XDomainRequest();xdr.timeout = 5000;xdr.onerror = this._boundXhrOnError;xdr.ontimeout = this._boundXdrOnTimeout;xdr.onprogress = this._boundOnProgress;xdr.onload = this._boundXhrOnLoad;xdr.open("GET", this.url, true);setTimeout(function(){xdr.send();}, 0);};Resource.prototype._createSource = function(type, url, mime){if(!mime){mime = type + "/" + url.substr(url.lastIndexOf(".") + 1);}var source=document.createElement("source");source.src = url;source.type = mime;return source;};Resource.prototype._onError = function(event){this.error = new Error("Failed to load element using " + event.target.nodeName);this.complete();};Resource.prototype._onProgress = function(event){if(event && event.lengthComputable){this.emit("progress", this, event.loaded / event.total);}};Resource.prototype._xhrOnError = function(){this.error = new Error(reqType(this.xhr) + " Request failed. " + "Status: " + this.xhr.status + ", text: \"" + this.xhr.statusText + "\"");this.complete();};Resource.prototype._xhrOnAbort = function(){this.error = new Error(reqType(this.xhr) + " Request was aborted by the user.");this.complete();};Resource.prototype._xdrOnTimeout = function(){this.error = new Error(reqType(this.xhr) + " Request timed out.");this.complete();};Resource.prototype._xhrOnLoad = function(){var xhr=this.xhr, status=xhr.status !== undefined?xhr.status:200;if(status === 200 || status === 204 || status === 0 && xhr.responseText.length > 0){if(this.xhrType === Resource.XHR_RESPONSE_TYPE.TEXT){this.data = xhr.responseText;}else if(this.xhrType === Resource.XHR_RESPONSE_TYPE.JSON){try{this.data = JSON.parse(xhr.responseText);this.isJson = true;}catch(e) {this.error = new Error("Error trying to parse loaded json:", e);}}else if(this.xhrType === Resource.XHR_RESPONSE_TYPE.DOCUMENT){try{if(window.DOMParser){var domparser=new DOMParser();this.data = domparser.parseFromString(xhr.responseText, "text/xml");}else {var div=document.createElement("div");div.innerHTML = xhr.responseText;this.data = div;}this.isXml = true;}catch(e) {this.error = new Error("Error trying to parse loaded xml:", e);}}else {this.data = xhr.response || xhr.responseText;}}else {this.error = new Error("[" + xhr.status + "]" + xhr.statusText + ":" + xhr.responseURL);}this.complete();};function reqType(xhr){return xhr.toString().replace("object ", "");}Resource.prototype._determineCrossOrigin = function(url, loc){if(url.indexOf("data:") === 0){return "";}loc = loc || window.location;if(!tempAnchor){tempAnchor = document.createElement("a");}tempAnchor.href = url;url = _url.parse(tempAnchor.href);var samePort=!url.port && loc.port === "" || url.port === loc.port;if(url.hostname !== loc.hostname || !samePort || url.protocol !== loc.protocol){return "anonymous";}return "";};Resource.prototype._determineXhrType = function(){return Resource._xhrTypeMap[this._getExtension()] || Resource.XHR_RESPONSE_TYPE.TEXT;};Resource.prototype._determineLoadType = function(){return Resource._loadTypeMap[this._getExtension()] || Resource.LOAD_TYPE.XHR;};Resource.prototype._getExtension = function(){var url=this.url, ext;if(this.isDataUrl){var slashIndex=url.indexOf("/");ext = url.substring(slashIndex + 1, url.indexOf(";", slashIndex));}else {var queryStart=url.indexOf("?");if(queryStart !== -1){url = url.substring(0, queryStart);}ext = url.substring(url.lastIndexOf(".") + 1);}return ext;};Resource.prototype._getMimeFromXhrType = function(type){switch(type){case Resource.XHR_RESPONSE_TYPE.BUFFER:return "application/octet-binary";case Resource.XHR_RESPONSE_TYPE.BLOB:return "application/blob";case Resource.XHR_RESPONSE_TYPE.DOCUMENT:return "application/xml";case Resource.XHR_RESPONSE_TYPE.JSON:return "application/json";case Resource.XHR_RESPONSE_TYPE.DEFAULT:case Resource.XHR_RESPONSE_TYPE.TEXT:default:return "text/plain";}};Resource.LOAD_TYPE = {XHR:1, IMAGE:2, AUDIO:3, VIDEO:4};Resource.XHR_READY_STATE = {UNSENT:0, OPENED:1, HEADERS_RECEIVED:2, LOADING:3, DONE:4};Resource.XHR_RESPONSE_TYPE = {DEFAULT:"text", BUFFER:"arraybuffer", BLOB:"blob", DOCUMENT:"document", JSON:"json", TEXT:"text"};Resource._loadTypeMap = {gif:Resource.LOAD_TYPE.IMAGE, png:Resource.LOAD_TYPE.IMAGE, bmp:Resource.LOAD_TYPE.IMAGE, jpg:Resource.LOAD_TYPE.IMAGE, jpeg:Resource.LOAD_TYPE.IMAGE, tif:Resource.LOAD_TYPE.IMAGE, tiff:Resource.LOAD_TYPE.IMAGE, webp:Resource.LOAD_TYPE.IMAGE, tga:Resource.LOAD_TYPE.IMAGE};Resource._xhrTypeMap = {xhtml:Resource.XHR_RESPONSE_TYPE.DOCUMENT, html:Resource.XHR_RESPONSE_TYPE.DOCUMENT, htm:Resource.XHR_RESPONSE_TYPE.DOCUMENT, xml:Resource.XHR_RESPONSE_TYPE.DOCUMENT, tmx:Resource.XHR_RESPONSE_TYPE.DOCUMENT, tsx:Resource.XHR_RESPONSE_TYPE.DOCUMENT, svg:Resource.XHR_RESPONSE_TYPE.DOCUMENT, gif:Resource.XHR_RESPONSE_TYPE.BLOB, png:Resource.XHR_RESPONSE_TYPE.BLOB, bmp:Resource.XHR_RESPONSE_TYPE.BLOB, jpg:Resource.XHR_RESPONSE_TYPE.BLOB, jpeg:Resource.XHR_RESPONSE_TYPE.BLOB, tif:Resource.XHR_RESPONSE_TYPE.BLOB, tiff:Resource.XHR_RESPONSE_TYPE.BLOB, webp:Resource.XHR_RESPONSE_TYPE.BLOB, tga:Resource.XHR_RESPONSE_TYPE.BLOB, json:Resource.XHR_RESPONSE_TYPE.JSON, text:Resource.XHR_RESPONSE_TYPE.TEXT, txt:Resource.XHR_RESPONSE_TYPE.TEXT};Resource.setExtensionLoadType = function(extname, loadType){setExtMap(Resource._loadTypeMap, extname, loadType);};Resource.setExtensionXhrType = function(extname, xhrType){setExtMap(Resource._xhrTypeMap, extname, xhrType);};function setExtMap(map, extname, val){if(extname && extname.indexOf(".") === 0){extname = extname.substring(1);}if(!extname){return;}map[extname] = val;}}, {eventemitter3:10, url:8}], 15:[function(require, module, exports){module.exports = {_keyStr:"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=", encodeBinary:function encodeBinary(input){var output="";var bytebuffer;var encodedCharIndexes=new Array(4);var inx=0;var jnx=0;var paddingBytes=0;while(inx < input.length) {bytebuffer = new Array(3);for(jnx = 0; jnx < bytebuffer.length; jnx++) {if(inx < input.length){bytebuffer[jnx] = input.charCodeAt(inx++) & 255;}else {bytebuffer[jnx] = 0;}}encodedCharIndexes[0] = bytebuffer[0] >> 2;encodedCharIndexes[1] = (bytebuffer[0] & 3) << 4 | bytebuffer[1] >> 4;encodedCharIndexes[2] = (bytebuffer[1] & 15) << 2 | bytebuffer[2] >> 6;encodedCharIndexes[3] = bytebuffer[2] & 63;paddingBytes = inx - (input.length - 1);switch(paddingBytes){case 2:encodedCharIndexes[3] = 64;encodedCharIndexes[2] = 64;break;case 1:encodedCharIndexes[3] = 64;break;default:break;}for(jnx = 0; jnx < encodedCharIndexes.length; jnx++) {output += this._keyStr.charAt(encodedCharIndexes[jnx]);}}return output;}};}, {}], 16:[function(require, module, exports){module.exports = require("./Loader");module.exports.Resource = require("./Resource");module.exports.middleware = {caching:{memory:require("./middlewares/caching/memory")}, parsing:{blob:require("./middlewares/parsing/blob")}};}, {"./Loader":13, "./Resource":14, "./middlewares/caching/memory":17, "./middlewares/parsing/blob":18}], 17:[function(require, module, exports){var cache={};module.exports = function(){return function(resource, next){if(cache[resource.url]){resource.data = cache[resource.url];resource.complete();}else {resource.once("complete", function(){cache[this.url] = this.data;});}next();};};}, {}], 18:[function(require, module, exports){var Resource=require("../../Resource"), b64=require("../../b64");window.URL = window.URL || window.webkitURL;module.exports = function(){return function(resource, next){if(!resource.data){return next();}if(resource.xhr && resource.xhrType === Resource.XHR_RESPONSE_TYPE.BLOB){if(!window.Blob || typeof resource.data === "string"){var type=resource.xhr.getResponseHeader("content-type");if(type && type.indexOf("image") === 0){resource.data = new Image();resource.data.src = "data:" + type + ";base64," + b64.encodeBinary(resource.xhr.responseText);resource.isImage = true;resource.data.onload = function(){resource.data.onload = null;next();};}}else if(resource.data.type.indexOf("image") === 0){var src=URL.createObjectURL(resource.data);resource.blob = resource.data;resource.data = new Image();resource.data.src = src;resource.isImage = true;resource.data.onload = function(){URL.revokeObjectURL(src);resource.data.onload = null;next();};}}else {next();}};};}, {"../../Resource":14, "../../b64":15}], 19:[function(require, module, exports){var core=require("../core");Object.assign(core.DisplayObject.prototype, require("./accessibleTarget"));function AccessibilityManager(renderer){var div=document.createElement("div");div.style.width = 100 + "px";div.style.height = 100 + "px";div.style.position = "absolute";div.style.top = 0;div.style.left = 0;div.style.zIndex = 2;this.div = div;this.pool = [];this.renderId = 0;this.debug = false;this.renderer = renderer;this.children = [];this._onKeyDown = this._onKeyDown.bind(this);this._onMouseMove = this._onMouseMove.bind(this);this.isActive = false;window.addEventListener("keydown", this._onKeyDown, false);}AccessibilityManager.prototype.constructor = AccessibilityManager;module.exports = AccessibilityManager;AccessibilityManager.prototype.activate = function(){if(this.isActive){return;}this.isActive = true;window.document.addEventListener("mousemove", this._onMouseMove, true);window.removeEventListener("keydown", this._onKeyDown, false);this.renderer.on("postrender", this.update, this);this.renderer.view.parentNode.appendChild(this.div);};AccessibilityManager.prototype.deactivate = function(){if(!this.isActive){return;}this.isActive = false;window.document.removeEventListener("mousemove", this._onMouseMove);window.addEventListener("keydown", this._onKeyDown, false);this.renderer.off("postrender", this.update);this.div.parentNode.removeChild(this.div);};AccessibilityManager.prototype.updateAccessibleObjects = function(displayObject){if(!displayObject.visible){return;}if(displayObject.accessible && displayObject.interactive){if(!displayObject._accessibleActive){this.addChild(displayObject);}displayObject.renderId = this.renderId;}if(displayObject.interactiveChildren){var children=displayObject.children;for(var i=children.length - 1; i >= 0; i--) {this.updateAccessibleObjects(children[i]);}}};AccessibilityManager.prototype.update = function(){this.updateAccessibleObjects(this.renderer._lastObjectRendered);var rect=this.renderer.view.getBoundingClientRect();var sx=rect.width / this.renderer.width;var sy=rect.height / this.renderer.height;var div=this.div;div.style.left = rect.left + "px";div.style.top = rect.top + "px";div.style.width = this.renderer.width + "px";div.style.height = this.renderer.height + "px";for(var i=0; i < this.children.length; i++) {var child=this.children[i];if(child.renderId !== this.renderId){child._accessibleActive = false;core.utils.removeItems(this.children, i, 1);this.div.removeChild(child._accessibleDiv);this.pool.push(child._accessibleDiv);child._accessibleDiv = null;i--;if(this.children.length === 0){this.deactivate();}}else {div = child._accessibleDiv;var hitArea=child.hitArea;var wt=child.worldTransform;if(child.hitArea){div.style.left = (wt.tx + hitArea.x * wt.a) * sx + "px";div.style.top = (wt.ty + hitArea.y * wt.d) * sy + "px";div.style.width = hitArea.width * wt.a * sx + "px";div.style.height = hitArea.height * wt.d * sy + "px";}else {hitArea = child.getBounds();this.capHitArea(hitArea);div.style.left = hitArea.x * sx + "px";div.style.top = hitArea.y * sy + "px";div.style.width = hitArea.width * sx + "px";div.style.height = hitArea.height * sy + "px";}}}this.renderId++;};AccessibilityManager.prototype.capHitArea = function(hitArea){if(hitArea.x < 0){hitArea.width += hitArea.x;hitArea.x = 0;}if(hitArea.y < 0){hitArea.height += hitArea.y;hitArea.y = 0;}if(hitArea.x + hitArea.width > this.renderer.width){hitArea.width = this.renderer.width - hitArea.x;}if(hitArea.y + hitArea.height > this.renderer.height){hitArea.height = this.renderer.height - hitArea.y;}};AccessibilityManager.prototype.addChild = function(displayObject){var div=this.pool.pop();if(!div){div = document.createElement("button");div.style.width = 100 + "px";div.style.height = 100 + "px";div.style.backgroundColor = this.debug?"rgba(255,0,0,0.5)":"transparent";div.style.position = "absolute";div.style.zIndex = 2;div.style.borderStyle = "none";div.addEventListener("click", this._onClick.bind(this));div.addEventListener("focus", this._onFocus.bind(this));div.addEventListener("focusout", this._onFocusOut.bind(this));}div.title = displayObject.accessibleTitle || "displayObject " + this.tabIndex;displayObject._accessibleActive = true;displayObject._accessibleDiv = div;div.displayObject = displayObject;this.children.push(displayObject);this.div.appendChild(displayObject._accessibleDiv);displayObject._accessibleDiv.tabIndex = displayObject.tabIndex;};AccessibilityManager.prototype._onClick = function(e){var interactionManager=this.renderer.plugins.interaction;interactionManager.dispatchEvent(e.target.displayObject, "click", interactionManager.eventData);};AccessibilityManager.prototype._onFocus = function(e){var interactionManager=this.renderer.plugins.interaction;interactionManager.dispatchEvent(e.target.displayObject, "mouseover", interactionManager.eventData);};AccessibilityManager.prototype._onFocusOut = function(e){var interactionManager=this.renderer.plugins.interaction;interactionManager.dispatchEvent(e.target.displayObject, "mouseout", interactionManager.eventData);};AccessibilityManager.prototype._onKeyDown = function(e){if(e.keyCode !== 9){return;}this.activate();};AccessibilityManager.prototype._onMouseMove = function(){this.deactivate();};AccessibilityManager.prototype.destroy = function(){this.div = null;for(var i=0; i < this.children.length; i++) {this.children[i].div = null;}window.document.removeEventListener("mousemove", this._onMouseMove);window.removeEventListener("keydown", this._onKeyDown);this.pool = null;this.children = null;this.renderer = null;};core.WebGLRenderer.registerPlugin("accessibility", AccessibilityManager);core.CanvasRenderer.registerPlugin("accessibility", AccessibilityManager);}, {"../core":29, "./accessibleTarget":20}], 20:[function(require, module, exports){var accessibleTarget={accessible:false, accessibleTitle:null, tabIndex:0, _accessibleActive:false, _accessibleDiv:false};module.exports = accessibleTarget;}, {}], 21:[function(require, module, exports){module.exports = {accessibleTarget:require("./accessibleTarget"), AccessibilityManager:require("./AccessibilityManager")};}, {"./AccessibilityManager":19, "./accessibleTarget":20}], 22:[function(require, module, exports){var CONST={VERSION:"3.0.10", PI_2:Math.PI * 2, RAD_TO_DEG:180 / Math.PI, DEG_TO_RAD:Math.PI / 180, TARGET_FPMS:0.06, RENDERER_TYPE:{UNKNOWN:0, WEBGL:1, CANVAS:2}, BLEND_MODES:{NORMAL:0, ADD:1, MULTIPLY:2, SCREEN:3, OVERLAY:4, DARKEN:5, LIGHTEN:6, COLOR_DODGE:7, COLOR_BURN:8, HARD_LIGHT:9, SOFT_LIGHT:10, DIFFERENCE:11, EXCLUSION:12, HUE:13, SATURATION:14, COLOR:15, LUMINOSITY:16}, DRAW_MODES:{POINTS:0, LINES:1, LINE_LOOP:2, LINE_STRIP:3, TRIANGLES:4, TRIANGLE_STRIP:5, TRIANGLE_FAN:6}, SCALE_MODES:{DEFAULT:0, LINEAR:0, NEAREST:1}, RETINA_PREFIX:/@(.+)x/, RESOLUTION:1, FILTER_RESOLUTION:1, DEFAULT_RENDER_OPTIONS:{view:null, resolution:1, antialias:false, forceFXAA:false, autoResize:false, transparent:false, backgroundColor:0, clearBeforeRender:true, preserveDrawingBuffer:false, roundPixels:false}, SHAPES:{POLY:0, RECT:1, CIRC:2, ELIP:3, RREC:4}, SPRITE_BATCH_SIZE:2000};module.exports = CONST;}, {}], 23:[function(require, module, exports){var math=require("../math"), utils=require("../utils"), DisplayObject=require("./DisplayObject"), RenderTexture=require("../textures/RenderTexture"), _tempMatrix=new math.Matrix();function Container(){DisplayObject.call(this);this.children = [];}Container.prototype = Object.create(DisplayObject.prototype);Container.prototype.constructor = Container;module.exports = Container;Object.defineProperties(Container.prototype, {width:{get:function get(){return this.scale.x * this.getLocalBounds().width;}, set:function set(value){var width=this.getLocalBounds().width;if(width !== 0){this.scale.x = value / width;}else {this.scale.x = 1;}this._width = value;}}, height:{get:function get(){return this.scale.y * this.getLocalBounds().height;}, set:function set(value){var height=this.getLocalBounds().height;if(height !== 0){this.scale.y = value / height;}else {this.scale.y = 1;}this._height = value;}}});Container.prototype.onChildrenChange = function(){};Container.prototype.addChild = function(child){var argumentsLength=arguments.length;if(argumentsLength > 1){for(var i=0; i < argumentsLength; i++) {this.addChild(arguments[i]);}}else {if(child.parent){child.parent.removeChild(child);}child.parent = this;this.children.push(child);this.onChildrenChange(this.children.length - 1);child.emit("added", this);}return child;};Container.prototype.addChildAt = function(child, index){if(index >= 0 && index <= this.children.length){if(child.parent){child.parent.removeChild(child);}child.parent = this;this.children.splice(index, 0, child);this.onChildrenChange(index);child.emit("added", this);return child;}else {throw new Error(child + "addChildAt: The index " + index + " supplied is out of bounds " + this.children.length);}};Container.prototype.swapChildren = function(child, child2){if(child === child2){return;}var index1=this.getChildIndex(child);var index2=this.getChildIndex(child2);if(index1 < 0 || index2 < 0){throw new Error("swapChildren: Both the supplied DisplayObjects must be children of the caller.");}this.children[index1] = child2;this.children[index2] = child;this.onChildrenChange(index1 < index2?index1:index2);};Container.prototype.getChildIndex = function(child){var index=this.children.indexOf(child);if(index === -1){throw new Error("The supplied DisplayObject must be a child of the caller");}return index;};Container.prototype.setChildIndex = function(child, index){if(index < 0 || index >= this.children.length){throw new Error("The supplied index is out of bounds");}var currentIndex=this.getChildIndex(child);utils.removeItems(this.children, currentIndex, 1);this.children.splice(index, 0, child);this.onChildrenChange(index);};Container.prototype.getChildAt = function(index){if(index < 0 || index >= this.children.length){throw new Error("getChildAt: Supplied index " + index + " does not exist in the child list, or the supplied DisplayObject is not a child of the caller");}return this.children[index];};Container.prototype.removeChild = function(child){var argumentsLength=arguments.length;if(argumentsLength > 1){for(var i=0; i < argumentsLength; i++) {this.removeChild(arguments[i]);}}else {var index=this.children.indexOf(child);if(index === -1){return;}child.parent = null;utils.removeItems(this.children, index, 1);this.onChildrenChange(index);child.emit("removed", this);}return child;};Container.prototype.removeChildAt = function(index){var child=this.getChildAt(index);child.parent = null;utils.removeItems(this.children, index, 1);this.onChildrenChange(index);child.emit("removed", this);return child;};Container.prototype.removeChildren = function(beginIndex, endIndex){var begin=beginIndex || 0;var end=typeof endIndex === "number"?endIndex:this.children.length;var range=end - begin;var removed, i;if(range > 0 && range <= end){removed = this.children.splice(begin, range);for(i = 0; i < removed.length; ++i) {removed[i].parent = null;}this.onChildrenChange(beginIndex);for(i = 0; i < removed.length; ++i) {removed[i].emit("removed", this);}return removed;}else if(range === 0 && this.children.length === 0){return [];}else {throw new RangeError("removeChildren: numeric values are outside the acceptable range.");}};Container.prototype.generateTexture = function(renderer, resolution, scaleMode){var bounds=this.getLocalBounds();var renderTexture=new RenderTexture(renderer, bounds.width | 0, bounds.height | 0, scaleMode, resolution);_tempMatrix.tx = -bounds.x;_tempMatrix.ty = -bounds.y;renderTexture.render(this, _tempMatrix);return renderTexture;};Container.prototype.updateTransform = function(){if(!this.visible){return;}this.displayObjectUpdateTransform();for(var i=0, j=this.children.length; i < j; ++i) {this.children[i].updateTransform();}};Container.prototype.containerUpdateTransform = Container.prototype.updateTransform;Container.prototype.getBounds = function(){if(!this._currentBounds){if(this.children.length === 0){return math.Rectangle.EMPTY;}var minX=Infinity;var minY=Infinity;var maxX=-Infinity;var maxY=-Infinity;var childBounds;var childMaxX;var childMaxY;var childVisible=false;for(var i=0, j=this.children.length; i < j; ++i) {var child=this.children[i];if(!child.visible){continue;}childVisible = true;childBounds = this.children[i].getBounds();minX = minX < childBounds.x?minX:childBounds.x;minY = minY < childBounds.y?minY:childBounds.y;childMaxX = childBounds.width + childBounds.x;childMaxY = childBounds.height + childBounds.y;maxX = maxX > childMaxX?maxX:childMaxX;maxY = maxY > childMaxY?maxY:childMaxY;}if(!childVisible){return math.Rectangle.EMPTY;}var bounds=this._bounds;bounds.x = minX;bounds.y = minY;bounds.width = maxX - minX;bounds.height = maxY - minY;this._currentBounds = bounds;}return this._currentBounds;};Container.prototype.containerGetBounds = Container.prototype.getBounds;Container.prototype.getLocalBounds = function(){var matrixCache=this.worldTransform;this.worldTransform = math.Matrix.IDENTITY;for(var i=0, j=this.children.length; i < j; ++i) {this.children[i].updateTransform();}this.worldTransform = matrixCache;this._currentBounds = null;return this.getBounds(math.Matrix.IDENTITY);};Container.prototype.renderWebGL = function(renderer){if(!this.visible || this.worldAlpha <= 0 || !this.renderable){return;}var i, j;if(this._mask || this._filters){renderer.currentRenderer.flush();if(this._filters && this._filters.length){renderer.filterManager.pushFilter(this, this._filters);}if(this._mask){renderer.maskManager.pushMask(this, this._mask);}renderer.currentRenderer.start();this._renderWebGL(renderer);for(i = 0, j = this.children.length; i < j; i++) {this.children[i].renderWebGL(renderer);}renderer.currentRenderer.flush();if(this._mask){renderer.maskManager.popMask(this, this._mask);}if(this._filters){renderer.filterManager.popFilter();}renderer.currentRenderer.start();}else {this._renderWebGL(renderer);for(i = 0, j = this.children.length; i < j; ++i) {this.children[i].renderWebGL(renderer);}}};Container.prototype._renderWebGL = function(renderer){};Container.prototype._renderCanvas = function(renderer){};Container.prototype.renderCanvas = function(renderer){if(!this.visible || this.alpha <= 0 || !this.renderable){return;}if(this._mask){renderer.maskManager.pushMask(this._mask, renderer);}this._renderCanvas(renderer);for(var i=0, j=this.children.length; i < j; ++i) {this.children[i].renderCanvas(renderer);}if(this._mask){renderer.maskManager.popMask(renderer);}};Container.prototype.destroy = function(destroyChildren){DisplayObject.prototype.destroy.call(this);if(destroyChildren){for(var i=0, j=this.children.length; i < j; ++i) {this.children[i].destroy(destroyChildren);}}this.removeChildren();this.children = null;};}, {"../math":33, "../textures/RenderTexture":71, "../utils":77, "./DisplayObject":24}], 24:[function(require, module, exports){var math=require("../math"), RenderTexture=require("../textures/RenderTexture"), EventEmitter=require("eventemitter3"), CONST=require("../const"), _tempMatrix=new math.Matrix(), _tempDisplayObjectParent={worldTransform:new math.Matrix(), worldAlpha:1, children:[]};function DisplayObject(){EventEmitter.call(this);this.position = new math.Point();this.scale = new math.Point(1, 1);this.pivot = new math.Point(0, 0);this.skew = new math.Point(0, 0);this.rotation = 0;this.alpha = 1;this.visible = true;this.renderable = true;this.parent = null;this.worldAlpha = 1;this.worldTransform = new math.Matrix();this.filterArea = null;this._sr = 0;this._cr = 1;this._bounds = new math.Rectangle(0, 0, 1, 1);this._currentBounds = null;this._mask = null;}DisplayObject.prototype = Object.create(EventEmitter.prototype);DisplayObject.prototype.constructor = DisplayObject;module.exports = DisplayObject;Object.defineProperties(DisplayObject.prototype, {x:{get:function get(){return this.position.x;}, set:function set(value){this.position.x = value;}}, y:{get:function get(){return this.position.y;}, set:function set(value){this.position.y = value;}}, worldVisible:{get:function get(){var item=this;do{if(!item.visible){return false;}item = item.parent;}while(item);return true;}}, mask:{get:function get(){return this._mask;}, set:function set(value){if(this._mask){this._mask.renderable = true;}this._mask = value;if(this._mask){this._mask.renderable = false;}}}, filters:{get:function get(){return this._filters && this._filters.slice();}, set:function set(value){this._filters = value && value.slice();}}});DisplayObject.prototype.updateTransform = function(){var pt=this.parent.worldTransform;var wt=this.worldTransform;var a, b, c, d, tx, ty;if(this.skew.x || this.skew.y){_tempMatrix.setTransform(this.position.x, this.position.y, this.pivot.x, this.pivot.y, this.scale.x, this.scale.y, this.rotation, this.skew.x, this.skew.y);wt.a = _tempMatrix.a * pt.a + _tempMatrix.b * pt.c;wt.b = _tempMatrix.a * pt.b + _tempMatrix.b * pt.d;wt.c = _tempMatrix.c * pt.a + _tempMatrix.d * pt.c;wt.d = _tempMatrix.c * pt.b + _tempMatrix.d * pt.d;wt.tx = _tempMatrix.tx * pt.a + _tempMatrix.ty * pt.c + pt.tx;wt.ty = _tempMatrix.tx * pt.b + _tempMatrix.ty * pt.d + pt.ty;}else {if(this.rotation % CONST.PI_2){if(this.rotation !== this.rotationCache){this.rotationCache = this.rotation;this._sr = Math.sin(this.rotation);this._cr = Math.cos(this.rotation);}a = this._cr * this.scale.x;b = this._sr * this.scale.x;c = -this._sr * this.scale.y;d = this._cr * this.scale.y;tx = this.position.x;ty = this.position.y;if(this.pivot.x || this.pivot.y){tx -= this.pivot.x * a + this.pivot.y * c;ty -= this.pivot.x * b + this.pivot.y * d;}wt.a = a * pt.a + b * pt.c;wt.b = a * pt.b + b * pt.d;wt.c = c * pt.a + d * pt.c;wt.d = c * pt.b + d * pt.d;wt.tx = tx * pt.a + ty * pt.c + pt.tx;wt.ty = tx * pt.b + ty * pt.d + pt.ty;}else {a = this.scale.x;d = this.scale.y;tx = this.position.x - this.pivot.x * a;ty = this.position.y - this.pivot.y * d;wt.a = a * pt.a;wt.b = a * pt.b;wt.c = d * pt.c;wt.d = d * pt.d;wt.tx = tx * pt.a + ty * pt.c + pt.tx;wt.ty = tx * pt.b + ty * pt.d + pt.ty;}}this.worldAlpha = this.alpha * this.parent.worldAlpha;this._currentBounds = null;};DisplayObject.prototype.displayObjectUpdateTransform = DisplayObject.prototype.updateTransform;DisplayObject.prototype.getBounds = function(matrix){return math.Rectangle.EMPTY;};DisplayObject.prototype.getLocalBounds = function(){return this.getBounds(math.Matrix.IDENTITY);};DisplayObject.prototype.toGlobal = function(position){if(!this.parent){this.parent = _tempDisplayObjectParent;this.displayObjectUpdateTransform();this.parent = null;}else {this.displayObjectUpdateTransform();}return this.worldTransform.apply(position);};DisplayObject.prototype.toLocal = function(position, from, point){if(from){position = from.toGlobal(position);}if(!this.parent){this.parent = _tempDisplayObjectParent;this.displayObjectUpdateTransform();this.parent = null;}else {this.displayObjectUpdateTransform();}return this.worldTransform.applyInverse(position, point);};DisplayObject.prototype.renderWebGL = function(renderer){};DisplayObject.prototype.renderCanvas = function(renderer){};DisplayObject.prototype.generateTexture = function(renderer, scaleMode, resolution){var bounds=this.getLocalBounds();var renderTexture=new RenderTexture(renderer, bounds.width | 0, bounds.height | 0, scaleMode, resolution);_tempMatrix.tx = -bounds.x;_tempMatrix.ty = -bounds.y;renderTexture.render(this, _tempMatrix);return renderTexture;};DisplayObject.prototype.setParent = function(container){if(!container || !container.addChild){throw new Error("setParent: Argument must be a Container");}container.addChild(this);return container;};DisplayObject.prototype.setTransform = function(x, y, scaleX, scaleY, rotation, skewX, skewY, pivotX, pivotY){this.position.x = x || 0;this.position.y = y || 0;this.scale.x = !scaleX?1:scaleX;this.scale.y = !scaleY?1:scaleY;this.rotation = rotation || 0;this.skew.x = skewX || 0;this.skew.y = skewY || 0;this.pivot.x = pivotX || 0;this.pivot.y = pivotY || 0;return this;};DisplayObject.prototype.destroy = function(){this.position = null;this.scale = null;this.pivot = null;this.skew = null;this.parent = null;this._bounds = null;this._currentBounds = null;this._mask = null;this.worldTransform = null;this.filterArea = null;};}, {"../const":22, "../math":33, "../textures/RenderTexture":71, eventemitter3:10}], 25:[function(require, module, exports){var Container=require("../display/Container"), Texture=require("../textures/Texture"), CanvasBuffer=require("../renderers/canvas/utils/CanvasBuffer"), CanvasGraphics=require("../renderers/canvas/utils/CanvasGraphics"), GraphicsData=require("./GraphicsData"), math=require("../math"), CONST=require("../const"), tempPoint=new math.Point();function Graphics(){Container.call(this);this.fillAlpha = 1;this.lineWidth = 0;this.lineColor = 0;this.graphicsData = [];this.tint = 16777215;this._prevTint = 16777215;this.blendMode = CONST.BLEND_MODES.NORMAL;this.currentPath = null;this._webGL = {};this.isMask = false;this.boundsPadding = 0;this._localBounds = new math.Rectangle(0, 0, 1, 1);this.dirty = true;this.glDirty = false;this.boundsDirty = true;this.cachedSpriteDirty = false;}Graphics.prototype = Object.create(Container.prototype);Graphics.prototype.constructor = Graphics;module.exports = Graphics;Graphics.prototype.clone = function(){var clone=new Graphics();clone.renderable = this.renderable;clone.fillAlpha = this.fillAlpha;clone.lineWidth = this.lineWidth;clone.lineColor = this.lineColor;clone.tint = this.tint;clone.blendMode = this.blendMode;clone.isMask = this.isMask;clone.boundsPadding = this.boundsPadding;clone.dirty = true;clone.glDirty = true;clone.cachedSpriteDirty = this.cachedSpriteDirty;for(var i=0; i < this.graphicsData.length; ++i) {clone.graphicsData.push(this.graphicsData[i].clone());}clone.currentPath = clone.graphicsData[clone.graphicsData.length - 1];clone.updateLocalBounds();return clone;};Graphics.prototype.lineStyle = function(lineWidth, color, alpha){this.lineWidth = lineWidth || 0;this.lineColor = color || 0;this.lineAlpha = alpha === undefined?1:alpha;if(this.currentPath){if(this.currentPath.shape.points.length){var shape=new math.Polygon(this.currentPath.shape.points.slice(-2));shape.closed = false;this.drawShape(shape);}else {this.currentPath.lineWidth = this.lineWidth;this.currentPath.lineColor = this.lineColor;this.currentPath.lineAlpha = this.lineAlpha;}}return this;};Graphics.prototype.moveTo = function(x, y){var shape=new math.Polygon([x, y]);shape.closed = false;this.drawShape(shape);return this;};Graphics.prototype.lineTo = function(x, y){this.currentPath.shape.points.push(x, y);this.dirty = true;return this;};Graphics.prototype.quadraticCurveTo = function(cpX, cpY, toX, toY){if(this.currentPath){if(this.currentPath.shape.points.length === 0){this.currentPath.shape.points = [0, 0];}}else {this.moveTo(0, 0);}var xa, ya, n=20, points=this.currentPath.shape.points;if(points.length === 0){this.moveTo(0, 0);}var fromX=points[points.length - 2];var fromY=points[points.length - 1];var j=0;for(var i=1; i <= n; ++i) {j = i / n;xa = fromX + (cpX - fromX) * j;ya = fromY + (cpY - fromY) * j;points.push(xa + (cpX + (toX - cpX) * j - xa) * j, ya + (cpY + (toY - cpY) * j - ya) * j);}this.dirty = this.boundsDirty = true;return this;};Graphics.prototype.bezierCurveTo = function(cpX, cpY, cpX2, cpY2, toX, toY){if(this.currentPath){if(this.currentPath.shape.points.length === 0){this.currentPath.shape.points = [0, 0];}}else {this.moveTo(0, 0);}var n=20, dt, dt2, dt3, t2, t3, points=this.currentPath.shape.points;var fromX=points[points.length - 2];var fromY=points[points.length - 1];var j=0;for(var i=1; i <= n; ++i) {j = i / n;dt = 1 - j;dt2 = dt * dt;dt3 = dt2 * dt;t2 = j * j;t3 = t2 * j;points.push(dt3 * fromX + 3 * dt2 * j * cpX + 3 * dt * t2 * cpX2 + t3 * toX, dt3 * fromY + 3 * dt2 * j * cpY + 3 * dt * t2 * cpY2 + t3 * toY);}this.dirty = this.boundsDirty = true;return this;};Graphics.prototype.arcTo = function(x1, y1, x2, y2, radius){if(this.currentPath){if(this.currentPath.shape.points.length === 0){this.currentPath.shape.points.push(x1, y1);}}else {this.moveTo(x1, y1);}var points=this.currentPath.shape.points, fromX=points[points.length - 2], fromY=points[points.length - 1], a1=fromY - y1, b1=fromX - x1, a2=y2 - y1, b2=x2 - x1, mm=Math.abs(a1 * b2 - b1 * a2);if(mm < 1e-8 || radius === 0){if(points[points.length - 2] !== x1 || points[points.length - 1] !== y1){points.push(x1, y1);}}else {var dd=a1 * a1 + b1 * b1, cc=a2 * a2 + b2 * b2, tt=a1 * a2 + b1 * b2, k1=radius * Math.sqrt(dd) / mm, k2=radius * Math.sqrt(cc) / mm, j1=k1 * tt / dd, j2=k2 * tt / cc, cx=k1 * b2 + k2 * b1, cy=k1 * a2 + k2 * a1, px=b1 * (k2 + j1), py=a1 * (k2 + j1), qx=b2 * (k1 + j2), qy=a2 * (k1 + j2), startAngle=Math.atan2(py - cy, px - cx), endAngle=Math.atan2(qy - cy, qx - cx);this.arc(cx + x1, cy + y1, radius, startAngle, endAngle, b1 * a2 > b2 * a1);}this.dirty = this.boundsDirty = true;return this;};Graphics.prototype.arc = function(cx, cy, radius, startAngle, endAngle, anticlockwise){anticlockwise = anticlockwise || false;if(startAngle === endAngle){return this;}if(!anticlockwise && endAngle <= startAngle){endAngle += Math.PI * 2;}else if(anticlockwise && startAngle <= endAngle){startAngle += Math.PI * 2;}var sweep=anticlockwise?(startAngle - endAngle) * -1:endAngle - startAngle;var segs=Math.ceil(Math.abs(sweep) / (Math.PI * 2)) * 40;if(sweep === 0){return this;}var startX=cx + Math.cos(startAngle) * radius;var startY=cy + Math.sin(startAngle) * radius;if(this.currentPath){this.currentPath.shape.points.push(startX, startY);}else {this.moveTo(startX, startY);}var points=this.currentPath.shape.points;var theta=sweep / (segs * 2);var theta2=theta * 2;var cTheta=Math.cos(theta);var sTheta=Math.sin(theta);var segMinus=segs - 1;var remainder=segMinus % 1 / segMinus;for(var i=0; i <= segMinus; i++) {var real=i + remainder * i;var angle=theta + startAngle + theta2 * real;var c=Math.cos(angle);var s=-Math.sin(angle);points.push((cTheta * c + sTheta * s) * radius + cx, (cTheta * -s + sTheta * c) * radius + cy);}this.dirty = this.boundsDirty = true;return this;};Graphics.prototype.beginFill = function(color, alpha){this.filling = true;this.fillColor = color || 0;this.fillAlpha = alpha === undefined?1:alpha;if(this.currentPath){if(this.currentPath.shape.points.length <= 2){this.currentPath.fill = this.filling;this.currentPath.fillColor = this.fillColor;this.currentPath.fillAlpha = this.fillAlpha;}}return this;};Graphics.prototype.endFill = function(){this.filling = false;this.fillColor = null;this.fillAlpha = 1;return this;};Graphics.prototype.drawRect = function(x, y, width, height){this.drawShape(new math.Rectangle(x, y, width, height));return this;};Graphics.prototype.drawRoundedRect = function(x, y, width, height, radius){this.drawShape(new math.RoundedRectangle(x, y, width, height, radius));return this;};Graphics.prototype.drawCircle = function(x, y, radius){this.drawShape(new math.Circle(x, y, radius));return this;};Graphics.prototype.drawEllipse = function(x, y, width, height){this.drawShape(new math.Ellipse(x, y, width, height));return this;};Graphics.prototype.drawPolygon = function(path){var points=path;var closed=true;if(points instanceof math.Polygon){closed = points.closed;points = points.points;}if(!Array.isArray(points)){points = new Array(arguments.length);for(var i=0; i < points.length; ++i) {points[i] = arguments[i];}}var shape=new math.Polygon(points);shape.closed = closed;this.drawShape(shape);return this;};Graphics.prototype.clear = function(){this.lineWidth = 0;this.filling = false;this.dirty = true;this.clearDirty = true;this.graphicsData = [];return this;};Graphics.prototype.generateTexture = function(renderer, resolution, scaleMode){resolution = resolution || 1;var bounds=this.getLocalBounds();var canvasBuffer=new CanvasBuffer(bounds.width * resolution, bounds.height * resolution);var texture=Texture.fromCanvas(canvasBuffer.canvas, scaleMode);texture.baseTexture.resolution = resolution;canvasBuffer.context.scale(resolution, resolution);canvasBuffer.context.translate(-bounds.x, -bounds.y);CanvasGraphics.renderGraphics(this, canvasBuffer.context);return texture;};Graphics.prototype._renderWebGL = function(renderer){if(this.glDirty){this.dirty = true;this.glDirty = false;}renderer.setObjectRenderer(renderer.plugins.graphics);renderer.plugins.graphics.render(this);};Graphics.prototype._renderCanvas = function(renderer){if(this.isMask === true){return;}if(this._prevTint !== this.tint){this.dirty = true;}var context=renderer.context;var transform=this.worldTransform;var compositeOperation=renderer.blendModes[this.blendMode];if(compositeOperation !== context.globalCompositeOperation){context.globalCompositeOperation = compositeOperation;}var resolution=renderer.resolution;context.setTransform(transform.a * resolution, transform.b * resolution, transform.c * resolution, transform.d * resolution, transform.tx * resolution, transform.ty * resolution);CanvasGraphics.renderGraphics(this, context);};Graphics.prototype.getBounds = function(matrix){if(!this._currentBounds){if(!this.renderable){return math.Rectangle.EMPTY;}if(this.boundsDirty){this.updateLocalBounds();this.glDirty = true;this.cachedSpriteDirty = true;this.boundsDirty = false;}var bounds=this._localBounds;var w0=bounds.x;var w1=bounds.width + bounds.x;var h0=bounds.y;var h1=bounds.height + bounds.y;var worldTransform=matrix || this.worldTransform;var a=worldTransform.a;var b=worldTransform.b;var c=worldTransform.c;var d=worldTransform.d;var tx=worldTransform.tx;var ty=worldTransform.ty;var x1=a * w1 + c * h1 + tx;var y1=d * h1 + b * w1 + ty;var x2=a * w0 + c * h1 + tx;var y2=d * h1 + b * w0 + ty;var x3=a * w0 + c * h0 + tx;var y3=d * h0 + b * w0 + ty;var x4=a * w1 + c * h0 + tx;var y4=d * h0 + b * w1 + ty;var maxX=x1;var maxY=y1;var minX=x1;var minY=y1;minX = x2 < minX?x2:minX;minX = x3 < minX?x3:minX;minX = x4 < minX?x4:minX;minY = y2 < minY?y2:minY;minY = y3 < minY?y3:minY;minY = y4 < minY?y4:minY;maxX = x2 > maxX?x2:maxX;maxX = x3 > maxX?x3:maxX;maxX = x4 > maxX?x4:maxX;maxY = y2 > maxY?y2:maxY;maxY = y3 > maxY?y3:maxY;maxY = y4 > maxY?y4:maxY;this._bounds.x = minX;this._bounds.width = maxX - minX;this._bounds.y = minY;this._bounds.height = maxY - minY;this._currentBounds = this._bounds;}return this._currentBounds;};Graphics.prototype.containsPoint = function(point){this.worldTransform.applyInverse(point, tempPoint);var graphicsData=this.graphicsData;for(var i=0; i < graphicsData.length; i++) {var data=graphicsData[i];if(!data.fill){continue;}if(data.shape){if(data.shape.contains(tempPoint.x, tempPoint.y)){return true;}}}return false;};Graphics.prototype.updateLocalBounds = function(){var minX=Infinity;var maxX=-Infinity;var minY=Infinity;var maxY=-Infinity;if(this.graphicsData.length){var shape, points, x, y, w, h;for(var i=0; i < this.graphicsData.length; i++) {var data=this.graphicsData[i];var type=data.type;var lineWidth=data.lineWidth;shape = data.shape;if(type === CONST.SHAPES.RECT || type === CONST.SHAPES.RREC){x = shape.x - lineWidth / 2;y = shape.y - lineWidth / 2;w = shape.width + lineWidth;h = shape.height + lineWidth;minX = x < minX?x:minX;maxX = x + w > maxX?x + w:maxX;minY = y < minY?y:minY;maxY = y + h > maxY?y + h:maxY;}else if(type === CONST.SHAPES.CIRC){x = shape.x;y = shape.y;w = shape.radius + lineWidth / 2;h = shape.radius + lineWidth / 2;minX = x - w < minX?x - w:minX;maxX = x + w > maxX?x + w:maxX;minY = y - h < minY?y - h:minY;maxY = y + h > maxY?y + h:maxY;}else if(type === CONST.SHAPES.ELIP){x = shape.x;y = shape.y;w = shape.width + lineWidth / 2;h = shape.height + lineWidth / 2;minX = x - w < minX?x - w:minX;maxX = x + w > maxX?x + w:maxX;minY = y - h < minY?y - h:minY;maxY = y + h > maxY?y + h:maxY;}else {points = shape.points;for(var j=0; j < points.length; j += 2) {x = points[j];y = points[j + 1];minX = x - lineWidth < minX?x - lineWidth:minX;maxX = x + lineWidth > maxX?x + lineWidth:maxX;minY = y - lineWidth < minY?y - lineWidth:minY;maxY = y + lineWidth > maxY?y + lineWidth:maxY;}}}}else {minX = 0;maxX = 0;minY = 0;maxY = 0;}var padding=this.boundsPadding;this._localBounds.x = minX - padding;this._localBounds.width = maxX - minX + padding * 2;this._localBounds.y = minY - padding;this._localBounds.height = maxY - minY + padding * 2;};Graphics.prototype.drawShape = function(shape){if(this.currentPath){if(this.currentPath.shape.points.length <= 2){this.graphicsData.pop();}}this.currentPath = null;var data=new GraphicsData(this.lineWidth, this.lineColor, this.lineAlpha, this.fillColor, this.fillAlpha, this.filling, shape);this.graphicsData.push(data);if(data.type === CONST.SHAPES.POLY){data.shape.closed = data.shape.closed || this.filling;this.currentPath = data;}this.dirty = this.boundsDirty = true;return data;};Graphics.prototype.destroy = function(){Container.prototype.destroy.apply(this, arguments);for(var i=0; i < this.graphicsData.length; ++i) {this.graphicsData[i].destroy();}for(var id in this._webgl) {for(var j=0; j < this._webgl[id].data.length; ++j) {this._webgl[id].data[j].destroy();}}this.graphicsData = null;this.currentPath = null;this._webgl = null;this._localBounds = null;};}, {"../const":22, "../display/Container":23, "../math":33, "../renderers/canvas/utils/CanvasBuffer":45, "../renderers/canvas/utils/CanvasGraphics":46, "../textures/Texture":72, "./GraphicsData":26}], 26:[function(require, module, exports){function GraphicsData(lineWidth, lineColor, lineAlpha, fillColor, fillAlpha, fill, shape){this.lineWidth = lineWidth;this.lineColor = lineColor;this.lineAlpha = lineAlpha;this._lineTint = lineColor;this.fillColor = fillColor;this.fillAlpha = fillAlpha;this._fillTint = fillColor;this.fill = fill;this.shape = shape;this.type = shape.type;}GraphicsData.prototype.constructor = GraphicsData;module.exports = GraphicsData;GraphicsData.prototype.clone = function(){return new GraphicsData(this.lineWidth, this.lineColor, this.lineAlpha, this.fillColor, this.fillAlpha, this.fill, this.shape);};GraphicsData.prototype.destroy = function(){this.shape = null;};}, {}], 27:[function(require, module, exports){var utils=require("../../utils"), math=require("../../math"), CONST=require("../../const"), ObjectRenderer=require("../../renderers/webgl/utils/ObjectRenderer"), WebGLRenderer=require("../../renderers/webgl/WebGLRenderer"), WebGLGraphicsData=require("./WebGLGraphicsData"), earcut=require("earcut");function GraphicsRenderer(renderer){ObjectRenderer.call(this, renderer);this.graphicsDataPool = [];this.primitiveShader = null;this.complexPrimitiveShader = null;this.maximumSimplePolySize = 200;}GraphicsRenderer.prototype = Object.create(ObjectRenderer.prototype);GraphicsRenderer.prototype.constructor = GraphicsRenderer;module.exports = GraphicsRenderer;WebGLRenderer.registerPlugin("graphics", GraphicsRenderer);GraphicsRenderer.prototype.onContextChange = function(){};GraphicsRenderer.prototype.destroy = function(){ObjectRenderer.prototype.destroy.call(this);for(var i=0; i < this.graphicsDataPool.length; ++i) {this.graphicsDataPool[i].destroy();}this.graphicsDataPool = null;};GraphicsRenderer.prototype.render = function(graphics){var renderer=this.renderer;var gl=renderer.gl;var shader=renderer.shaderManager.plugins.primitiveShader, webGLData;if(graphics.dirty || !graphics._webGL[gl.id]){this.updateGraphics(graphics);}var webGL=graphics._webGL[gl.id];renderer.blendModeManager.setBlendMode(graphics.blendMode);for(var i=0, n=webGL.data.length; i < n; i++) {webGLData = webGL.data[i];if(webGL.data[i].mode === 1){renderer.stencilManager.pushStencil(graphics, webGLData);gl.uniform1f(renderer.shaderManager.complexPrimitiveShader.uniforms.alpha._location, graphics.worldAlpha * webGLData.alpha);gl.drawElements(gl.TRIANGLE_FAN, 4, gl.UNSIGNED_SHORT, (webGLData.indices.length - 4) * 2);renderer.stencilManager.popStencil(graphics, webGLData);}else {shader = renderer.shaderManager.primitiveShader;renderer.shaderManager.setShader(shader);gl.uniformMatrix3fv(shader.uniforms.translationMatrix._location, false, graphics.worldTransform.toArray(true));gl.uniformMatrix3fv(shader.uniforms.projectionMatrix._location, false, renderer.currentRenderTarget.projectionMatrix.toArray(true));gl.uniform3fv(shader.uniforms.tint._location, utils.hex2rgb(graphics.tint));gl.uniform1f(shader.uniforms.alpha._location, graphics.worldAlpha);gl.bindBuffer(gl.ARRAY_BUFFER, webGLData.buffer);gl.vertexAttribPointer(shader.attributes.aVertexPosition, 2, gl.FLOAT, false, 4 * 6, 0);gl.vertexAttribPointer(shader.attributes.aColor, 4, gl.FLOAT, false, 4 * 6, 2 * 4);gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, webGLData.indexBuffer);gl.drawElements(gl.TRIANGLE_STRIP, webGLData.indices.length, gl.UNSIGNED_SHORT, 0);}renderer.drawCount++;}};GraphicsRenderer.prototype.updateGraphics = function(graphics){var gl=this.renderer.gl;var webGL=graphics._webGL[gl.id];if(!webGL){webGL = graphics._webGL[gl.id] = {lastIndex:0, data:[], gl:gl};}graphics.dirty = false;var i;if(graphics.clearDirty){graphics.clearDirty = false;for(i = 0; i < webGL.data.length; i++) {var graphicsData=webGL.data[i];graphicsData.reset();this.graphicsDataPool.push(graphicsData);}webGL.data = [];webGL.lastIndex = 0;}var webGLData;for(i = webGL.lastIndex; i < graphics.graphicsData.length; i++) {var data=graphics.graphicsData[i];if(data.type === CONST.SHAPES.POLY){data.points = data.shape.points.slice();if(data.shape.closed){if(data.points[0] !== data.points[data.points.length - 2] || data.points[1] !== data.points[data.points.length - 1]){data.points.push(data.points[0], data.points[1]);}}if(data.fill){if(data.points.length >= 6){if(data.points.length < this.maximumSimplePolySize * 2){webGLData = this.switchMode(webGL, 0);var canDrawUsingSimple=this.buildPoly(data, webGLData);if(!canDrawUsingSimple){webGLData = this.switchMode(webGL, 1);this.buildComplexPoly(data, webGLData);}}else {webGLData = this.switchMode(webGL, 1);this.buildComplexPoly(data, webGLData);}}}if(data.lineWidth > 0){webGLData = this.switchMode(webGL, 0);this.buildLine(data, webGLData);}}else {webGLData = this.switchMode(webGL, 0);if(data.type === CONST.SHAPES.RECT){this.buildRectangle(data, webGLData);}else if(data.type === CONST.SHAPES.CIRC || data.type === CONST.SHAPES.ELIP){this.buildCircle(data, webGLData);}else if(data.type === CONST.SHAPES.RREC){this.buildRoundedRectangle(data, webGLData);}}webGL.lastIndex++;}for(i = 0; i < webGL.data.length; i++) {webGLData = webGL.data[i];if(webGLData.dirty){webGLData.upload();}}};GraphicsRenderer.prototype.switchMode = function(webGL, type){var webGLData;if(!webGL.data.length){webGLData = this.graphicsDataPool.pop() || new WebGLGraphicsData(webGL.gl);webGLData.mode = type;webGL.data.push(webGLData);}else {webGLData = webGL.data[webGL.data.length - 1];if(webGLData.points.length > 320000 || webGLData.mode !== type || type === 1){webGLData = this.graphicsDataPool.pop() || new WebGLGraphicsData(webGL.gl);webGLData.mode = type;webGL.data.push(webGLData);}}webGLData.dirty = true;return webGLData;};GraphicsRenderer.prototype.buildRectangle = function(graphicsData, webGLData){var rectData=graphicsData.shape;var x=rectData.x;var y=rectData.y;var width=rectData.width;var height=rectData.height;if(graphicsData.fill){var color=utils.hex2rgb(graphicsData.fillColor);var alpha=graphicsData.fillAlpha;var r=color[0] * alpha;var g=color[1] * alpha;var b=color[2] * alpha;var verts=webGLData.points;var indices=webGLData.indices;var vertPos=verts.length / 6;verts.push(x, y);verts.push(r, g, b, alpha);verts.push(x + width, y);verts.push(r, g, b, alpha);verts.push(x, y + height);verts.push(r, g, b, alpha);verts.push(x + width, y + height);verts.push(r, g, b, alpha);indices.push(vertPos, vertPos, vertPos + 1, vertPos + 2, vertPos + 3, vertPos + 3);}if(graphicsData.lineWidth){var tempPoints=graphicsData.points;graphicsData.points = [x, y, x + width, y, x + width, y + height, x, y + height, x, y];this.buildLine(graphicsData, webGLData);graphicsData.points = tempPoints;}};GraphicsRenderer.prototype.buildRoundedRectangle = function(graphicsData, webGLData){var rrectData=graphicsData.shape;var x=rrectData.x;var y=rrectData.y;var width=rrectData.width;var height=rrectData.height;var radius=rrectData.radius;var recPoints=[];recPoints.push(x, y + radius);this.quadraticBezierCurve(x, y + height - radius, x, y + height, x + radius, y + height, recPoints);this.quadraticBezierCurve(x + width - radius, y + height, x + width, y + height, x + width, y + height - radius, recPoints);this.quadraticBezierCurve(x + width, y + radius, x + width, y, x + width - radius, y, recPoints);this.quadraticBezierCurve(x + radius, y, x, y, x, y + radius + 1e-10, recPoints);if(graphicsData.fill){var color=utils.hex2rgb(graphicsData.fillColor);var alpha=graphicsData.fillAlpha;var r=color[0] * alpha;var g=color[1] * alpha;var b=color[2] * alpha;var verts=webGLData.points;var indices=webGLData.indices;var vecPos=verts.length / 6;var triangles=earcut(recPoints, null, 2);var i=0;for(i = 0; i < triangles.length; i += 3) {indices.push(triangles[i] + vecPos);indices.push(triangles[i] + vecPos);indices.push(triangles[i + 1] + vecPos);indices.push(triangles[i + 2] + vecPos);indices.push(triangles[i + 2] + vecPos);}for(i = 0; i < recPoints.length; i++) {verts.push(recPoints[i], recPoints[++i], r, g, b, alpha);}}if(graphicsData.lineWidth){var tempPoints=graphicsData.points;graphicsData.points = recPoints;this.buildLine(graphicsData, webGLData);graphicsData.points = tempPoints;}};GraphicsRenderer.prototype.quadraticBezierCurve = function(fromX, fromY, cpX, cpY, toX, toY, out){var xa, ya, xb, yb, x, y, n=20, points=out || [];function getPt(n1, n2, perc){var diff=n2 - n1;return n1 + diff * perc;}var j=0;for(var i=0; i <= n; i++) {j = i / n;xa = getPt(fromX, cpX, j);ya = getPt(fromY, cpY, j);xb = getPt(cpX, toX, j);yb = getPt(cpY, toY, j);x = getPt(xa, xb, j);y = getPt(ya, yb, j);points.push(x, y);}return points;};GraphicsRenderer.prototype.buildCircle = function(graphicsData, webGLData){var circleData=graphicsData.shape;var x=circleData.x;var y=circleData.y;var width;var height;if(graphicsData.type === CONST.SHAPES.CIRC){width = circleData.radius;height = circleData.radius;}else {width = circleData.width;height = circleData.height;}var totalSegs=Math.floor(30 * Math.sqrt(circleData.radius)) || Math.floor(15 * Math.sqrt(circleData.width + circleData.height));var seg=Math.PI * 2 / totalSegs;var i=0;if(graphicsData.fill){var color=utils.hex2rgb(graphicsData.fillColor);var alpha=graphicsData.fillAlpha;var r=color[0] * alpha;var g=color[1] * alpha;var b=color[2] * alpha;var verts=webGLData.points;var indices=webGLData.indices;var vecPos=verts.length / 6;indices.push(vecPos);for(i = 0; i < totalSegs + 1; i++) {verts.push(x, y, r, g, b, alpha);verts.push(x + Math.sin(seg * i) * width, y + Math.cos(seg * i) * height, r, g, b, alpha);indices.push(vecPos++, vecPos++);}indices.push(vecPos - 1);}if(graphicsData.lineWidth){var tempPoints=graphicsData.points;graphicsData.points = [];for(i = 0; i < totalSegs + 1; i++) {graphicsData.points.push(x + Math.sin(seg * i) * width, y + Math.cos(seg * i) * height);}this.buildLine(graphicsData, webGLData);graphicsData.points = tempPoints;}};GraphicsRenderer.prototype.buildLine = function(graphicsData, webGLData){var i=0;var points=graphicsData.points;if(points.length === 0){return;}var firstPoint=new math.Point(points[0], points[1]);var lastPoint=new math.Point(points[points.length - 2], points[points.length - 1]);if(firstPoint.x === lastPoint.x && firstPoint.y === lastPoint.y){points = points.slice();points.pop();points.pop();lastPoint = new math.Point(points[points.length - 2], points[points.length - 1]);var midPointX=lastPoint.x + (firstPoint.x - lastPoint.x) * 0.5;var midPointY=lastPoint.y + (firstPoint.y - lastPoint.y) * 0.5;points.unshift(midPointX, midPointY);points.push(midPointX, midPointY);}var verts=webGLData.points;var indices=webGLData.indices;var length=points.length / 2;var indexCount=points.length;var indexStart=verts.length / 6;var width=graphicsData.lineWidth / 2;var color=utils.hex2rgb(graphicsData.lineColor);var alpha=graphicsData.lineAlpha;var r=color[0] * alpha;var g=color[1] * alpha;var b=color[2] * alpha;var px, py, p1x, p1y, p2x, p2y, p3x, p3y;var perpx, perpy, perp2x, perp2y, perp3x, perp3y;var a1, b1, c1, a2, b2, c2;var denom, pdist, dist;p1x = points[0];p1y = points[1];p2x = points[2];p2y = points[3];perpx = -(p1y - p2y);perpy = p1x - p2x;dist = Math.sqrt(perpx * perpx + perpy * perpy);perpx /= dist;perpy /= dist;perpx *= width;perpy *= width;verts.push(p1x - perpx, p1y - perpy, r, g, b, alpha);verts.push(p1x + perpx, p1y + perpy, r, g, b, alpha);for(i = 1; i < length - 1; i++) {p1x = points[(i - 1) * 2];p1y = points[(i - 1) * 2 + 1];p2x = points[i * 2];p2y = points[i * 2 + 1];p3x = points[(i + 1) * 2];p3y = points[(i + 1) * 2 + 1];perpx = -(p1y - p2y);perpy = p1x - p2x;dist = Math.sqrt(perpx * perpx + perpy * perpy);perpx /= dist;perpy /= dist;perpx *= width;perpy *= width;perp2x = -(p2y - p3y);perp2y = p2x - p3x;dist = Math.sqrt(perp2x * perp2x + perp2y * perp2y);perp2x /= dist;perp2y /= dist;perp2x *= width;perp2y *= width;a1 = -perpy + p1y - (-perpy + p2y);b1 = -perpx + p2x - (-perpx + p1x);c1 = (-perpx + p1x) * (-perpy + p2y) - (-perpx + p2x) * (-perpy + p1y);a2 = -perp2y + p3y - (-perp2y + p2y);b2 = -perp2x + p2x - (-perp2x + p3x);c2 = (-perp2x + p3x) * (-perp2y + p2y) - (-perp2x + p2x) * (-perp2y + p3y);denom = a1 * b2 - a2 * b1;if(Math.abs(denom) < 0.1){denom += 10.1;verts.push(p2x - perpx, p2y - perpy, r, g, b, alpha);verts.push(p2x + perpx, p2y + perpy, r, g, b, alpha);continue;}px = (b1 * c2 - b2 * c1) / denom;py = (a2 * c1 - a1 * c2) / denom;pdist = (px - p2x) * (px - p2x) + (py - p2y) * (py - p2y);if(pdist > 140 * 140){perp3x = perpx - perp2x;perp3y = perpy - perp2y;dist = Math.sqrt(perp3x * perp3x + perp3y * perp3y);perp3x /= dist;perp3y /= dist;perp3x *= width;perp3y *= width;verts.push(p2x - perp3x, p2y - perp3y);verts.push(r, g, b, alpha);verts.push(p2x + perp3x, p2y + perp3y);verts.push(r, g, b, alpha);verts.push(p2x - perp3x, p2y - perp3y);verts.push(r, g, b, alpha);indexCount++;}else {verts.push(px, py);verts.push(r, g, b, alpha);verts.push(p2x - (px - p2x), p2y - (py - p2y));verts.push(r, g, b, alpha);}}p1x = points[(length - 2) * 2];p1y = points[(length - 2) * 2 + 1];p2x = points[(length - 1) * 2];p2y = points[(length - 1) * 2 + 1];perpx = -(p1y - p2y);perpy = p1x - p2x;dist = Math.sqrt(perpx * perpx + perpy * perpy);perpx /= dist;perpy /= dist;perpx *= width;perpy *= width;verts.push(p2x - perpx, p2y - perpy);verts.push(r, g, b, alpha);verts.push(p2x + perpx, p2y + perpy);verts.push(r, g, b, alpha);indices.push(indexStart);for(i = 0; i < indexCount; i++) {indices.push(indexStart++);}indices.push(indexStart - 1);};GraphicsRenderer.prototype.buildComplexPoly = function(graphicsData, webGLData){var points=graphicsData.points.slice();if(points.length < 6){return;}var indices=webGLData.indices;webGLData.points = points;webGLData.alpha = graphicsData.fillAlpha;webGLData.color = utils.hex2rgb(graphicsData.fillColor);var minX=Infinity;var maxX=-Infinity;var minY=Infinity;var maxY=-Infinity;var x, y;for(var i=0; i < points.length; i += 2) {x = points[i];y = points[i + 1];minX = x < minX?x:minX;maxX = x > maxX?x:maxX;minY = y < minY?y:minY;maxY = y > maxY?y:maxY;}points.push(minX, minY, maxX, minY, maxX, maxY, minX, maxY);var length=points.length / 2;for(i = 0; i < length; i++) {indices.push(i);}};GraphicsRenderer.prototype.buildPoly = function(graphicsData, webGLData){var points=graphicsData.points;if(points.length < 6){return;}var verts=webGLData.points;var indices=webGLData.indices;var length=points.length / 2;var color=utils.hex2rgb(graphicsData.fillColor);var alpha=graphicsData.fillAlpha;var r=color[0] * alpha;var g=color[1] * alpha;var b=color[2] * alpha;var triangles=earcut(points, null, 2);if(!triangles){return false;}var vertPos=verts.length / 6;var i=0;for(i = 0; i < triangles.length; i += 3) {indices.push(triangles[i] + vertPos);indices.push(triangles[i] + vertPos);indices.push(triangles[i + 1] + vertPos);indices.push(triangles[i + 2] + vertPos);indices.push(triangles[i + 2] + vertPos);}for(i = 0; i < length; i++) {verts.push(points[i * 2], points[i * 2 + 1], r, g, b, alpha);}return true;};}, {"../../const":22, "../../math":33, "../../renderers/webgl/WebGLRenderer":49, "../../renderers/webgl/utils/ObjectRenderer":63, "../../utils":77, "./WebGLGraphicsData":28, earcut:9}], 28:[function(require, module, exports){function WebGLGraphicsData(gl){this.gl = gl;this.color = [0, 0, 0];this.points = [];this.indices = [];this.buffer = gl.createBuffer();this.indexBuffer = gl.createBuffer();this.mode = 1;this.alpha = 1;this.dirty = true;this.glPoints = null;this.glIndices = null;}WebGLGraphicsData.prototype.constructor = WebGLGraphicsData;module.exports = WebGLGraphicsData;WebGLGraphicsData.prototype.reset = function(){this.points.length = 0;this.indices.length = 0;};WebGLGraphicsData.prototype.upload = function(){var gl=this.gl;this.glPoints = new Float32Array(this.points);gl.bindBuffer(gl.ARRAY_BUFFER, this.buffer);gl.bufferData(gl.ARRAY_BUFFER, this.glPoints, gl.STATIC_DRAW);this.glIndices = new Uint16Array(this.indices);gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.indexBuffer);gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, this.glIndices, gl.STATIC_DRAW);this.dirty = false;};WebGLGraphicsData.prototype.destroy = function(){this.color = null;this.points = null;this.indices = null;this.gl.deleteBuffer(this.buffer);this.gl.deleteBuffer(this.indexBuffer);this.gl = null;this.buffer = null;this.indexBuffer = null;this.glPoints = null;this.glIndices = null;};}, {}], 29:[function(require, module, exports){var core=module.exports = Object.assign(require("./const"), require("./math"), {utils:require("./utils"), ticker:require("./ticker"), DisplayObject:require("./display/DisplayObject"), Container:require("./display/Container"), Sprite:require("./sprites/Sprite"), ParticleContainer:require("./particles/ParticleContainer"), SpriteRenderer:require("./sprites/webgl/SpriteRenderer"), ParticleRenderer:require("./particles/webgl/ParticleRenderer"), Text:require("./text/Text"), Graphics:require("./graphics/Graphics"), GraphicsData:require("./graphics/GraphicsData"), GraphicsRenderer:require("./graphics/webgl/GraphicsRenderer"), Texture:require("./textures/Texture"), BaseTexture:require("./textures/BaseTexture"), RenderTexture:require("./textures/RenderTexture"), VideoBaseTexture:require("./textures/VideoBaseTexture"), TextureUvs:require("./textures/TextureUvs"), CanvasRenderer:require("./renderers/canvas/CanvasRenderer"), CanvasGraphics:require("./renderers/canvas/utils/CanvasGraphics"), CanvasBuffer:require("./renderers/canvas/utils/CanvasBuffer"), WebGLRenderer:require("./renderers/webgl/WebGLRenderer"), WebGLManager:require("./renderers/webgl/managers/WebGLManager"), ShaderManager:require("./renderers/webgl/managers/ShaderManager"), Shader:require("./renderers/webgl/shaders/Shader"), TextureShader:require("./renderers/webgl/shaders/TextureShader"), PrimitiveShader:require("./renderers/webgl/shaders/PrimitiveShader"), ComplexPrimitiveShader:require("./renderers/webgl/shaders/ComplexPrimitiveShader"), ObjectRenderer:require("./renderers/webgl/utils/ObjectRenderer"), RenderTarget:require("./renderers/webgl/utils/RenderTarget"), AbstractFilter:require("./renderers/webgl/filters/AbstractFilter"), FXAAFilter:require("./renderers/webgl/filters/FXAAFilter"), SpriteMaskFilter:require("./renderers/webgl/filters/SpriteMaskFilter"), autoDetectRenderer:function autoDetectRenderer(width, height, options, noWebGL){width = width || 800;height = height || 600;if(!noWebGL && core.utils.isWebGLSupported()){return new core.WebGLRenderer(width, height, options);}return new core.CanvasRenderer(width, height, options);}});}, {"./const":22, "./display/Container":23, "./display/DisplayObject":24, "./graphics/Graphics":25, "./graphics/GraphicsData":26, "./graphics/webgl/GraphicsRenderer":27, "./math":33, "./particles/ParticleContainer":39, "./particles/webgl/ParticleRenderer":41, "./renderers/canvas/CanvasRenderer":44, "./renderers/canvas/utils/CanvasBuffer":45, "./renderers/canvas/utils/CanvasGraphics":46, "./renderers/webgl/WebGLRenderer":49, "./renderers/webgl/filters/AbstractFilter":50, "./renderers/webgl/filters/FXAAFilter":51, "./renderers/webgl/filters/SpriteMaskFilter":52, "./renderers/webgl/managers/ShaderManager":56, "./renderers/webgl/managers/WebGLManager":58, "./renderers/webgl/shaders/ComplexPrimitiveShader":59, "./renderers/webgl/shaders/PrimitiveShader":60, "./renderers/webgl/shaders/Shader":61, "./renderers/webgl/shaders/TextureShader":62, "./renderers/webgl/utils/ObjectRenderer":63, "./renderers/webgl/utils/RenderTarget":65, "./sprites/Sprite":67, "./sprites/webgl/SpriteRenderer":68, "./text/Text":69, "./textures/BaseTexture":70, "./textures/RenderTexture":71, "./textures/Texture":72, "./textures/TextureUvs":73, "./textures/VideoBaseTexture":74, "./ticker":76, "./utils":77}], 30:[function(require, module, exports){var ux=[1, 1, 0, -1, -1, -1, 0, 1, 1, 1, 0, -1, -1, -1, 0, 1];var uy=[0, 1, 1, 1, 0, -1, -1, -1, 0, 1, 1, 1, 0, -1, -1, -1];var vx=[0, -1, -1, -1, 0, 1, 1, 1, 0, 1, 1, 1, 0, -1, -1, -1];var vy=[1, 1, 0, -1, -1, -1, 0, 1, -1, -1, 0, 1, 1, 1, 0, -1];var tempMatrices=[];var Matrix=require("./Matrix");var mul=[];function signum(x){if(x < 0){return -1;}if(x > 0){return 1;}return 0;}function init(){for(var i=0; i < 16; i++) {var row=[];mul.push(row);for(var j=0; j < 16; j++) {var _ux=signum(ux[i] * ux[j] + vx[i] * uy[j]);var _uy=signum(uy[i] * ux[j] + vy[i] * uy[j]);var _vx=signum(ux[i] * vx[j] + vx[i] * vy[j]);var _vy=signum(uy[i] * vx[j] + vy[i] * vy[j]);for(var k=0; k < 16; k++) {if(ux[k] === _ux && uy[k] === _uy && vx[k] === _vx && vy[k] === _vy){row.push(k);break;}}}}for(i = 0; i < 16; i++) {var mat=new Matrix();mat.set(ux[i], uy[i], vx[i], vy[i], 0, 0);tempMatrices.push(mat);}}init();var GroupD8={E:0, SE:1, S:2, SW:3, W:4, NW:5, N:6, NE:7, MIRROR_VERTICAL:8, MIRROR_HORIZONTAL:12, uX:function uX(ind){return ux[ind];}, uY:function uY(ind){return uy[ind];}, vX:function vX(ind){return vx[ind];}, vY:function vY(ind){return vy[ind];}, inv:function inv(rotation){if(rotation & 8){return rotation & 15;}return -rotation & 7;}, add:function add(rotationSecond, rotationFirst){return mul[rotationSecond][rotationFirst];}, sub:function sub(rotationSecond, rotationFirst){return mul[rotationSecond][GroupD8.inv(rotationFirst)];}, rotate180:function rotate180(rotation){return rotation ^ 4;}, isSwapWidthHeight:function isSwapWidthHeight(rotation){return (rotation & 3) === 2;}, byDirection:function byDirection(dx, dy){if(Math.abs(dx) * 2 <= Math.abs(dy)){if(dy >= 0){return GroupD8.S;}else {return GroupD8.N;}}else if(Math.abs(dy) * 2 <= Math.abs(dx)){if(dx > 0){return GroupD8.E;}else {return GroupD8.W;}}else {if(dy > 0){if(dx > 0){return GroupD8.SE;}else {return GroupD8.SW;}}else if(dx > 0){return GroupD8.NE;}else {return GroupD8.NW;}}}, matrixAppendRotationInv:function matrixAppendRotationInv(matrix, rotation, tx, ty){var mat=tempMatrices[GroupD8.inv(rotation)];tx = tx || 0;ty = ty || 0;mat.tx = tx;mat.ty = ty;matrix.append(mat);}};module.exports = GroupD8;}, {"./Matrix":31}], 31:[function(require, module, exports){var Point=require("./Point");function Matrix(){this.a = 1;this.b = 0;this.c = 0;this.d = 1;this.tx = 0;this.ty = 0;}Matrix.prototype.constructor = Matrix;module.exports = Matrix;Matrix.prototype.fromArray = function(array){this.a = array[0];this.b = array[1];this.c = array[3];this.d = array[4];this.tx = array[2];this.ty = array[5];};Matrix.prototype.set = function(a, b, c, d, tx, ty){this.a = a;this.b = b;this.c = c;this.d = d;this.tx = tx;this.ty = ty;return this;};Matrix.prototype.toArray = function(transpose, out){if(!this.array){this.array = new Float32Array(9);}var array=out || this.array;if(transpose){array[0] = this.a;array[1] = this.b;array[2] = 0;array[3] = this.c;array[4] = this.d;array[5] = 0;array[6] = this.tx;array[7] = this.ty;array[8] = 1;}else {array[0] = this.a;array[1] = this.c;array[2] = this.tx;array[3] = this.b;array[4] = this.d;array[5] = this.ty;array[6] = 0;array[7] = 0;array[8] = 1;}return array;};Matrix.prototype.apply = function(pos, newPos){newPos = newPos || new Point();var x=pos.x;var y=pos.y;newPos.x = this.a * x + this.c * y + this.tx;newPos.y = this.b * x + this.d * y + this.ty;return newPos;};Matrix.prototype.applyInverse = function(pos, newPos){newPos = newPos || new Point();var id=1 / (this.a * this.d + this.c * -this.b);var x=pos.x;var y=pos.y;newPos.x = this.d * id * x + -this.c * id * y + (this.ty * this.c - this.tx * this.d) * id;newPos.y = this.a * id * y + -this.b * id * x + (-this.ty * this.a + this.tx * this.b) * id;return newPos;};Matrix.prototype.translate = function(x, y){this.tx += x;this.ty += y;return this;};Matrix.prototype.scale = function(x, y){this.a *= x;this.d *= y;this.c *= x;this.b *= y;this.tx *= x;this.ty *= y;return this;};Matrix.prototype.rotate = function(angle){var cos=Math.cos(angle);var sin=Math.sin(angle);var a1=this.a;var c1=this.c;var tx1=this.tx;this.a = a1 * cos - this.b * sin;this.b = a1 * sin + this.b * cos;this.c = c1 * cos - this.d * sin;this.d = c1 * sin + this.d * cos;this.tx = tx1 * cos - this.ty * sin;this.ty = tx1 * sin + this.ty * cos;return this;};Matrix.prototype.append = function(matrix){var a1=this.a;var b1=this.b;var c1=this.c;var d1=this.d;this.a = matrix.a * a1 + matrix.b * c1;this.b = matrix.a * b1 + matrix.b * d1;this.c = matrix.c * a1 + matrix.d * c1;this.d = matrix.c * b1 + matrix.d * d1;this.tx = matrix.tx * a1 + matrix.ty * c1 + this.tx;this.ty = matrix.tx * b1 + matrix.ty * d1 + this.ty;return this;};Matrix.prototype.setTransform = function(x, y, pivotX, pivotY, scaleX, scaleY, rotation, skewX, skewY){var a, b, c, d, sr, cr, cy, sy, nsx, cx;sr = Math.sin(rotation);cr = Math.cos(rotation);cy = Math.cos(skewY);sy = Math.sin(skewY);nsx = -Math.sin(skewX);cx = Math.cos(skewX);a = cr * scaleX;b = sr * scaleX;c = -sr * scaleY;d = cr * scaleY;this.a = cy * a + sy * c;this.b = cy * b + sy * d;this.c = nsx * a + cx * c;this.d = nsx * b + cx * d;this.tx = x + (pivotX * a + pivotY * c);this.ty = y + (pivotX * b + pivotY * d);return this;};Matrix.prototype.prepend = function(matrix){var tx1=this.tx;if(matrix.a !== 1 || matrix.b !== 0 || matrix.c !== 0 || matrix.d !== 1){var a1=this.a;var c1=this.c;this.a = a1 * matrix.a + this.b * matrix.c;this.b = a1 * matrix.b + this.b * matrix.d;this.c = c1 * matrix.a + this.d * matrix.c;this.d = c1 * matrix.b + this.d * matrix.d;}this.tx = tx1 * matrix.a + this.ty * matrix.c + matrix.tx;this.ty = tx1 * matrix.b + this.ty * matrix.d + matrix.ty;return this;};Matrix.prototype.invert = function(){var a1=this.a;var b1=this.b;var c1=this.c;var d1=this.d;var tx1=this.tx;var n=a1 * d1 - b1 * c1;this.a = d1 / n;this.b = -b1 / n;this.c = -c1 / n;this.d = a1 / n;this.tx = (c1 * this.ty - d1 * tx1) / n;this.ty = -(a1 * this.ty - b1 * tx1) / n;return this;};Matrix.prototype.identity = function(){this.a = 1;this.b = 0;this.c = 0;this.d = 1;this.tx = 0;this.ty = 0;return this;};Matrix.prototype.clone = function(){var matrix=new Matrix();matrix.a = this.a;matrix.b = this.b;matrix.c = this.c;matrix.d = this.d;matrix.tx = this.tx;matrix.ty = this.ty;return matrix;};Matrix.prototype.copy = function(matrix){matrix.a = this.a;matrix.b = this.b;matrix.c = this.c;matrix.d = this.d;matrix.tx = this.tx;matrix.ty = this.ty;return matrix;};Matrix.IDENTITY = new Matrix();Matrix.TEMP_MATRIX = new Matrix();}, {"./Point":32}], 32:[function(require, module, exports){function Point(x, y){this.x = x || 0;this.y = y || 0;}Point.prototype.constructor = Point;module.exports = Point;Point.prototype.clone = function(){return new Point(this.x, this.y);};Point.prototype.copy = function(p){this.set(p.x, p.y);};Point.prototype.equals = function(p){return p.x === this.x && p.y === this.y;};Point.prototype.set = function(x, y){this.x = x || 0;this.y = y || (y !== 0?this.x:0);};}, {}], 33:[function(require, module, exports){module.exports = {Point:require("./Point"), Matrix:require("./Matrix"), GroupD8:require("./GroupD8"), Circle:require("./shapes/Circle"), Ellipse:require("./shapes/Ellipse"), Polygon:require("./shapes/Polygon"), Rectangle:require("./shapes/Rectangle"), RoundedRectangle:require("./shapes/RoundedRectangle")};}, {"./GroupD8":30, "./Matrix":31, "./Point":32, "./shapes/Circle":34, "./shapes/Ellipse":35, "./shapes/Polygon":36, "./shapes/Rectangle":37, "./shapes/RoundedRectangle":38}], 34:[function(require, module, exports){var Rectangle=require("./Rectangle"), CONST=require("../../const");function Circle(x, y, radius){this.x = x || 0;this.y = y || 0;this.radius = radius || 0;this.type = CONST.SHAPES.CIRC;}Circle.prototype.constructor = Circle;module.exports = Circle;Circle.prototype.clone = function(){return new Circle(this.x, this.y, this.radius);};Circle.prototype.contains = function(x, y){if(this.radius <= 0){return false;}var dx=this.x - x, dy=this.y - y, r2=this.radius * this.radius;dx *= dx;dy *= dy;return dx + dy <= r2;};Circle.prototype.getBounds = function(){return new Rectangle(this.x - this.radius, this.y - this.radius, this.radius * 2, this.radius * 2);};}, {"../../const":22, "./Rectangle":37}], 35:[function(require, module, exports){var Rectangle=require("./Rectangle"), CONST=require("../../const");function Ellipse(x, y, width, height){this.x = x || 0;this.y = y || 0;this.width = width || 0;this.height = height || 0;this.type = CONST.SHAPES.ELIP;}Ellipse.prototype.constructor = Ellipse;module.exports = Ellipse;Ellipse.prototype.clone = function(){return new Ellipse(this.x, this.y, this.width, this.height);};Ellipse.prototype.contains = function(x, y){if(this.width <= 0 || this.height <= 0){return false;}var normx=(x - this.x) / this.width, normy=(y - this.y) / this.height;normx *= normx;normy *= normy;return normx + normy <= 1;};Ellipse.prototype.getBounds = function(){return new Rectangle(this.x - this.width, this.y - this.height, this.width, this.height);};}, {"../../const":22, "./Rectangle":37}], 36:[function(require, module, exports){var Point=require("../Point"), CONST=require("../../const");function Polygon(points_){var points=points_;if(!Array.isArray(points)){points = new Array(arguments.length);for(var a=0; a < points.length; ++a) {points[a] = arguments[a];}}if(points[0] instanceof Point){var p=[];for(var i=0, il=points.length; i < il; i++) {p.push(points[i].x, points[i].y);}points = p;}this.closed = true;this.points = points;this.type = CONST.SHAPES.POLY;}Polygon.prototype.constructor = Polygon;module.exports = Polygon;Polygon.prototype.clone = function(){return new Polygon(this.points.slice());};Polygon.prototype.contains = function(x, y){var inside=false;var length=this.points.length / 2;for(var i=0, j=length - 1; i < length; j = i++) {var xi=this.points[i * 2], yi=this.points[i * 2 + 1], xj=this.points[j * 2], yj=this.points[j * 2 + 1], intersect=yi > y !== yj > y && x < (xj - xi) * (y - yi) / (yj - yi) + xi;if(intersect){inside = !inside;}}return inside;};}, {"../../const":22, "../Point":32}], 37:[function(require, module, exports){var CONST=require("../../const");function Rectangle(x, y, width, height){this.x = x || 0;this.y = y || 0;this.width = width || 0;this.height = height || 0;this.type = CONST.SHAPES.RECT;}Rectangle.prototype.constructor = Rectangle;module.exports = Rectangle;Rectangle.EMPTY = new Rectangle(0, 0, 0, 0);Rectangle.prototype.clone = function(){return new Rectangle(this.x, this.y, this.width, this.height);};Rectangle.prototype.contains = function(x, y){if(this.width <= 0 || this.height <= 0){return false;}if(x >= this.x && x < this.x + this.width){if(y >= this.y && y < this.y + this.height){return true;}}return false;};}, {"../../const":22}], 38:[function(require, module, exports){var CONST=require("../../const");function RoundedRectangle(x, y, width, height, radius){this.x = x || 0;this.y = y || 0;this.width = width || 0;this.height = height || 0;this.radius = radius || 20;this.type = CONST.SHAPES.RREC;}RoundedRectangle.prototype.constructor = RoundedRectangle;module.exports = RoundedRectangle;RoundedRectangle.prototype.clone = function(){return new RoundedRectangle(this.x, this.y, this.width, this.height, this.radius);};RoundedRectangle.prototype.contains = function(x, y){if(this.width <= 0 || this.height <= 0){return false;}if(x >= this.x && x <= this.x + this.width){if(y >= this.y && y <= this.y + this.height){return true;}}return false;};}, {"../../const":22}], 39:[function(require, module, exports){var Container=require("../display/Container"), CONST=require("../const");function ParticleContainer(maxSize, properties, batchSize){Container.call(this);batchSize = batchSize || 15000;maxSize = maxSize || 15000;var maxBatchSize=16384;if(batchSize > maxBatchSize){batchSize = maxBatchSize;}if(batchSize > maxSize){batchSize = maxSize;}this._properties = [false, true, false, false, false];this._maxSize = maxSize;this._batchSize = batchSize;this._buffers = null;this._bufferToUpdate = 0;this.interactiveChildren = false;this.blendMode = CONST.BLEND_MODES.NORMAL;this.roundPixels = true;this.setProperties(properties);}ParticleContainer.prototype = Object.create(Container.prototype);ParticleContainer.prototype.constructor = ParticleContainer;module.exports = ParticleContainer;ParticleContainer.prototype.setProperties = function(properties){if(properties){this._properties[0] = "scale" in properties?!!properties.scale:this._properties[0];this._properties[1] = "position" in properties?!!properties.position:this._properties[1];this._properties[2] = "rotation" in properties?!!properties.rotation:this._properties[2];this._properties[3] = "uvs" in properties?!!properties.uvs:this._properties[3];this._properties[4] = "alpha" in properties?!!properties.alpha:this._properties[4];}};ParticleContainer.prototype.updateTransform = function(){this.displayObjectUpdateTransform();};ParticleContainer.prototype.renderWebGL = function(renderer){if(!this.visible || this.worldAlpha <= 0 || !this.children.length || !this.renderable){return;}renderer.setObjectRenderer(renderer.plugins.particle);renderer.plugins.particle.render(this);};ParticleContainer.prototype.onChildrenChange = function(smallestChildIndex){var bufferIndex=Math.floor(smallestChildIndex / this._batchSize);if(bufferIndex < this._bufferToUpdate){this._bufferToUpdate = bufferIndex;}};ParticleContainer.prototype.renderCanvas = function(renderer){if(!this.visible || this.worldAlpha <= 0 || !this.children.length || !this.renderable){return;}var context=renderer.context;var transform=this.worldTransform;var isRotated=true;var positionX=0;var positionY=0;var finalWidth=0;var finalHeight=0;var compositeOperation=renderer.blendModes[this.blendMode];if(compositeOperation !== context.globalCompositeOperation){context.globalCompositeOperation = compositeOperation;}context.globalAlpha = this.worldAlpha;this.displayObjectUpdateTransform();for(var i=0; i < this.children.length; ++i) {var child=this.children[i];if(!child.visible){continue;}var frame=child.texture.frame;context.globalAlpha = this.worldAlpha * child.alpha;if(child.rotation % (Math.PI * 2) === 0){if(isRotated){context.setTransform(transform.a, transform.b, transform.c, transform.d, transform.tx, transform.ty);isRotated = false;}positionX = child.anchor.x * (-frame.width * child.scale.x) + child.position.x + 0.5;positionY = child.anchor.y * (-frame.height * child.scale.y) + child.position.y + 0.5;finalWidth = frame.width * child.scale.x;finalHeight = frame.height * child.scale.y;}else {if(!isRotated){isRotated = true;}child.displayObjectUpdateTransform();var childTransform=child.worldTransform;if(renderer.roundPixels){context.setTransform(childTransform.a, childTransform.b, childTransform.c, childTransform.d, childTransform.tx | 0, childTransform.ty | 0);}else {context.setTransform(childTransform.a, childTransform.b, childTransform.c, childTransform.d, childTransform.tx, childTransform.ty);}positionX = child.anchor.x * -frame.width + 0.5;positionY = child.anchor.y * -frame.height + 0.5;finalWidth = frame.width;finalHeight = frame.height;}context.drawImage(child.texture.baseTexture.source, frame.x, frame.y, frame.width, frame.height, positionX, positionY, finalWidth, finalHeight);}};ParticleContainer.prototype.destroy = function(){Container.prototype.destroy.apply(this, arguments);if(this._buffers){for(var i=0; i < this._buffers.length; ++i) {this._buffers[i].destroy();}}this._properties = null;this._buffers = null;};}, {"../const":22, "../display/Container":23}], 40:[function(require, module, exports){function ParticleBuffer(gl, properties, dynamicPropertyFlags, size){this.gl = gl;this.vertSize = 2;this.vertByteSize = this.vertSize * 4;this.size = size;this.dynamicProperties = [];this.staticProperties = [];for(var i=0; i < properties.length; i++) {var property=properties[i];if(dynamicPropertyFlags[i]){this.dynamicProperties.push(property);}else {this.staticProperties.push(property);}}this.staticStride = 0;this.staticBuffer = null;this.staticData = null;this.dynamicStride = 0;this.dynamicBuffer = null;this.dynamicData = null;this.initBuffers();}ParticleBuffer.prototype.constructor = ParticleBuffer;module.exports = ParticleBuffer;ParticleBuffer.prototype.initBuffers = function(){var gl=this.gl;var i;var property;var dynamicOffset=0;this.dynamicStride = 0;for(i = 0; i < this.dynamicProperties.length; i++) {property = this.dynamicProperties[i];property.offset = dynamicOffset;dynamicOffset += property.size;this.dynamicStride += property.size;}this.dynamicData = new Float32Array(this.size * this.dynamicStride * 4);this.dynamicBuffer = gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER, this.dynamicBuffer);gl.bufferData(gl.ARRAY_BUFFER, this.dynamicData, gl.DYNAMIC_DRAW);var staticOffset=0;this.staticStride = 0;for(i = 0; i < this.staticProperties.length; i++) {property = this.staticProperties[i];property.offset = staticOffset;staticOffset += property.size;this.staticStride += property.size;}this.staticData = new Float32Array(this.size * this.staticStride * 4);this.staticBuffer = gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER, this.staticBuffer);gl.bufferData(gl.ARRAY_BUFFER, this.staticData, gl.DYNAMIC_DRAW);};ParticleBuffer.prototype.uploadDynamic = function(children, startIndex, amount){var gl=this.gl;for(var i=0; i < this.dynamicProperties.length; i++) {var property=this.dynamicProperties[i];property.uploadFunction(children, startIndex, amount, this.dynamicData, this.dynamicStride, property.offset);}gl.bindBuffer(gl.ARRAY_BUFFER, this.dynamicBuffer);gl.bufferSubData(gl.ARRAY_BUFFER, 0, this.dynamicData);};ParticleBuffer.prototype.uploadStatic = function(children, startIndex, amount){var gl=this.gl;for(var i=0; i < this.staticProperties.length; i++) {var property=this.staticProperties[i];property.uploadFunction(children, startIndex, amount, this.staticData, this.staticStride, property.offset);}gl.bindBuffer(gl.ARRAY_BUFFER, this.staticBuffer);gl.bufferSubData(gl.ARRAY_BUFFER, 0, this.staticData);};ParticleBuffer.prototype.bind = function(){var gl=this.gl;var i, property;gl.bindBuffer(gl.ARRAY_BUFFER, this.dynamicBuffer);for(i = 0; i < this.dynamicProperties.length; i++) {property = this.dynamicProperties[i];gl.vertexAttribPointer(property.attribute, property.size, gl.FLOAT, false, this.dynamicStride * 4, property.offset * 4);}gl.bindBuffer(gl.ARRAY_BUFFER, this.staticBuffer);for(i = 0; i < this.staticProperties.length; i++) {property = this.staticProperties[i];gl.vertexAttribPointer(property.attribute, property.size, gl.FLOAT, false, this.staticStride * 4, property.offset * 4);}};ParticleBuffer.prototype.destroy = function(){this.dynamicProperties = null;this.dynamicData = null;this.gl.deleteBuffer(this.dynamicBuffer);this.staticProperties = null;this.staticData = null;this.gl.deleteBuffer(this.staticBuffer);};}, {}], 41:[function(require, module, exports){var ObjectRenderer=require("../../renderers/webgl/utils/ObjectRenderer"), WebGLRenderer=require("../../renderers/webgl/WebGLRenderer"), ParticleShader=require("./ParticleShader"), ParticleBuffer=require("./ParticleBuffer"), math=require("../../math");function ParticleRenderer(renderer){ObjectRenderer.call(this, renderer);var numIndices=98304;this.indices = new Uint16Array(numIndices);for(var i=0, j=0; i < numIndices; i += 6, j += 4) {this.indices[i + 0] = j + 0;this.indices[i + 1] = j + 1;this.indices[i + 2] = j + 2;this.indices[i + 3] = j + 0;this.indices[i + 4] = j + 2;this.indices[i + 5] = j + 3;}this.shader = null;this.indexBuffer = null;this.properties = null;this.tempMatrix = new math.Matrix();}ParticleRenderer.prototype = Object.create(ObjectRenderer.prototype);ParticleRenderer.prototype.constructor = ParticleRenderer;module.exports = ParticleRenderer;WebGLRenderer.registerPlugin("particle", ParticleRenderer);ParticleRenderer.prototype.onContextChange = function(){var gl=this.renderer.gl;this.shader = new ParticleShader(this.renderer.shaderManager);this.indexBuffer = gl.createBuffer();gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.indexBuffer);gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, this.indices, gl.STATIC_DRAW);this.properties = [{attribute:this.shader.attributes.aVertexPosition, size:2, uploadFunction:this.uploadVertices, offset:0}, {attribute:this.shader.attributes.aPositionCoord, size:2, uploadFunction:this.uploadPosition, offset:0}, {attribute:this.shader.attributes.aRotation, size:1, uploadFunction:this.uploadRotation, offset:0}, {attribute:this.shader.attributes.aTextureCoord, size:2, uploadFunction:this.uploadUvs, offset:0}, {attribute:this.shader.attributes.aColor, size:1, uploadFunction:this.uploadAlpha, offset:0}];};ParticleRenderer.prototype.start = function(){var gl=this.renderer.gl;gl.activeTexture(gl.TEXTURE0);gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.indexBuffer);var shader=this.shader;this.renderer.shaderManager.setShader(shader);};ParticleRenderer.prototype.render = function(container){var children=container.children, totalChildren=children.length, maxSize=container._maxSize, batchSize=container._batchSize;if(totalChildren === 0){return;}else if(totalChildren > maxSize){totalChildren = maxSize;}if(!container._buffers){container._buffers = this.generateBuffers(container);}this.renderer.blendModeManager.setBlendMode(container.blendMode);var gl=this.renderer.gl;var m=container.worldTransform.copy(this.tempMatrix);m.prepend(this.renderer.currentRenderTarget.projectionMatrix);gl.uniformMatrix3fv(this.shader.uniforms.projectionMatrix._location, false, m.toArray(true));gl.uniform1f(this.shader.uniforms.uAlpha._location, container.worldAlpha);var baseTexture=children[0]._texture.baseTexture;if(!baseTexture._glTextures[gl.id]){if(!this.renderer.updateTexture(baseTexture)){return;}if(!container._properties[0] || !container._properties[3]){container._bufferToUpdate = 0;}}else {gl.bindTexture(gl.TEXTURE_2D, baseTexture._glTextures[gl.id]);}for(var i=0, j=0; i < totalChildren; i += batchSize, j += 1) {var amount=totalChildren - i;if(amount > batchSize){amount = batchSize;}var buffer=container._buffers[j];buffer.uploadDynamic(children, i, amount);if(container._bufferToUpdate === j){buffer.uploadStatic(children, i, amount);container._bufferToUpdate = j + 1;}buffer.bind(this.shader);gl.drawElements(gl.TRIANGLES, amount * 6, gl.UNSIGNED_SHORT, 0);this.renderer.drawCount++;}};ParticleRenderer.prototype.generateBuffers = function(container){var gl=this.renderer.gl, buffers=[], size=container._maxSize, batchSize=container._batchSize, dynamicPropertyFlags=container._properties, i;for(i = 0; i < size; i += batchSize) {buffers.push(new ParticleBuffer(gl, this.properties, dynamicPropertyFlags, batchSize));}return buffers;};ParticleRenderer.prototype.uploadVertices = function(children, startIndex, amount, array, stride, offset){var sprite, texture, trim, sx, sy, w0, w1, h0, h1;for(var i=0; i < amount; i++) {sprite = children[startIndex + i];texture = sprite._texture;sx = sprite.scale.x;sy = sprite.scale.y;if(texture.trim){trim = texture.trim;w1 = trim.x - sprite.anchor.x * trim.width;w0 = w1 + texture.crop.width;h1 = trim.y - sprite.anchor.y * trim.height;h0 = h1 + texture.crop.height;}else {w0 = texture._frame.width * (1 - sprite.anchor.x);w1 = texture._frame.width * -sprite.anchor.x;h0 = texture._frame.height * (1 - sprite.anchor.y);h1 = texture._frame.height * -sprite.anchor.y;}array[offset] = w1 * sx;array[offset + 1] = h1 * sy;array[offset + stride] = w0 * sx;array[offset + stride + 1] = h1 * sy;array[offset + stride * 2] = w0 * sx;array[offset + stride * 2 + 1] = h0 * sy;array[offset + stride * 3] = w1 * sx;array[offset + stride * 3 + 1] = h0 * sy;offset += stride * 4;}};ParticleRenderer.prototype.uploadPosition = function(children, startIndex, amount, array, stride, offset){for(var i=0; i < amount; i++) {var spritePosition=children[startIndex + i].position;array[offset] = spritePosition.x;array[offset + 1] = spritePosition.y;array[offset + stride] = spritePosition.x;array[offset + stride + 1] = spritePosition.y;array[offset + stride * 2] = spritePosition.x;array[offset + stride * 2 + 1] = spritePosition.y;array[offset + stride * 3] = spritePosition.x;array[offset + stride * 3 + 1] = spritePosition.y;offset += stride * 4;}};ParticleRenderer.prototype.uploadRotation = function(children, startIndex, amount, array, stride, offset){for(var i=0; i < amount; i++) {var spriteRotation=children[startIndex + i].rotation;array[offset] = spriteRotation;array[offset + stride] = spriteRotation;array[offset + stride * 2] = spriteRotation;array[offset + stride * 3] = spriteRotation;offset += stride * 4;}};ParticleRenderer.prototype.uploadUvs = function(children, startIndex, amount, array, stride, offset){for(var i=0; i < amount; i++) {var textureUvs=children[startIndex + i]._texture._uvs;if(textureUvs){array[offset] = textureUvs.x0;array[offset + 1] = textureUvs.y0;array[offset + stride] = textureUvs.x1;array[offset + stride + 1] = textureUvs.y1;array[offset + stride * 2] = textureUvs.x2;array[offset + stride * 2 + 1] = textureUvs.y2;array[offset + stride * 3] = textureUvs.x3;array[offset + stride * 3 + 1] = textureUvs.y3;offset += stride * 4;}else {array[offset] = 0;array[offset + 1] = 0;array[offset + stride] = 0;array[offset + stride + 1] = 0;array[offset + stride * 2] = 0;array[offset + stride * 2 + 1] = 0;array[offset + stride * 3] = 0;array[offset + stride * 3 + 1] = 0;offset += stride * 4;}}};ParticleRenderer.prototype.uploadAlpha = function(children, startIndex, amount, array, stride, offset){for(var i=0; i < amount; i++) {var spriteAlpha=children[startIndex + i].alpha;array[offset] = spriteAlpha;array[offset + stride] = spriteAlpha;array[offset + stride * 2] = spriteAlpha;array[offset + stride * 3] = spriteAlpha;offset += stride * 4;}};ParticleRenderer.prototype.destroy = function(){if(this.renderer.gl){this.renderer.gl.deleteBuffer(this.indexBuffer);}ObjectRenderer.prototype.destroy.apply(this, arguments);this.shader.destroy();this.indices = null;this.tempMatrix = null;};}, {"../../math":33, "../../renderers/webgl/WebGLRenderer":49, "../../renderers/webgl/utils/ObjectRenderer":63, "./ParticleBuffer":40, "./ParticleShader":42}], 42:[function(require, module, exports){var TextureShader=require("../../renderers/webgl/shaders/TextureShader");function ParticleShader(shaderManager){TextureShader.call(this, shaderManager, ["attribute vec2 aVertexPosition;", "attribute vec2 aTextureCoord;", "attribute float aColor;", "attribute vec2 aPositionCoord;", "attribute vec2 aScale;", "attribute float aRotation;", "uniform mat3 projectionMatrix;", "varying vec2 vTextureCoord;", "varying float vColor;", "void main(void){", "   vec2 v = aVertexPosition;", "   v.x = (aVertexPosition.x) * cos(aRotation) - (aVertexPosition.y) * sin(aRotation);", "   v.y = (aVertexPosition.x) * sin(aRotation) + (aVertexPosition.y) * cos(aRotation);", "   v = v + aPositionCoord;", "   gl_Position = vec4((projectionMatrix * vec3(v, 1.0)).xy, 0.0, 1.0);", "   vTextureCoord = aTextureCoord;", "   vColor = aColor;", "}"].join("\n"), ["precision lowp float;", "varying vec2 vTextureCoord;", "varying float vColor;", "uniform sampler2D uSampler;", "uniform float uAlpha;", "void main(void){", "  vec4 color = texture2D(uSampler, vTextureCoord) * vColor * uAlpha;", "  if (color.a == 0.0) discard;", "  gl_FragColor = color;", "}"].join("\n"), {uAlpha:{type:"1f", value:1}}, {aPositionCoord:0, aRotation:0});}ParticleShader.prototype = Object.create(TextureShader.prototype);ParticleShader.prototype.constructor = ParticleShader;module.exports = ParticleShader;}, {"../../renderers/webgl/shaders/TextureShader":62}], 43:[function(require, module, exports){var utils=require("../utils"), math=require("../math"), CONST=require("../const"), EventEmitter=require("eventemitter3");function SystemRenderer(system, width, height, options){EventEmitter.call(this);utils.sayHello(system);if(options){for(var i in CONST.DEFAULT_RENDER_OPTIONS) {if(typeof options[i] === "undefined"){options[i] = CONST.DEFAULT_RENDER_OPTIONS[i];}}}else {options = CONST.DEFAULT_RENDER_OPTIONS;}this.type = CONST.RENDERER_TYPE.UNKNOWN;this.width = width || 800;this.height = height || 600;this.view = options.view || document.createElement("canvas");this.resolution = options.resolution;this.transparent = options.transparent;this.autoResize = options.autoResize || false;this.blendModes = null;this.preserveDrawingBuffer = options.preserveDrawingBuffer;this.clearBeforeRender = options.clearBeforeRender;this.roundPixels = options.roundPixels;this._backgroundColor = 0;this._backgroundColorRgb = [0, 0, 0];this._backgroundColorString = "#000000";this.backgroundColor = options.backgroundColor || this._backgroundColor;this._tempDisplayObjectParent = {worldTransform:new math.Matrix(), worldAlpha:1, children:[]};this._lastObjectRendered = this._tempDisplayObjectParent;}SystemRenderer.prototype = Object.create(EventEmitter.prototype);SystemRenderer.prototype.constructor = SystemRenderer;module.exports = SystemRenderer;Object.defineProperties(SystemRenderer.prototype, {backgroundColor:{get:function get(){return this._backgroundColor;}, set:function set(val){this._backgroundColor = val;this._backgroundColorString = utils.hex2string(val);utils.hex2rgb(val, this._backgroundColorRgb);}}});SystemRenderer.prototype.resize = function(width, height){this.width = width * this.resolution;this.height = height * this.resolution;this.view.width = this.width;this.view.height = this.height;if(this.autoResize){this.view.style.width = this.width / this.resolution + "px";this.view.style.height = this.height / this.resolution + "px";}};SystemRenderer.prototype.destroy = function(removeView){if(removeView && this.view.parentNode){this.view.parentNode.removeChild(this.view);}this.type = CONST.RENDERER_TYPE.UNKNOWN;this.width = 0;this.height = 0;this.view = null;this.resolution = 0;this.transparent = false;this.autoResize = false;this.blendModes = null;this.preserveDrawingBuffer = false;this.clearBeforeRender = false;this.roundPixels = false;this._backgroundColor = 0;this._backgroundColorRgb = null;this._backgroundColorString = null;};}, {"../const":22, "../math":33, "../utils":77, eventemitter3:10}], 44:[function(require, module, exports){var SystemRenderer=require("../SystemRenderer"), CanvasMaskManager=require("./utils/CanvasMaskManager"), utils=require("../../utils"), math=require("../../math"), CONST=require("../../const");function CanvasRenderer(width, height, options){options = options || {};SystemRenderer.call(this, "Canvas", width, height, options);this.type = CONST.RENDERER_TYPE.CANVAS;this.context = this.view.getContext("2d", {alpha:this.transparent});this.refresh = true;this.maskManager = new CanvasMaskManager();this.smoothProperty = "imageSmoothingEnabled";if(!this.context.imageSmoothingEnabled){if(this.context.webkitImageSmoothingEnabled){this.smoothProperty = "webkitImageSmoothingEnabled";}else if(this.context.mozImageSmoothingEnabled){this.smoothProperty = "mozImageSmoothingEnabled";}else if(this.context.oImageSmoothingEnabled){this.smoothProperty = "oImageSmoothingEnabled";}else if(this.context.msImageSmoothingEnabled){this.smoothProperty = "msImageSmoothingEnabled";}}this.initPlugins();this._mapBlendModes();this._tempDisplayObjectParent = {worldTransform:new math.Matrix(), worldAlpha:1};this.resize(width, height);}CanvasRenderer.prototype = Object.create(SystemRenderer.prototype);CanvasRenderer.prototype.constructor = CanvasRenderer;module.exports = CanvasRenderer;utils.pluginTarget.mixin(CanvasRenderer);CanvasRenderer.prototype.render = function(object){this.emit("prerender");var cacheParent=object.parent;this._lastObjectRendered = object;object.parent = this._tempDisplayObjectParent;object.updateTransform();object.parent = cacheParent;this.context.setTransform(1, 0, 0, 1, 0, 0);this.context.globalAlpha = 1;this.context.globalCompositeOperation = this.blendModes[CONST.BLEND_MODES.NORMAL];if(navigator.isCocoonJS && this.view.screencanvas){this.context.fillStyle = "black";this.context.clear();}if(this.clearBeforeRender){if(this.transparent){this.context.clearRect(0, 0, this.width, this.height);}else {this.context.fillStyle = this._backgroundColorString;this.context.fillRect(0, 0, this.width, this.height);}}this.renderDisplayObject(object, this.context);this.emit("postrender");};CanvasRenderer.prototype.destroy = function(removeView){this.destroyPlugins();SystemRenderer.prototype.destroy.call(this, removeView);this.context = null;this.refresh = true;this.maskManager.destroy();this.maskManager = null;this.smoothProperty = null;};CanvasRenderer.prototype.renderDisplayObject = function(displayObject, context){var tempContext=this.context;this.context = context;displayObject.renderCanvas(this);this.context = tempContext;};CanvasRenderer.prototype.resize = function(w, h){SystemRenderer.prototype.resize.call(this, w, h);if(this.smoothProperty){this.context[this.smoothProperty] = CONST.SCALE_MODES.DEFAULT === CONST.SCALE_MODES.LINEAR;}};CanvasRenderer.prototype._mapBlendModes = function(){if(!this.blendModes){this.blendModes = {};if(utils.canUseNewCanvasBlendModes()){this.blendModes[CONST.BLEND_MODES.NORMAL] = "source-over";this.blendModes[CONST.BLEND_MODES.ADD] = "lighter";this.blendModes[CONST.BLEND_MODES.MULTIPLY] = "multiply";this.blendModes[CONST.BLEND_MODES.SCREEN] = "screen";this.blendModes[CONST.BLEND_MODES.OVERLAY] = "overlay";this.blendModes[CONST.BLEND_MODES.DARKEN] = "darken";this.blendModes[CONST.BLEND_MODES.LIGHTEN] = "lighten";this.blendModes[CONST.BLEND_MODES.COLOR_DODGE] = "color-dodge";this.blendModes[CONST.BLEND_MODES.COLOR_BURN] = "color-burn";this.blendModes[CONST.BLEND_MODES.HARD_LIGHT] = "hard-light";this.blendModes[CONST.BLEND_MODES.SOFT_LIGHT] = "soft-light";this.blendModes[CONST.BLEND_MODES.DIFFERENCE] = "difference";this.blendModes[CONST.BLEND_MODES.EXCLUSION] = "exclusion";this.blendModes[CONST.BLEND_MODES.HUE] = "hue";this.blendModes[CONST.BLEND_MODES.SATURATION] = "saturate";this.blendModes[CONST.BLEND_MODES.COLOR] = "color";this.blendModes[CONST.BLEND_MODES.LUMINOSITY] = "luminosity";}else {this.blendModes[CONST.BLEND_MODES.NORMAL] = "source-over";this.blendModes[CONST.BLEND_MODES.ADD] = "lighter";this.blendModes[CONST.BLEND_MODES.MULTIPLY] = "source-over";this.blendModes[CONST.BLEND_MODES.SCREEN] = "source-over";this.blendModes[CONST.BLEND_MODES.OVERLAY] = "source-over";this.blendModes[CONST.BLEND_MODES.DARKEN] = "source-over";this.blendModes[CONST.BLEND_MODES.LIGHTEN] = "source-over";this.blendModes[CONST.BLEND_MODES.COLOR_DODGE] = "source-over";this.blendModes[CONST.BLEND_MODES.COLOR_BURN] = "source-over";this.blendModes[CONST.BLEND_MODES.HARD_LIGHT] = "source-over";this.blendModes[CONST.BLEND_MODES.SOFT_LIGHT] = "source-over";this.blendModes[CONST.BLEND_MODES.DIFFERENCE] = "source-over";this.blendModes[CONST.BLEND_MODES.EXCLUSION] = "source-over";this.blendModes[CONST.BLEND_MODES.HUE] = "source-over";this.blendModes[CONST.BLEND_MODES.SATURATION] = "source-over";this.blendModes[CONST.BLEND_MODES.COLOR] = "source-over";this.blendModes[CONST.BLEND_MODES.LUMINOSITY] = "source-over";}}};}, {"../../const":22, "../../math":33, "../../utils":77, "../SystemRenderer":43, "./utils/CanvasMaskManager":47}], 45:[function(require, module, exports){function CanvasBuffer(width, height){this.canvas = document.createElement("canvas");this.context = this.canvas.getContext("2d");this.canvas.width = width;this.canvas.height = height;}CanvasBuffer.prototype.constructor = CanvasBuffer;module.exports = CanvasBuffer;Object.defineProperties(CanvasBuffer.prototype, {width:{get:function get(){return this.canvas.width;}, set:function set(val){this.canvas.width = val;}}, height:{get:function get(){return this.canvas.height;}, set:function set(val){this.canvas.height = val;}}});CanvasBuffer.prototype.clear = function(){this.context.setTransform(1, 0, 0, 1, 0, 0);this.context.clearRect(0, 0, this.canvas.width, this.canvas.height);};CanvasBuffer.prototype.resize = function(width, height){this.canvas.width = width;this.canvas.height = height;};CanvasBuffer.prototype.destroy = function(){this.context = null;this.canvas = null;};}, {}], 46:[function(require, module, exports){var CONST=require("../../../const");var CanvasGraphics={};module.exports = CanvasGraphics;CanvasGraphics.renderGraphics = function(graphics, context){var worldAlpha=graphics.worldAlpha;if(graphics.dirty){this.updateGraphicsTint(graphics);graphics.dirty = false;}for(var i=0; i < graphics.graphicsData.length; i++) {var data=graphics.graphicsData[i];var shape=data.shape;var fillColor=data._fillTint;var lineColor=data._lineTint;context.lineWidth = data.lineWidth;if(data.type === CONST.SHAPES.POLY){context.beginPath();var points=shape.points;context.moveTo(points[0], points[1]);for(var j=1; j < points.length / 2; j++) {context.lineTo(points[j * 2], points[j * 2 + 1]);}if(shape.closed){context.lineTo(points[0], points[1]);}if(points[0] === points[points.length - 2] && points[1] === points[points.length - 1]){context.closePath();}if(data.fill){context.globalAlpha = data.fillAlpha * worldAlpha;context.fillStyle = "#" + ("00000" + (fillColor | 0).toString(16)).substr(-6);context.fill();}if(data.lineWidth){context.globalAlpha = data.lineAlpha * worldAlpha;context.strokeStyle = "#" + ("00000" + (lineColor | 0).toString(16)).substr(-6);context.stroke();}}else if(data.type === CONST.SHAPES.RECT){if(data.fillColor || data.fillColor === 0){context.globalAlpha = data.fillAlpha * worldAlpha;context.fillStyle = "#" + ("00000" + (fillColor | 0).toString(16)).substr(-6);context.fillRect(shape.x, shape.y, shape.width, shape.height);}if(data.lineWidth){context.globalAlpha = data.lineAlpha * worldAlpha;context.strokeStyle = "#" + ("00000" + (lineColor | 0).toString(16)).substr(-6);context.strokeRect(shape.x, shape.y, shape.width, shape.height);}}else if(data.type === CONST.SHAPES.CIRC){context.beginPath();context.arc(shape.x, shape.y, shape.radius, 0, 2 * Math.PI);context.closePath();if(data.fill){context.globalAlpha = data.fillAlpha * worldAlpha;context.fillStyle = "#" + ("00000" + (fillColor | 0).toString(16)).substr(-6);context.fill();}if(data.lineWidth){context.globalAlpha = data.lineAlpha * worldAlpha;context.strokeStyle = "#" + ("00000" + (lineColor | 0).toString(16)).substr(-6);context.stroke();}}else if(data.type === CONST.SHAPES.ELIP){var w=shape.width * 2;var h=shape.height * 2;var x=shape.x - w / 2;var y=shape.y - h / 2;context.beginPath();var kappa=0.5522848, ox=w / 2 * kappa, oy=h / 2 * kappa, xe=x + w, ye=y + h, xm=x + w / 2, ym=y + h / 2;context.moveTo(x, ym);context.bezierCurveTo(x, ym - oy, xm - ox, y, xm, y);context.bezierCurveTo(xm + ox, y, xe, ym - oy, xe, ym);context.bezierCurveTo(xe, ym + oy, xm + ox, ye, xm, ye);context.bezierCurveTo(xm - ox, ye, x, ym + oy, x, ym);context.closePath();if(data.fill){context.globalAlpha = data.fillAlpha * worldAlpha;context.fillStyle = "#" + ("00000" + (fillColor | 0).toString(16)).substr(-6);context.fill();}if(data.lineWidth){context.globalAlpha = data.lineAlpha * worldAlpha;context.strokeStyle = "#" + ("00000" + (lineColor | 0).toString(16)).substr(-6);context.stroke();}}else if(data.type === CONST.SHAPES.RREC){var rx=shape.x;var ry=shape.y;var width=shape.width;var height=shape.height;var radius=shape.radius;var maxRadius=Math.min(width, height) / 2 | 0;radius = radius > maxRadius?maxRadius:radius;context.beginPath();context.moveTo(rx, ry + radius);context.lineTo(rx, ry + height - radius);context.quadraticCurveTo(rx, ry + height, rx + radius, ry + height);context.lineTo(rx + width - radius, ry + height);context.quadraticCurveTo(rx + width, ry + height, rx + width, ry + height - radius);context.lineTo(rx + width, ry + radius);context.quadraticCurveTo(rx + width, ry, rx + width - radius, ry);context.lineTo(rx + radius, ry);context.quadraticCurveTo(rx, ry, rx, ry + radius);context.closePath();if(data.fillColor || data.fillColor === 0){context.globalAlpha = data.fillAlpha * worldAlpha;context.fillStyle = "#" + ("00000" + (fillColor | 0).toString(16)).substr(-6);context.fill();}if(data.lineWidth){context.globalAlpha = data.lineAlpha * worldAlpha;context.strokeStyle = "#" + ("00000" + (lineColor | 0).toString(16)).substr(-6);context.stroke();}}}};CanvasGraphics.renderGraphicsMask = function(graphics, context){var len=graphics.graphicsData.length;if(len === 0){return;}context.beginPath();for(var i=0; i < len; i++) {var data=graphics.graphicsData[i];var shape=data.shape;if(data.type === CONST.SHAPES.POLY){var points=shape.points;context.moveTo(points[0], points[1]);for(var j=1; j < points.length / 2; j++) {context.lineTo(points[j * 2], points[j * 2 + 1]);}if(points[0] === points[points.length - 2] && points[1] === points[points.length - 1]){context.closePath();}}else if(data.type === CONST.SHAPES.RECT){context.rect(shape.x, shape.y, shape.width, shape.height);context.closePath();}else if(data.type === CONST.SHAPES.CIRC){context.arc(shape.x, shape.y, shape.radius, 0, 2 * Math.PI);context.closePath();}else if(data.type === CONST.SHAPES.ELIP){var w=shape.width * 2;var h=shape.height * 2;var x=shape.x - w / 2;var y=shape.y - h / 2;var kappa=0.5522848, ox=w / 2 * kappa, oy=h / 2 * kappa, xe=x + w, ye=y + h, xm=x + w / 2, ym=y + h / 2;context.moveTo(x, ym);context.bezierCurveTo(x, ym - oy, xm - ox, y, xm, y);context.bezierCurveTo(xm + ox, y, xe, ym - oy, xe, ym);context.bezierCurveTo(xe, ym + oy, xm + ox, ye, xm, ye);context.bezierCurveTo(xm - ox, ye, x, ym + oy, x, ym);context.closePath();}else if(data.type === CONST.SHAPES.RREC){var rx=shape.x;var ry=shape.y;var width=shape.width;var height=shape.height;var radius=shape.radius;var maxRadius=Math.min(width, height) / 2 | 0;radius = radius > maxRadius?maxRadius:radius;context.moveTo(rx, ry + radius);context.lineTo(rx, ry + height - radius);context.quadraticCurveTo(rx, ry + height, rx + radius, ry + height);context.lineTo(rx + width - radius, ry + height);context.quadraticCurveTo(rx + width, ry + height, rx + width, ry + height - radius);context.lineTo(rx + width, ry + radius);context.quadraticCurveTo(rx + width, ry, rx + width - radius, ry);context.lineTo(rx + radius, ry);context.quadraticCurveTo(rx, ry, rx, ry + radius);context.closePath();}}};CanvasGraphics.updateGraphicsTint = function(graphics){if(graphics.tint === 16777215 && graphics._prevTint === graphics.tint){return;}graphics._prevTint = graphics.tint;var tintR=(graphics.tint >> 16 & 255) / 255;var tintG=(graphics.tint >> 8 & 255) / 255;var tintB=(graphics.tint & 255) / 255;for(var i=0; i < graphics.graphicsData.length; i++) {var data=graphics.graphicsData[i];var fillColor=data.fillColor | 0;var lineColor=data.lineColor | 0;data._fillTint = ((fillColor >> 16 & 255) / 255 * tintR * 255 << 16) + ((fillColor >> 8 & 255) / 255 * tintG * 255 << 8) + (fillColor & 255) / 255 * tintB * 255;data._lineTint = ((lineColor >> 16 & 255) / 255 * tintR * 255 << 16) + ((lineColor >> 8 & 255) / 255 * tintG * 255 << 8) + (lineColor & 255) / 255 * tintB * 255;}};}, {"../../../const":22}], 47:[function(require, module, exports){var CanvasGraphics=require("./CanvasGraphics");function CanvasMaskManager(){}CanvasMaskManager.prototype.constructor = CanvasMaskManager;module.exports = CanvasMaskManager;CanvasMaskManager.prototype.pushMask = function(maskData, renderer){renderer.context.save();var cacheAlpha=maskData.alpha;var transform=maskData.worldTransform;var resolution=renderer.resolution;renderer.context.setTransform(transform.a * resolution, transform.b * resolution, transform.c * resolution, transform.d * resolution, transform.tx * resolution, transform.ty * resolution);if(!maskData.texture){CanvasGraphics.renderGraphicsMask(maskData, renderer.context);renderer.context.clip();}maskData.worldAlpha = cacheAlpha;};CanvasMaskManager.prototype.popMask = function(renderer){renderer.context.restore();};CanvasMaskManager.prototype.destroy = function(){};}, {"./CanvasGraphics":46}], 48:[function(require, module, exports){var utils=require("../../../utils");var CanvasTinter={};module.exports = CanvasTinter;CanvasTinter.getTintedTexture = function(sprite, color){var texture=sprite.texture;color = CanvasTinter.roundColor(color);var stringColor="#" + ("00000" + (color | 0).toString(16)).substr(-6);texture.tintCache = texture.tintCache || {};if(texture.tintCache[stringColor]){return texture.tintCache[stringColor];}var canvas=CanvasTinter.canvas || document.createElement("canvas");CanvasTinter.tintMethod(texture, color, canvas);if(CanvasTinter.convertTintToImage){var tintImage=new Image();tintImage.src = canvas.toDataURL();texture.tintCache[stringColor] = tintImage;}else {texture.tintCache[stringColor] = canvas;CanvasTinter.canvas = null;}return canvas;};CanvasTinter.tintWithMultiply = function(texture, color, canvas){var context=canvas.getContext("2d");var resolution=texture.baseTexture.resolution;var crop=texture.crop.clone();crop.x *= resolution;crop.y *= resolution;crop.width *= resolution;crop.height *= resolution;canvas.width = crop.width;canvas.height = crop.height;context.fillStyle = "#" + ("00000" + (color | 0).toString(16)).substr(-6);context.fillRect(0, 0, crop.width, crop.height);context.globalCompositeOperation = "multiply";context.drawImage(texture.baseTexture.source, crop.x, crop.y, crop.width, crop.height, 0, 0, crop.width, crop.height);context.globalCompositeOperation = "destination-atop";context.drawImage(texture.baseTexture.source, crop.x, crop.y, crop.width, crop.height, 0, 0, crop.width, crop.height);};CanvasTinter.tintWithOverlay = function(texture, color, canvas){var context=canvas.getContext("2d");var resolution=texture.baseTexture.resolution;var crop=texture.crop.clone();crop.x *= resolution;crop.y *= resolution;crop.width *= resolution;crop.height *= resolution;canvas.width = crop.width;canvas.height = crop.height;context.globalCompositeOperation = "copy";context.fillStyle = "#" + ("00000" + (color | 0).toString(16)).substr(-6);context.fillRect(0, 0, crop.width, crop.height);context.globalCompositeOperation = "destination-atop";context.drawImage(texture.baseTexture.source, crop.x, crop.y, crop.width, crop.height, 0, 0, crop.width, crop.height);};CanvasTinter.tintWithPerPixel = function(texture, color, canvas){var context=canvas.getContext("2d");var resolution=texture.baseTexture.resolution;var crop=texture.crop.clone();crop.x *= resolution;crop.y *= resolution;crop.width *= resolution;crop.height *= resolution;canvas.width = crop.width;canvas.height = crop.height;context.globalCompositeOperation = "copy";context.drawImage(texture.baseTexture.source, crop.x, crop.y, crop.width, crop.height, 0, 0, crop.width, crop.height);var rgbValues=utils.hex2rgb(color);var r=rgbValues[0], g=rgbValues[1], b=rgbValues[2];var pixelData=context.getImageData(0, 0, crop.width, crop.height);var pixels=pixelData.data;for(var i=0; i < pixels.length; i += 4) {pixels[i + 0] *= r;pixels[i + 1] *= g;pixels[i + 2] *= b;}context.putImageData(pixelData, 0, 0);};CanvasTinter.roundColor = function(color){var step=CanvasTinter.cacheStepsPerColorChannel;var rgbValues=utils.hex2rgb(color);rgbValues[0] = Math.min(255, rgbValues[0] / step * step);rgbValues[1] = Math.min(255, rgbValues[1] / step * step);rgbValues[2] = Math.min(255, rgbValues[2] / step * step);return utils.rgb2hex(rgbValues);};CanvasTinter.cacheStepsPerColorChannel = 8;CanvasTinter.convertTintToImage = false;CanvasTinter.canUseMultiply = utils.canUseNewCanvasBlendModes();CanvasTinter.tintMethod = CanvasTinter.canUseMultiply?CanvasTinter.tintWithMultiply:CanvasTinter.tintWithPerPixel;}, {"../../../utils":77}], 49:[function(require, module, exports){var SystemRenderer=require("../SystemRenderer"), ShaderManager=require("./managers/ShaderManager"), MaskManager=require("./managers/MaskManager"), StencilManager=require("./managers/StencilManager"), FilterManager=require("./managers/FilterManager"), BlendModeManager=require("./managers/BlendModeManager"), RenderTarget=require("./utils/RenderTarget"), ObjectRenderer=require("./utils/ObjectRenderer"), FXAAFilter=require("./filters/FXAAFilter"), utils=require("../../utils"), CONST=require("../../const");function WebGLRenderer(width, height, options){options = options || {};SystemRenderer.call(this, "WebGL", width, height, options);this.type = CONST.RENDERER_TYPE.WEBGL;this.handleContextLost = this.handleContextLost.bind(this);this.handleContextRestored = this.handleContextRestored.bind(this);this.view.addEventListener("webglcontextlost", this.handleContextLost, false);this.view.addEventListener("webglcontextrestored", this.handleContextRestored, false);this._useFXAA = !!options.forceFXAA && options.antialias;this._FXAAFilter = null;this._contextOptions = {alpha:this.transparent, antialias:options.antialias, premultipliedAlpha:this.transparent && this.transparent !== "notMultiplied", stencil:true, preserveDrawingBuffer:options.preserveDrawingBuffer};this.drawCount = 0;this.shaderManager = new ShaderManager(this);this.maskManager = new MaskManager(this);this.stencilManager = new StencilManager(this);this.filterManager = new FilterManager(this);this.blendModeManager = new BlendModeManager(this);this.currentRenderTarget = null;this.currentRenderer = new ObjectRenderer(this);this.initPlugins();this._createContext();this._initContext();this._mapGlModes();this._managedTextures = [];this._renderTargetStack = [];}WebGLRenderer.prototype = Object.create(SystemRenderer.prototype);WebGLRenderer.prototype.constructor = WebGLRenderer;module.exports = WebGLRenderer;utils.pluginTarget.mixin(WebGLRenderer);WebGLRenderer.glContextId = 0;WebGLRenderer.prototype._createContext = function(){var gl=this.view.getContext("webgl", this._contextOptions) || this.view.getContext("experimental-webgl", this._contextOptions);this.gl = gl;if(!gl){throw new Error("This browser does not support webGL. Try using the canvas renderer");}this.glContextId = WebGLRenderer.glContextId++;gl.id = this.glContextId;gl.renderer = this;};WebGLRenderer.prototype._initContext = function(){var gl=this.gl;gl.disable(gl.DEPTH_TEST);gl.disable(gl.CULL_FACE);gl.enable(gl.BLEND);this.renderTarget = new RenderTarget(gl, this.width, this.height, null, this.resolution, true);this.setRenderTarget(this.renderTarget);this.emit("context", gl);this.resize(this.width, this.height);if(!this._useFXAA){this._useFXAA = this._contextOptions.antialias && !gl.getContextAttributes().antialias;}if(this._useFXAA){window.console.warn("FXAA antialiasing being used instead of native antialiasing");this._FXAAFilter = [new FXAAFilter()];}};WebGLRenderer.prototype.render = function(object){this.emit("prerender");if(this.gl.isContextLost()){return;}this.drawCount = 0;this._lastObjectRendered = object;if(this._useFXAA){this._FXAAFilter[0].uniforms.resolution.value.x = this.width;this._FXAAFilter[0].uniforms.resolution.value.y = this.height;object.filterArea = this.renderTarget.size;object.filters = this._FXAAFilter;}var cacheParent=object.parent;object.parent = this._tempDisplayObjectParent;object.updateTransform();object.parent = cacheParent;var gl=this.gl;this.setRenderTarget(this.renderTarget);if(this.clearBeforeRender){if(this.transparent){gl.clearColor(0, 0, 0, 0);}else {gl.clearColor(this._backgroundColorRgb[0], this._backgroundColorRgb[1], this._backgroundColorRgb[2], 1);}gl.clear(gl.COLOR_BUFFER_BIT);}this.renderDisplayObject(object, this.renderTarget);this.emit("postrender");};WebGLRenderer.prototype.renderDisplayObject = function(displayObject, renderTarget, clear){this.setRenderTarget(renderTarget);if(clear){renderTarget.clear();}this.filterManager.setFilterStack(renderTarget.filterStack);displayObject.renderWebGL(this);this.currentRenderer.flush();};WebGLRenderer.prototype.setObjectRenderer = function(objectRenderer){if(this.currentRenderer === objectRenderer){return;}this.currentRenderer.stop();this.currentRenderer = objectRenderer;this.currentRenderer.start();};WebGLRenderer.prototype.setRenderTarget = function(renderTarget){if(this.currentRenderTarget === renderTarget){return;}this.currentRenderTarget = renderTarget;this.currentRenderTarget.activate();this.stencilManager.setMaskStack(renderTarget.stencilMaskStack);};WebGLRenderer.prototype.resize = function(width, height){SystemRenderer.prototype.resize.call(this, width, height);this.filterManager.resize(width, height);this.renderTarget.resize(width, height);if(this.currentRenderTarget === this.renderTarget){this.renderTarget.activate();this.gl.viewport(0, 0, this.width, this.height);}};WebGLRenderer.prototype.updateTexture = function(texture){texture = texture.baseTexture || texture;if(!texture.hasLoaded){return;}var gl=this.gl;if(!texture._glTextures[gl.id]){texture._glTextures[gl.id] = gl.createTexture();texture.on("update", this.updateTexture, this);texture.on("dispose", this.destroyTexture, this);this._managedTextures.push(texture);}gl.bindTexture(gl.TEXTURE_2D, texture._glTextures[gl.id]);gl.pixelStorei(gl.UNPACK_PREMULTIPLY_ALPHA_WEBGL, texture.premultipliedAlpha);gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, texture.source);gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, texture.scaleMode === CONST.SCALE_MODES.LINEAR?gl.LINEAR:gl.NEAREST);if(texture.mipmap && texture.isPowerOfTwo){gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, texture.scaleMode === CONST.SCALE_MODES.LINEAR?gl.LINEAR_MIPMAP_LINEAR:gl.NEAREST_MIPMAP_NEAREST);gl.generateMipmap(gl.TEXTURE_2D);}else {gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, texture.scaleMode === CONST.SCALE_MODES.LINEAR?gl.LINEAR:gl.NEAREST);}if(!texture.isPowerOfTwo){gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);}else {gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);}return texture._glTextures[gl.id];};WebGLRenderer.prototype.destroyTexture = function(texture, _skipRemove){texture = texture.baseTexture || texture;if(!texture.hasLoaded){return;}if(texture._glTextures[this.gl.id]){this.gl.deleteTexture(texture._glTextures[this.gl.id]);delete texture._glTextures[this.gl.id];if(!_skipRemove){var i=this._managedTextures.indexOf(texture);if(i !== -1){utils.removeItems(this._managedTextures, i, 1);}}}};WebGLRenderer.prototype.handleContextLost = function(event){event.preventDefault();};WebGLRenderer.prototype.handleContextRestored = function(){this._initContext();for(var i=0; i < this._managedTextures.length; ++i) {var texture=this._managedTextures[i];if(texture._glTextures[this.gl.id]){delete texture._glTextures[this.gl.id];}}};WebGLRenderer.prototype.destroy = function(removeView){this.destroyPlugins();this.view.removeEventListener("webglcontextlost", this.handleContextLost);this.view.removeEventListener("webglcontextrestored", this.handleContextRestored);for(var i=0; i < this._managedTextures.length; ++i) {var texture=this._managedTextures[i];this.destroyTexture(texture, true);texture.off("update", this.updateTexture, this);texture.off("dispose", this.destroyTexture, this);}SystemRenderer.prototype.destroy.call(this, removeView);this.uid = 0;this.shaderManager.destroy();this.maskManager.destroy();this.stencilManager.destroy();this.filterManager.destroy();this.blendModeManager.destroy();this.shaderManager = null;this.maskManager = null;this.filterManager = null;this.blendModeManager = null;this.currentRenderer = null;this.handleContextLost = null;this.handleContextRestored = null;this._contextOptions = null;this._managedTextures = null;this.drawCount = 0;this.gl.useProgram(null);this.gl.flush();this.gl = null;};WebGLRenderer.prototype._mapGlModes = function(){var gl=this.gl;if(!this.blendModes){this.blendModes = {};this.blendModes[CONST.BLEND_MODES.NORMAL] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.ADD] = [gl.ONE, gl.DST_ALPHA];this.blendModes[CONST.BLEND_MODES.MULTIPLY] = [gl.DST_COLOR, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.SCREEN] = [gl.ONE, gl.ONE_MINUS_SRC_COLOR];this.blendModes[CONST.BLEND_MODES.OVERLAY] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.DARKEN] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.LIGHTEN] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.COLOR_DODGE] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.COLOR_BURN] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.HARD_LIGHT] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.SOFT_LIGHT] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.DIFFERENCE] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.EXCLUSION] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.HUE] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.SATURATION] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.COLOR] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];this.blendModes[CONST.BLEND_MODES.LUMINOSITY] = [gl.ONE, gl.ONE_MINUS_SRC_ALPHA];}if(!this.drawModes){this.drawModes = {};this.drawModes[CONST.DRAW_MODES.POINTS] = gl.POINTS;this.drawModes[CONST.DRAW_MODES.LINES] = gl.LINES;this.drawModes[CONST.DRAW_MODES.LINE_LOOP] = gl.LINE_LOOP;this.drawModes[CONST.DRAW_MODES.LINE_STRIP] = gl.LINE_STRIP;this.drawModes[CONST.DRAW_MODES.TRIANGLES] = gl.TRIANGLES;this.drawModes[CONST.DRAW_MODES.TRIANGLE_STRIP] = gl.TRIANGLE_STRIP;this.drawModes[CONST.DRAW_MODES.TRIANGLE_FAN] = gl.TRIANGLE_FAN;}};}, {"../../const":22, "../../utils":77, "../SystemRenderer":43, "./filters/FXAAFilter":51, "./managers/BlendModeManager":53, "./managers/FilterManager":54, "./managers/MaskManager":55, "./managers/ShaderManager":56, "./managers/StencilManager":57, "./utils/ObjectRenderer":63, "./utils/RenderTarget":65}], 50:[function(require, module, exports){var DefaultShader=require("../shaders/TextureShader");function AbstractFilter(vertexSrc, fragmentSrc, uniforms){this.shaders = [];this.padding = 0;this.uniforms = uniforms || {};this.vertexSrc = vertexSrc || DefaultShader.defaultVertexSrc;this.fragmentSrc = fragmentSrc || DefaultShader.defaultFragmentSrc;}AbstractFilter.prototype.constructor = AbstractFilter;module.exports = AbstractFilter;AbstractFilter.prototype.getShader = function(renderer){var gl=renderer.gl;var shader=this.shaders[gl.id];if(!shader){shader = new DefaultShader(renderer.shaderManager, this.vertexSrc, this.fragmentSrc, this.uniforms, this.attributes);this.shaders[gl.id] = shader;}return shader;};AbstractFilter.prototype.applyFilter = function(renderer, input, output, clear){var shader=this.getShader(renderer);renderer.filterManager.applyFilter(shader, input, output, clear);};AbstractFilter.prototype.syncUniform = function(uniform){for(var i=0, j=this.shaders.length; i < j; ++i) {this.shaders[i].syncUniform(uniform);}};}, {"../shaders/TextureShader":62}], 51:[function(require, module, exports){var AbstractFilter=require("./AbstractFilter");function FXAAFilter(){AbstractFilter.call(this, "\nprecision mediump float;\n\nattribute vec2 aVertexPosition;\nattribute vec2 aTextureCoord;\nattribute vec4 aColor;\n\nuniform mat3 projectionMatrix;\nuniform vec2 resolution;\n\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\n\nvarying vec2 vResolution;\n\n//texcoords computed in vertex step\n//to avoid dependent texture reads\nvarying vec2 v_rgbNW;\nvarying vec2 v_rgbNE;\nvarying vec2 v_rgbSW;\nvarying vec2 v_rgbSE;\nvarying vec2 v_rgbM;\n\n\nvoid texcoords(vec2 fragCoord, vec2 resolution,\n            out vec2 v_rgbNW, out vec2 v_rgbNE,\n            out vec2 v_rgbSW, out vec2 v_rgbSE,\n            out vec2 v_rgbM) {\n    vec2 inverseVP = 1.0 / resolution.xy;\n    v_rgbNW = (fragCoord + vec2(-1.0, -1.0)) * inverseVP;\n    v_rgbNE = (fragCoord + vec2(1.0, -1.0)) * inverseVP;\n    v_rgbSW = (fragCoord + vec2(-1.0, 1.0)) * inverseVP;\n    v_rgbSE = (fragCoord + vec2(1.0, 1.0)) * inverseVP;\n    v_rgbM = vec2(fragCoord * inverseVP);\n}\n\nvoid main(void){\n   gl_Position = vec4((projectionMatrix * vec3(aVertexPosition, 1.0)).xy, 0.0, 1.0);\n   vTextureCoord = aTextureCoord;\n   vColor = vec4(aColor.rgb * aColor.a, aColor.a);\n   vResolution = resolution;\n\n   //compute the texture coords and send them to varyings\n   texcoords(aTextureCoord * resolution, resolution, v_rgbNW, v_rgbNE, v_rgbSW, v_rgbSE, v_rgbM);\n}\n", "precision lowp float;\n\n\n/**\nBasic FXAA implementation based on the code on geeks3d.com with the\nmodification that the texture2DLod stuff was removed since it's\nunsupported by WebGL.\n\n--\n\nFrom:\nhttps://github.com/mitsuhiko/webgl-meincraft\n\nCopyright (c) 2011 by Armin Ronacher.\n\nSome rights reserved.\n\nRedistribution and use in source and binary forms, with or without\nmodification, are permitted provided that the following conditions are\nmet:\n\n    * Redistributions of source code must retain the above copyright\n      notice, this list of conditions and the following disclaimer.\n\n    * Redistributions in binary form must reproduce the above\n      copyright notice, this list of conditions and the following\n      disclaimer in the documentation and/or other materials provided\n      with the distribution.\n\n    * The names of the contributors may not be used to endorse or\n      promote products derived from this software without specific\n      prior written permission.\n\nTHIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n\"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\nLIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\nA PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT\nOWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,\nSPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT\nLIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,\nDATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY\nTHEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE\nOF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n*/\n\n#ifndef FXAA_REDUCE_MIN\n    #define FXAA_REDUCE_MIN   (1.0/ 128.0)\n#endif\n#ifndef FXAA_REDUCE_MUL\n    #define FXAA_REDUCE_MUL   (1.0 / 8.0)\n#endif\n#ifndef FXAA_SPAN_MAX\n    #define FXAA_SPAN_MAX     8.0\n#endif\n\n//optimized version for mobile, where dependent\n//texture reads can be a bottleneck\nvec4 fxaa(sampler2D tex, vec2 fragCoord, vec2 resolution,\n            vec2 v_rgbNW, vec2 v_rgbNE,\n            vec2 v_rgbSW, vec2 v_rgbSE,\n            vec2 v_rgbM) {\n    vec4 color;\n    mediump vec2 inverseVP = vec2(1.0 / resolution.x, 1.0 / resolution.y);\n    vec3 rgbNW = texture2D(tex, v_rgbNW).xyz;\n    vec3 rgbNE = texture2D(tex, v_rgbNE).xyz;\n    vec3 rgbSW = texture2D(tex, v_rgbSW).xyz;\n    vec3 rgbSE = texture2D(tex, v_rgbSE).xyz;\n    vec4 texColor = texture2D(tex, v_rgbM);\n    vec3 rgbM  = texColor.xyz;\n    vec3 luma = vec3(0.299, 0.587, 0.114);\n    float lumaNW = dot(rgbNW, luma);\n    float lumaNE = dot(rgbNE, luma);\n    float lumaSW = dot(rgbSW, luma);\n    float lumaSE = dot(rgbSE, luma);\n    float lumaM  = dot(rgbM,  luma);\n    float lumaMin = min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE)));\n    float lumaMax = max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));\n\n    mediump vec2 dir;\n    dir.x = -((lumaNW + lumaNE) - (lumaSW + lumaSE));\n    dir.y =  ((lumaNW + lumaSW) - (lumaNE + lumaSE));\n\n    float dirReduce = max((lumaNW + lumaNE + lumaSW + lumaSE) *\n                          (0.25 * FXAA_REDUCE_MUL), FXAA_REDUCE_MIN);\n\n    float rcpDirMin = 1.0 / (min(abs(dir.x), abs(dir.y)) + dirReduce);\n    dir = min(vec2(FXAA_SPAN_MAX, FXAA_SPAN_MAX),\n              max(vec2(-FXAA_SPAN_MAX, -FXAA_SPAN_MAX),\n              dir * rcpDirMin)) * inverseVP;\n\n    vec3 rgbA = 0.5 * (\n        texture2D(tex, fragCoord * inverseVP + dir * (1.0 / 3.0 - 0.5)).xyz +\n        texture2D(tex, fragCoord * inverseVP + dir * (2.0 / 3.0 - 0.5)).xyz);\n    vec3 rgbB = rgbA * 0.5 + 0.25 * (\n        texture2D(tex, fragCoord * inverseVP + dir * -0.5).xyz +\n        texture2D(tex, fragCoord * inverseVP + dir * 0.5).xyz);\n\n    float lumaB = dot(rgbB, luma);\n    if ((lumaB < lumaMin) || (lumaB > lumaMax))\n        color = vec4(rgbA, texColor.a);\n    else\n        color = vec4(rgbB, texColor.a);\n    return color;\n}\n\n\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\nvarying vec2 vResolution;\n\n//texcoords computed in vertex step\n//to avoid dependent texture reads\nvarying vec2 v_rgbNW;\nvarying vec2 v_rgbNE;\nvarying vec2 v_rgbSW;\nvarying vec2 v_rgbSE;\nvarying vec2 v_rgbM;\n\nuniform sampler2D uSampler;\n\n\nvoid main(void){\n\n    gl_FragColor = fxaa(uSampler, vTextureCoord * vResolution, vResolution, v_rgbNW, v_rgbNE, v_rgbSW, v_rgbSE, v_rgbM);\n\n}\n", {resolution:{type:"v2", value:{x:1, y:1}}});}FXAAFilter.prototype = Object.create(AbstractFilter.prototype);FXAAFilter.prototype.constructor = FXAAFilter;module.exports = FXAAFilter;FXAAFilter.prototype.applyFilter = function(renderer, input, output){var filterManager=renderer.filterManager;var shader=this.getShader(renderer);filterManager.applyFilter(shader, input, output);};}, {"./AbstractFilter":50}], 52:[function(require, module, exports){var AbstractFilter=require("./AbstractFilter"), math=require("../../../math");function SpriteMaskFilter(sprite){var maskMatrix=new math.Matrix();AbstractFilter.call(this, "attribute vec2 aVertexPosition;\nattribute vec2 aTextureCoord;\nattribute vec4 aColor;\n\nuniform mat3 projectionMatrix;\nuniform mat3 otherMatrix;\n\nvarying vec2 vMaskCoord;\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\n\nvoid main(void)\n{\n    gl_Position = vec4((projectionMatrix * vec3(aVertexPosition, 1.0)).xy, 0.0, 1.0);\n    vTextureCoord = aTextureCoord;\n    vMaskCoord = ( otherMatrix * vec3( aTextureCoord, 1.0)  ).xy;\n    vColor = vec4(aColor.rgb * aColor.a, aColor.a);\n}\n", "precision lowp float;\n\nvarying vec2 vMaskCoord;\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\n\nuniform sampler2D uSampler;\nuniform float alpha;\nuniform sampler2D mask;\n\nvoid main(void)\n{\n    // check clip! this will stop the mask bleeding out from the edges\n    vec2 text = abs( vMaskCoord - 0.5 );\n    text = step(0.5, text);\n    float clip = 1.0 - max(text.y, text.x);\n    vec4 original = texture2D(uSampler, vTextureCoord);\n    vec4 masky = texture2D(mask, vMaskCoord);\n    original *= (masky.r * masky.a * alpha * clip);\n    gl_FragColor = original;\n}\n", {mask:{type:"sampler2D", value:sprite._texture}, alpha:{type:"f", value:1}, otherMatrix:{type:"mat3", value:maskMatrix.toArray(true)}});this.maskSprite = sprite;this.maskMatrix = maskMatrix;}SpriteMaskFilter.prototype = Object.create(AbstractFilter.prototype);SpriteMaskFilter.prototype.constructor = SpriteMaskFilter;module.exports = SpriteMaskFilter;SpriteMaskFilter.prototype.applyFilter = function(renderer, input, output){var filterManager=renderer.filterManager;this.uniforms.mask.value = this.maskSprite._texture;filterManager.calculateMappedMatrix(input.frame, this.maskSprite, this.maskMatrix);this.uniforms.otherMatrix.value = this.maskMatrix.toArray(true);this.uniforms.alpha.value = this.maskSprite.worldAlpha;var shader=this.getShader(renderer);filterManager.applyFilter(shader, input, output);};Object.defineProperties(SpriteMaskFilter.prototype, {map:{get:function get(){return this.uniforms.mask.value;}, set:function set(value){this.uniforms.mask.value = value;}}, offset:{get:function get(){return this.uniforms.offset.value;}, set:function set(value){this.uniforms.offset.value = value;}}});}, {"../../../math":33, "./AbstractFilter":50}], 53:[function(require, module, exports){var WebGLManager=require("./WebGLManager");function BlendModeManager(renderer){WebGLManager.call(this, renderer);this.currentBlendMode = 99999;}BlendModeManager.prototype = Object.create(WebGLManager.prototype);BlendModeManager.prototype.constructor = BlendModeManager;module.exports = BlendModeManager;BlendModeManager.prototype.setBlendMode = function(blendMode){if(this.currentBlendMode === blendMode){return false;}this.currentBlendMode = blendMode;var mode=this.renderer.blendModes[this.currentBlendMode];this.renderer.gl.blendFunc(mode[0], mode[1]);return true;};}, {"./WebGLManager":58}], 54:[function(require, module, exports){var WebGLManager=require("./WebGLManager"), RenderTarget=require("../utils/RenderTarget"), CONST=require("../../../const"), Quad=require("../utils/Quad"), math=require("../../../math");function FilterManager(renderer){WebGLManager.call(this, renderer);this.filterStack = [];this.filterStack.push({renderTarget:renderer.currentRenderTarget, filter:[], bounds:null});this.texturePool = [];this.textureSize = new math.Rectangle(0, 0, renderer.width, renderer.height);this.currentFrame = null;}FilterManager.prototype = Object.create(WebGLManager.prototype);FilterManager.prototype.constructor = FilterManager;module.exports = FilterManager;FilterManager.prototype.onContextChange = function(){this.texturePool.length = 0;var gl=this.renderer.gl;this.quad = new Quad(gl);};FilterManager.prototype.setFilterStack = function(filterStack){this.filterStack = filterStack;};FilterManager.prototype.pushFilter = function(target, filters){var bounds=target.filterArea?target.filterArea.clone():target.getBounds();bounds.x = bounds.x | 0;bounds.y = bounds.y | 0;bounds.width = bounds.width | 0;bounds.height = bounds.height | 0;var padding=filters[0].padding | 0;bounds.x -= padding;bounds.y -= padding;bounds.width += padding * 2;bounds.height += padding * 2;if(this.renderer.currentRenderTarget.transform){var transform=this.renderer.currentRenderTarget.transform;bounds.x += transform.tx;bounds.y += transform.ty;this.capFilterArea(bounds);bounds.x -= transform.tx;bounds.y -= transform.ty;}else {this.capFilterArea(bounds);}if(bounds.width > 0 && bounds.height > 0){this.currentFrame = bounds;var texture=this.getRenderTarget();this.renderer.setRenderTarget(texture);texture.clear();this.filterStack.push({renderTarget:texture, filter:filters});}else {this.filterStack.push({renderTarget:null, filter:filters});}};FilterManager.prototype.popFilter = function(){var filterData=this.filterStack.pop();var previousFilterData=this.filterStack[this.filterStack.length - 1];var input=filterData.renderTarget;if(!filterData.renderTarget){return;}var output=previousFilterData.renderTarget;var gl=this.renderer.gl;this.currentFrame = input.frame;this.quad.map(this.textureSize, input.frame);gl.bindBuffer(gl.ARRAY_BUFFER, this.quad.vertexBuffer);gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.quad.indexBuffer);var filters=filterData.filter;gl.vertexAttribPointer(this.renderer.shaderManager.defaultShader.attributes.aVertexPosition, 2, gl.FLOAT, false, 0, 0);gl.vertexAttribPointer(this.renderer.shaderManager.defaultShader.attributes.aTextureCoord, 2, gl.FLOAT, false, 0, 2 * 4 * 4);gl.vertexAttribPointer(this.renderer.shaderManager.defaultShader.attributes.aColor, 4, gl.FLOAT, false, 0, 4 * 4 * 4);this.renderer.blendModeManager.setBlendMode(CONST.BLEND_MODES.NORMAL);if(filters.length === 1){if(filters[0].uniforms.dimensions){filters[0].uniforms.dimensions.value[0] = this.renderer.width;filters[0].uniforms.dimensions.value[1] = this.renderer.height;filters[0].uniforms.dimensions.value[2] = this.quad.vertices[0];filters[0].uniforms.dimensions.value[3] = this.quad.vertices[5];}filters[0].applyFilter(this.renderer, input, output);this.returnRenderTarget(input);}else {var flipTexture=input;var flopTexture=this.getRenderTarget(true);for(var i=0; i < filters.length - 1; i++) {var filter=filters[i];if(filter.uniforms.dimensions){filter.uniforms.dimensions.value[0] = this.renderer.width;filter.uniforms.dimensions.value[1] = this.renderer.height;filter.uniforms.dimensions.value[2] = this.quad.vertices[0];filter.uniforms.dimensions.value[3] = this.quad.vertices[5];}filter.applyFilter(this.renderer, flipTexture, flopTexture);var temp=flipTexture;flipTexture = flopTexture;flopTexture = temp;}filters[filters.length - 1].applyFilter(this.renderer, flipTexture, output);this.returnRenderTarget(flipTexture);this.returnRenderTarget(flopTexture);}return filterData.filter;};FilterManager.prototype.getRenderTarget = function(clear){var renderTarget=this.texturePool.pop() || new RenderTarget(this.renderer.gl, this.textureSize.width, this.textureSize.height, CONST.SCALE_MODES.LINEAR, this.renderer.resolution * CONST.FILTER_RESOLUTION);renderTarget.frame = this.currentFrame;if(clear){renderTarget.clear(true);}return renderTarget;};FilterManager.prototype.returnRenderTarget = function(renderTarget){this.texturePool.push(renderTarget);};FilterManager.prototype.applyFilter = function(shader, inputTarget, outputTarget, clear){var gl=this.renderer.gl;this.renderer.setRenderTarget(outputTarget);if(clear){outputTarget.clear();}this.renderer.shaderManager.setShader(shader);shader.uniforms.projectionMatrix.value = this.renderer.currentRenderTarget.projectionMatrix.toArray(true);shader.syncUniforms();gl.activeTexture(gl.TEXTURE0);gl.bindTexture(gl.TEXTURE_2D, inputTarget.texture);gl.drawElements(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0);this.renderer.drawCount++;};FilterManager.prototype.calculateMappedMatrix = function(filterArea, sprite, outputMatrix){var worldTransform=sprite.worldTransform.copy(math.Matrix.TEMP_MATRIX), texture=sprite._texture.baseTexture;var mappedMatrix=outputMatrix.identity();var ratio=this.textureSize.height / this.textureSize.width;mappedMatrix.translate(filterArea.x / this.textureSize.width, filterArea.y / this.textureSize.height);mappedMatrix.scale(1, ratio);var translateScaleX=this.textureSize.width / texture.width;var translateScaleY=this.textureSize.height / texture.height;worldTransform.tx /= texture.width * translateScaleX;worldTransform.ty /= texture.width * translateScaleX;worldTransform.invert();mappedMatrix.prepend(worldTransform);mappedMatrix.scale(1, 1 / ratio);mappedMatrix.scale(translateScaleX, translateScaleY);mappedMatrix.translate(sprite.anchor.x, sprite.anchor.y);return mappedMatrix;};FilterManager.prototype.capFilterArea = function(filterArea){if(filterArea.x < 0){filterArea.width += filterArea.x;filterArea.x = 0;}if(filterArea.y < 0){filterArea.height += filterArea.y;filterArea.y = 0;}if(filterArea.x + filterArea.width > this.textureSize.width){filterArea.width = this.textureSize.width - filterArea.x;}if(filterArea.y + filterArea.height > this.textureSize.height){filterArea.height = this.textureSize.height - filterArea.y;}};FilterManager.prototype.resize = function(width, height){this.textureSize.width = width;this.textureSize.height = height;for(var i=0; i < this.texturePool.length; i++) {this.texturePool[i].resize(width, height);}};FilterManager.prototype.destroy = function(){this.quad.destroy();WebGLManager.prototype.destroy.call(this);this.filterStack = null;this.offsetY = 0;for(var i=0; i < this.texturePool.length; i++) {this.texturePool[i].destroy();}this.texturePool = null;};}, {"../../../const":22, "../../../math":33, "../utils/Quad":64, "../utils/RenderTarget":65, "./WebGLManager":58}], 55:[function(require, module, exports){var WebGLManager=require("./WebGLManager"), AlphaMaskFilter=require("../filters/SpriteMaskFilter");function MaskManager(renderer){WebGLManager.call(this, renderer);this.stencilStack = [];this.reverse = true;this.count = 0;this.alphaMaskPool = [];}MaskManager.prototype = Object.create(WebGLManager.prototype);MaskManager.prototype.constructor = MaskManager;module.exports = MaskManager;MaskManager.prototype.pushMask = function(target, maskData){if(maskData.texture){this.pushSpriteMask(target, maskData);}else {this.pushStencilMask(target, maskData);}};MaskManager.prototype.popMask = function(target, maskData){if(maskData.texture){this.popSpriteMask(target, maskData);}else {this.popStencilMask(target, maskData);}};MaskManager.prototype.pushSpriteMask = function(target, maskData){var alphaMaskFilter=this.alphaMaskPool.pop();if(!alphaMaskFilter){alphaMaskFilter = [new AlphaMaskFilter(maskData)];}alphaMaskFilter[0].maskSprite = maskData;this.renderer.filterManager.pushFilter(target, alphaMaskFilter);};MaskManager.prototype.popSpriteMask = function(){var filters=this.renderer.filterManager.popFilter();this.alphaMaskPool.push(filters);};MaskManager.prototype.pushStencilMask = function(target, maskData){this.renderer.stencilManager.pushMask(maskData);};MaskManager.prototype.popStencilMask = function(target, maskData){this.renderer.stencilManager.popMask(maskData);};}, {"../filters/SpriteMaskFilter":52, "./WebGLManager":58}], 56:[function(require, module, exports){var WebGLManager=require("./WebGLManager"), TextureShader=require("../shaders/TextureShader"), ComplexPrimitiveShader=require("../shaders/ComplexPrimitiveShader"), PrimitiveShader=require("../shaders/PrimitiveShader"), utils=require("../../../utils");function ShaderManager(renderer){WebGLManager.call(this, renderer);this.maxAttibs = 10;this.attribState = [];this.tempAttribState = [];for(var i=0; i < this.maxAttibs; i++) {this.attribState[i] = false;}this.stack = [];this._currentId = -1;this.currentShader = null;}ShaderManager.prototype = Object.create(WebGLManager.prototype);ShaderManager.prototype.constructor = ShaderManager;utils.pluginTarget.mixin(ShaderManager);module.exports = ShaderManager;ShaderManager.prototype.onContextChange = function(){this.initPlugins();var gl=this.renderer.gl;this.maxAttibs = gl.getParameter(gl.MAX_VERTEX_ATTRIBS);this.attribState = [];for(var i=0; i < this.maxAttibs; i++) {this.attribState[i] = false;}this.defaultShader = new TextureShader(this);this.primitiveShader = new PrimitiveShader(this);this.complexPrimitiveShader = new ComplexPrimitiveShader(this);};ShaderManager.prototype.setAttribs = function(attribs){var i;for(i = 0; i < this.tempAttribState.length; i++) {this.tempAttribState[i] = false;}for(var a in attribs) {this.tempAttribState[attribs[a]] = true;}var gl=this.renderer.gl;for(i = 0; i < this.attribState.length; i++) {if(this.attribState[i] !== this.tempAttribState[i]){this.attribState[i] = this.tempAttribState[i];if(this.attribState[i]){gl.enableVertexAttribArray(i);}else {gl.disableVertexAttribArray(i);}}}};ShaderManager.prototype.setShader = function(shader){if(this._currentId === shader.uid){return false;}this._currentId = shader.uid;this.currentShader = shader;this.renderer.gl.useProgram(shader.program);this.setAttribs(shader.attributes);return true;};ShaderManager.prototype.destroy = function(){this.primitiveShader.destroy();this.complexPrimitiveShader.destroy();WebGLManager.prototype.destroy.call(this);this.destroyPlugins();this.attribState = null;this.tempAttribState = null;};}, {"../../../utils":77, "../shaders/ComplexPrimitiveShader":59, "../shaders/PrimitiveShader":60, "../shaders/TextureShader":62, "./WebGLManager":58}], 57:[function(require, module, exports){var WebGLManager=require("./WebGLManager"), utils=require("../../../utils");function WebGLMaskManager(renderer){WebGLManager.call(this, renderer);this.stencilMaskStack = null;}WebGLMaskManager.prototype = Object.create(WebGLManager.prototype);WebGLMaskManager.prototype.constructor = WebGLMaskManager;module.exports = WebGLMaskManager;WebGLMaskManager.prototype.setMaskStack = function(stencilMaskStack){this.stencilMaskStack = stencilMaskStack;var gl=this.renderer.gl;if(stencilMaskStack.stencilStack.length === 0){gl.disable(gl.STENCIL_TEST);}else {gl.enable(gl.STENCIL_TEST);}};WebGLMaskManager.prototype.pushStencil = function(graphics, webGLData){this.renderer.currentRenderTarget.attachStencilBuffer();var gl=this.renderer.gl, sms=this.stencilMaskStack;this.bindGraphics(graphics, webGLData);if(sms.stencilStack.length === 0){gl.enable(gl.STENCIL_TEST);gl.clear(gl.STENCIL_BUFFER_BIT);sms.reverse = true;sms.count = 0;}sms.stencilStack.push(webGLData);var level=sms.count;gl.colorMask(false, false, false, false);gl.stencilFunc(gl.ALWAYS, 0, 255);gl.stencilOp(gl.KEEP, gl.KEEP, gl.INVERT);if(webGLData.mode === 1){gl.drawElements(gl.TRIANGLE_FAN, webGLData.indices.length - 4, gl.UNSIGNED_SHORT, 0);if(sms.reverse){gl.stencilFunc(gl.EQUAL, 255 - level, 255);gl.stencilOp(gl.KEEP, gl.KEEP, gl.DECR);}else {gl.stencilFunc(gl.EQUAL, level, 255);gl.stencilOp(gl.KEEP, gl.KEEP, gl.INCR);}gl.drawElements(gl.TRIANGLE_FAN, 4, gl.UNSIGNED_SHORT, (webGLData.indices.length - 4) * 2);if(sms.reverse){gl.stencilFunc(gl.EQUAL, 255 - (level + 1), 255);}else {gl.stencilFunc(gl.EQUAL, level + 1, 255);}sms.reverse = !sms.reverse;}else {if(!sms.reverse){gl.stencilFunc(gl.EQUAL, 255 - level, 255);gl.stencilOp(gl.KEEP, gl.KEEP, gl.DECR);}else {gl.stencilFunc(gl.EQUAL, level, 255);gl.stencilOp(gl.KEEP, gl.KEEP, gl.INCR);}gl.drawElements(gl.TRIANGLE_STRIP, webGLData.indices.length, gl.UNSIGNED_SHORT, 0);if(!sms.reverse){gl.stencilFunc(gl.EQUAL, 255 - (level + 1), 255);}else {gl.stencilFunc(gl.EQUAL, level + 1, 255);}}gl.colorMask(true, true, true, true);gl.stencilOp(gl.KEEP, gl.KEEP, gl.KEEP);sms.count++;};WebGLMaskManager.prototype.bindGraphics = function(graphics, webGLData){var gl=this.renderer.gl;var shader;if(webGLData.mode === 1){shader = this.renderer.shaderManager.complexPrimitiveShader;this.renderer.shaderManager.setShader(shader);gl.uniformMatrix3fv(shader.uniforms.translationMatrix._location, false, graphics.worldTransform.toArray(true));gl.uniformMatrix3fv(shader.uniforms.projectionMatrix._location, false, this.renderer.currentRenderTarget.projectionMatrix.toArray(true));gl.uniform3fv(shader.uniforms.tint._location, utils.hex2rgb(graphics.tint));gl.uniform3fv(shader.uniforms.color._location, webGLData.color);gl.uniform1f(shader.uniforms.alpha._location, graphics.worldAlpha);gl.bindBuffer(gl.ARRAY_BUFFER, webGLData.buffer);gl.vertexAttribPointer(shader.attributes.aVertexPosition, 2, gl.FLOAT, false, 4 * 2, 0);gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, webGLData.indexBuffer);}else {shader = this.renderer.shaderManager.primitiveShader;this.renderer.shaderManager.setShader(shader);gl.uniformMatrix3fv(shader.uniforms.translationMatrix._location, false, graphics.worldTransform.toArray(true));gl.uniformMatrix3fv(shader.uniforms.projectionMatrix._location, false, this.renderer.currentRenderTarget.projectionMatrix.toArray(true));gl.uniform3fv(shader.uniforms.tint._location, utils.hex2rgb(graphics.tint));gl.uniform1f(shader.uniforms.alpha._location, graphics.worldAlpha);gl.bindBuffer(gl.ARRAY_BUFFER, webGLData.buffer);gl.vertexAttribPointer(shader.attributes.aVertexPosition, 2, gl.FLOAT, false, 4 * 6, 0);gl.vertexAttribPointer(shader.attributes.aColor, 4, gl.FLOAT, false, 4 * 6, 2 * 4);gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, webGLData.indexBuffer);}};WebGLMaskManager.prototype.popStencil = function(graphics, webGLData){var gl=this.renderer.gl, sms=this.stencilMaskStack;sms.stencilStack.pop();sms.count--;if(sms.stencilStack.length === 0){gl.disable(gl.STENCIL_TEST);}else {var level=sms.count;this.bindGraphics(graphics, webGLData);gl.colorMask(false, false, false, false);if(webGLData.mode === 1){sms.reverse = !sms.reverse;if(sms.reverse){gl.stencilFunc(gl.EQUAL, 255 - (level + 1), 255);gl.stencilOp(gl.KEEP, gl.KEEP, gl.INCR);}else {gl.stencilFunc(gl.EQUAL, level + 1, 255);gl.stencilOp(gl.KEEP, gl.KEEP, gl.DECR);}gl.drawElements(gl.TRIANGLE_FAN, 4, gl.UNSIGNED_SHORT, (webGLData.indices.length - 4) * 2);gl.stencilFunc(gl.ALWAYS, 0, 255);gl.stencilOp(gl.KEEP, gl.KEEP, gl.INVERT);gl.drawElements(gl.TRIANGLE_FAN, webGLData.indices.length - 4, gl.UNSIGNED_SHORT, 0);this.renderer.drawCount += 2;if(!sms.reverse){gl.stencilFunc(gl.EQUAL, 255 - level, 255);}else {gl.stencilFunc(gl.EQUAL, level, 255);}}else {if(!sms.reverse){gl.stencilFunc(gl.EQUAL, 255 - (level + 1), 255);gl.stencilOp(gl.KEEP, gl.KEEP, gl.INCR);}else {gl.stencilFunc(gl.EQUAL, level + 1, 255);gl.stencilOp(gl.KEEP, gl.KEEP, gl.DECR);}gl.drawElements(gl.TRIANGLE_STRIP, webGLData.indices.length, gl.UNSIGNED_SHORT, 0);this.renderer.drawCount++;if(!sms.reverse){gl.stencilFunc(gl.EQUAL, 255 - level, 255);}else {gl.stencilFunc(gl.EQUAL, level, 255);}}gl.colorMask(true, true, true, true);gl.stencilOp(gl.KEEP, gl.KEEP, gl.KEEP);}};WebGLMaskManager.prototype.destroy = function(){WebGLManager.prototype.destroy.call(this);this.stencilMaskStack.stencilStack = null;};WebGLMaskManager.prototype.pushMask = function(maskData){this.renderer.setObjectRenderer(this.renderer.plugins.graphics);if(maskData.dirty){this.renderer.plugins.graphics.updateGraphics(maskData, this.renderer.gl);}if(!maskData._webGL[this.renderer.gl.id].data.length){return;}this.pushStencil(maskData, maskData._webGL[this.renderer.gl.id].data[0]);};WebGLMaskManager.prototype.popMask = function(maskData){this.renderer.setObjectRenderer(this.renderer.plugins.graphics);this.popStencil(maskData, maskData._webGL[this.renderer.gl.id].data[0]);};}, {"../../../utils":77, "./WebGLManager":58}], 58:[function(require, module, exports){function WebGLManager(renderer){this.renderer = renderer;this.renderer.on("context", this.onContextChange, this);}WebGLManager.prototype.constructor = WebGLManager;module.exports = WebGLManager;WebGLManager.prototype.onContextChange = function(){};WebGLManager.prototype.destroy = function(){this.renderer.off("context", this.onContextChange, this);this.renderer = null;};}, {}], 59:[function(require, module, exports){var Shader=require("./Shader");function ComplexPrimitiveShader(shaderManager){Shader.call(this, shaderManager, ["attribute vec2 aVertexPosition;", "uniform mat3 translationMatrix;", "uniform mat3 projectionMatrix;", "uniform vec3 tint;", "uniform float alpha;", "uniform vec3 color;", "varying vec4 vColor;", "void main(void){", "   gl_Position = vec4((projectionMatrix * translationMatrix * vec3(aVertexPosition, 1.0)).xy, 0.0, 1.0);", "   vColor = vec4(color * alpha * tint, alpha);", "}"].join("\n"), ["precision mediump float;", "varying vec4 vColor;", "void main(void){", "   gl_FragColor = vColor;", "}"].join("\n"), {tint:{type:"3f", value:[0, 0, 0]}, alpha:{type:"1f", value:0}, color:{type:"3f", value:[0, 0, 0]}, translationMatrix:{type:"mat3", value:new Float32Array(9)}, projectionMatrix:{type:"mat3", value:new Float32Array(9)}}, {aVertexPosition:0});}ComplexPrimitiveShader.prototype = Object.create(Shader.prototype);ComplexPrimitiveShader.prototype.constructor = ComplexPrimitiveShader;module.exports = ComplexPrimitiveShader;}, {"./Shader":61}], 60:[function(require, module, exports){var Shader=require("./Shader");function PrimitiveShader(shaderManager){Shader.call(this, shaderManager, ["attribute vec2 aVertexPosition;", "attribute vec4 aColor;", "uniform mat3 translationMatrix;", "uniform mat3 projectionMatrix;", "uniform float alpha;", "uniform float flipY;", "uniform vec3 tint;", "varying vec4 vColor;", "void main(void){", "   gl_Position = vec4((projectionMatrix * translationMatrix * vec3(aVertexPosition, 1.0)).xy, 0.0, 1.0);", "   vColor = aColor * vec4(tint * alpha, alpha);", "}"].join("\n"), ["precision mediump float;", "varying vec4 vColor;", "void main(void){", "   gl_FragColor = vColor;", "}"].join("\n"), {tint:{type:"3f", value:[0, 0, 0]}, alpha:{type:"1f", value:0}, translationMatrix:{type:"mat3", value:new Float32Array(9)}, projectionMatrix:{type:"mat3", value:new Float32Array(9)}}, {aVertexPosition:0, aColor:0});}PrimitiveShader.prototype = Object.create(Shader.prototype);PrimitiveShader.prototype.constructor = PrimitiveShader;module.exports = PrimitiveShader;}, {"./Shader":61}], 61:[function(require, module, exports){var utils=require("../../../utils");function Shader(shaderManager, vertexSrc, fragmentSrc, uniforms, attributes){if(!vertexSrc || !fragmentSrc){throw new Error("Pixi.js Error. Shader requires vertexSrc and fragmentSrc");}this.uid = utils.uid();this.gl = shaderManager.renderer.gl;this.shaderManager = shaderManager;this.program = null;this.uniforms = uniforms || {};this.attributes = attributes || {};this.textureCount = 1;this.vertexSrc = vertexSrc;this.fragmentSrc = fragmentSrc;this.init();}Shader.prototype.constructor = Shader;module.exports = Shader;Shader.prototype.init = function(){this.compile();this.gl.useProgram(this.program);this.cacheUniformLocations(Object.keys(this.uniforms));this.cacheAttributeLocations(Object.keys(this.attributes));};Shader.prototype.cacheUniformLocations = function(keys){for(var i=0; i < keys.length; ++i) {this.uniforms[keys[i]]._location = this.gl.getUniformLocation(this.program, keys[i]);}};Shader.prototype.cacheAttributeLocations = function(keys){for(var i=0; i < keys.length; ++i) {this.attributes[keys[i]] = this.gl.getAttribLocation(this.program, keys[i]);}};Shader.prototype.compile = function(){var gl=this.gl;var glVertShader=this._glCompile(gl.VERTEX_SHADER, this.vertexSrc);var glFragShader=this._glCompile(gl.FRAGMENT_SHADER, this.fragmentSrc);var program=gl.createProgram();gl.attachShader(program, glVertShader);gl.attachShader(program, glFragShader);gl.linkProgram(program);if(!gl.getProgramParameter(program, gl.LINK_STATUS)){console.error("Pixi.js Error: Could not initialize shader.");console.error("gl.VALIDATE_STATUS", gl.getProgramParameter(program, gl.VALIDATE_STATUS));console.error("gl.getError()", gl.getError());if(gl.getProgramInfoLog(program) !== ""){console.warn("Pixi.js Warning: gl.getProgramInfoLog()", gl.getProgramInfoLog(program));}gl.deleteProgram(program);program = null;}gl.deleteShader(glVertShader);gl.deleteShader(glFragShader);return this.program = program;};Shader.prototype.syncUniform = function(uniform){var location=uniform._location, value=uniform.value, gl=this.gl, i, il;switch(uniform.type){case "b":case "bool":case "boolean":gl.uniform1i(location, value?1:0);break;case "i":case "1i":gl.uniform1i(location, value);break;case "f":case "1f":gl.uniform1f(location, value);break;case "2f":gl.uniform2f(location, value[0], value[1]);break;case "3f":gl.uniform3f(location, value[0], value[1], value[2]);break;case "4f":gl.uniform4f(location, value[0], value[1], value[2], value[3]);break;case "v2":gl.uniform2f(location, value.x, value.y);break;case "v3":gl.uniform3f(location, value.x, value.y, value.z);break;case "v4":gl.uniform4f(location, value.x, value.y, value.z, value.w);break;case "1iv":gl.uniform1iv(location, value);break;case "2iv":gl.uniform2iv(location, value);break;case "3iv":gl.uniform3iv(location, value);break;case "4iv":gl.uniform4iv(location, value);break;case "1fv":gl.uniform1fv(location, value);break;case "2fv":gl.uniform2fv(location, value);break;case "3fv":gl.uniform3fv(location, value);break;case "4fv":gl.uniform4fv(location, value);break;case "m2":case "mat2":case "Matrix2fv":gl.uniformMatrix2fv(location, uniform.transpose, value);break;case "m3":case "mat3":case "Matrix3fv":gl.uniformMatrix3fv(location, uniform.transpose, value);break;case "m4":case "mat4":case "Matrix4fv":gl.uniformMatrix4fv(location, uniform.transpose, value);break;case "c":if(typeof value === "number"){value = utils.hex2rgb(value);}gl.uniform3f(location, value[0], value[1], value[2]);break;case "iv1":gl.uniform1iv(location, value);break;case "iv":gl.uniform3iv(location, value);break;case "fv1":gl.uniform1fv(location, value);break;case "fv":gl.uniform3fv(location, value);break;case "v2v":if(!uniform._array){uniform._array = new Float32Array(2 * value.length);}for(i = 0, il = value.length; i < il; ++i) {uniform._array[i * 2] = value[i].x;uniform._array[i * 2 + 1] = value[i].y;}gl.uniform2fv(location, uniform._array);break;case "v3v":if(!uniform._array){uniform._array = new Float32Array(3 * value.length);}for(i = 0, il = value.length; i < il; ++i) {uniform._array[i * 3] = value[i].x;uniform._array[i * 3 + 1] = value[i].y;uniform._array[i * 3 + 2] = value[i].z;}gl.uniform3fv(location, uniform._array);break;case "v4v":if(!uniform._array){uniform._array = new Float32Array(4 * value.length);}for(i = 0, il = value.length; i < il; ++i) {uniform._array[i * 4] = value[i].x;uniform._array[i * 4 + 1] = value[i].y;uniform._array[i * 4 + 2] = value[i].z;uniform._array[i * 4 + 3] = value[i].w;}gl.uniform4fv(location, uniform._array);break;case "t":case "sampler2D":if(!uniform.value || !uniform.value.baseTexture.hasLoaded){break;}gl.activeTexture(gl["TEXTURE" + this.textureCount]);var texture=uniform.value.baseTexture._glTextures[gl.id];if(!texture){this.initSampler2D(uniform);texture = uniform.value.baseTexture._glTextures[gl.id];}gl.bindTexture(gl.TEXTURE_2D, texture);gl.uniform1i(uniform._location, this.textureCount);this.textureCount++;break;default:console.warn("Pixi.js Shader Warning: Unknown uniform type: " + uniform.type);}};Shader.prototype.syncUniforms = function(){this.textureCount = 1;for(var key in this.uniforms) {this.syncUniform(this.uniforms[key]);}};Shader.prototype.initSampler2D = function(uniform){var gl=this.gl;var texture=uniform.value.baseTexture;if(!texture.hasLoaded){return;}if(uniform.textureData){var data=uniform.textureData;texture._glTextures[gl.id] = gl.createTexture();gl.bindTexture(gl.TEXTURE_2D, texture._glTextures[gl.id]);gl.pixelStorei(gl.UNPACK_PREMULTIPLY_ALPHA_WEBGL, texture.premultipliedAlpha);gl.texImage2D(gl.TEXTURE_2D, 0, data.luminance?gl.LUMINANCE:gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, texture.source);gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, data.magFilter?data.magFilter:gl.LINEAR);gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, data.wrapS?data.wrapS:gl.CLAMP_TO_EDGE);gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, data.wrapS?data.wrapS:gl.CLAMP_TO_EDGE);gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, data.wrapT?data.wrapT:gl.CLAMP_TO_EDGE);}else {this.shaderManager.renderer.updateTexture(texture);}};Shader.prototype.destroy = function(){this.gl.deleteProgram(this.program);this.gl = null;this.uniforms = null;this.attributes = null;this.vertexSrc = null;this.fragmentSrc = null;};Shader.prototype._glCompile = function(type, src){var shader=this.gl.createShader(type);this.gl.shaderSource(shader, src);this.gl.compileShader(shader);if(!this.gl.getShaderParameter(shader, this.gl.COMPILE_STATUS)){console.log(this.gl.getShaderInfoLog(shader));return null;}return shader;};}, {"../../../utils":77}], 62:[function(require, module, exports){var Shader=require("./Shader");function TextureShader(shaderManager, vertexSrc, fragmentSrc, customUniforms, customAttributes){var uniforms={uSampler:{type:"sampler2D", value:0}, projectionMatrix:{type:"mat3", value:new Float32Array([1, 0, 0, 0, 1, 0, 0, 0, 1])}};if(customUniforms){for(var u in customUniforms) {uniforms[u] = customUniforms[u];}}var attributes={aVertexPosition:0, aTextureCoord:0, aColor:0};if(customAttributes){for(var a in customAttributes) {attributes[a] = customAttributes[a];}}vertexSrc = vertexSrc || TextureShader.defaultVertexSrc;fragmentSrc = fragmentSrc || TextureShader.defaultFragmentSrc;Shader.call(this, shaderManager, vertexSrc, fragmentSrc, uniforms, attributes);}TextureShader.prototype = Object.create(Shader.prototype);TextureShader.prototype.constructor = TextureShader;module.exports = TextureShader;TextureShader.defaultVertexSrc = ["precision lowp float;", "attribute vec2 aVertexPosition;", "attribute vec2 aTextureCoord;", "attribute vec4 aColor;", "uniform mat3 projectionMatrix;", "varying vec2 vTextureCoord;", "varying vec4 vColor;", "void main(void){", "   gl_Position = vec4((projectionMatrix * vec3(aVertexPosition, 1.0)).xy, 0.0, 1.0);", "   vTextureCoord = aTextureCoord;", "   vColor = vec4(aColor.rgb * aColor.a, aColor.a);", "}"].join("\n");TextureShader.defaultFragmentSrc = ["precision lowp float;", "varying vec2 vTextureCoord;", "varying vec4 vColor;", "uniform sampler2D uSampler;", "void main(void){", "   gl_FragColor = texture2D(uSampler, vTextureCoord) * vColor ;", "}"].join("\n");}, {"./Shader":61}], 63:[function(require, module, exports){var WebGLManager=require("../managers/WebGLManager");function ObjectRenderer(renderer){WebGLManager.call(this, renderer);}ObjectRenderer.prototype = Object.create(WebGLManager.prototype);ObjectRenderer.prototype.constructor = ObjectRenderer;module.exports = ObjectRenderer;ObjectRenderer.prototype.start = function(){};ObjectRenderer.prototype.stop = function(){this.flush();};ObjectRenderer.prototype.flush = function(){};ObjectRenderer.prototype.render = function(object){};}, {"../managers/WebGLManager":58}], 64:[function(require, module, exports){function Quad(gl){this.gl = gl;this.vertices = new Float32Array([0, 0, 200, 0, 200, 200, 0, 200]);this.uvs = new Float32Array([0, 0, 1, 0, 1, 1, 0, 1]);this.colors = new Float32Array([1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);this.indices = new Uint16Array([0, 1, 2, 0, 3, 2]);this.vertexBuffer = gl.createBuffer();this.indexBuffer = gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);gl.bufferData(gl.ARRAY_BUFFER, (8 + 8 + 16) * 4, gl.DYNAMIC_DRAW);gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.indexBuffer);gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, this.indices, gl.STATIC_DRAW);this.upload();}Quad.prototype.constructor = Quad;Quad.prototype.map = function(rect, rect2){var x=0;var y=0;this.uvs[0] = x;this.uvs[1] = y;this.uvs[2] = x + rect2.width / rect.width;this.uvs[3] = y;this.uvs[4] = x + rect2.width / rect.width;this.uvs[5] = y + rect2.height / rect.height;this.uvs[6] = x;this.uvs[7] = y + rect2.height / rect.height;x = rect2.x;y = rect2.y;this.vertices[0] = x;this.vertices[1] = y;this.vertices[2] = x + rect2.width;this.vertices[3] = y;this.vertices[4] = x + rect2.width;this.vertices[5] = y + rect2.height;this.vertices[6] = x;this.vertices[7] = y + rect2.height;this.upload();};Quad.prototype.upload = function(){var gl=this.gl;gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);gl.bufferSubData(gl.ARRAY_BUFFER, 0, this.vertices);gl.bufferSubData(gl.ARRAY_BUFFER, 8 * 4, this.uvs);gl.bufferSubData(gl.ARRAY_BUFFER, (8 + 8) * 4, this.colors);};Quad.prototype.destroy = function(){var gl=this.gl;gl.deleteBuffer(this.vertexBuffer);gl.deleteBuffer(this.indexBuffer);};module.exports = Quad;}, {}], 65:[function(require, module, exports){var math=require("../../../math"), utils=require("../../../utils"), CONST=require("../../../const"), StencilMaskStack=require("./StencilMaskStack");var RenderTarget=function RenderTarget(gl, width, height, scaleMode, resolution, root){this.gl = gl;this.frameBuffer = null;this.texture = null;this.size = new math.Rectangle(0, 0, 1, 1);this.resolution = resolution || CONST.RESOLUTION;this.projectionMatrix = new math.Matrix();this.transform = null;this.frame = null;this.stencilBuffer = null;this.stencilMaskStack = new StencilMaskStack();this.filterStack = [{renderTarget:this, filter:[], bounds:this.size}];this.scaleMode = scaleMode || CONST.SCALE_MODES.DEFAULT;this.root = root;if(!this.root){this.frameBuffer = gl.createFramebuffer();this.texture = gl.createTexture();gl.bindTexture(gl.TEXTURE_2D, this.texture);gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, scaleMode === CONST.SCALE_MODES.LINEAR?gl.LINEAR:gl.NEAREST);gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, scaleMode === CONST.SCALE_MODES.LINEAR?gl.LINEAR:gl.NEAREST);var isPowerOfTwo=utils.isPowerOfTwo(width, height);if(!isPowerOfTwo){gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);}else {gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);}gl.bindFramebuffer(gl.FRAMEBUFFER, this.frameBuffer);gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, this.texture, 0);}this.resize(width, height);};RenderTarget.prototype.constructor = RenderTarget;module.exports = RenderTarget;RenderTarget.prototype.clear = function(bind){var gl=this.gl;if(bind){gl.bindFramebuffer(gl.FRAMEBUFFER, this.frameBuffer);}gl.clearColor(0, 0, 0, 0);gl.clear(gl.COLOR_BUFFER_BIT);};RenderTarget.prototype.attachStencilBuffer = function(){if(this.stencilBuffer){return;}if(!this.root){var gl=this.gl;this.stencilBuffer = gl.createRenderbuffer();gl.bindRenderbuffer(gl.RENDERBUFFER, this.stencilBuffer);gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_STENCIL_ATTACHMENT, gl.RENDERBUFFER, this.stencilBuffer);gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_STENCIL, this.size.width * this.resolution, this.size.height * this.resolution);}};RenderTarget.prototype.activate = function(){var gl=this.gl;gl.bindFramebuffer(gl.FRAMEBUFFER, this.frameBuffer);var projectionFrame=this.frame || this.size;this.calculateProjection(projectionFrame);if(this.transform){this.projectionMatrix.append(this.transform);}gl.viewport(0, 0, projectionFrame.width * this.resolution, projectionFrame.height * this.resolution);};RenderTarget.prototype.calculateProjection = function(projectionFrame){var pm=this.projectionMatrix;pm.identity();if(!this.root){pm.a = 1 / projectionFrame.width * 2;pm.d = 1 / projectionFrame.height * 2;pm.tx = -1 - projectionFrame.x * pm.a;pm.ty = -1 - projectionFrame.y * pm.d;}else {pm.a = 1 / projectionFrame.width * 2;pm.d = -1 / projectionFrame.height * 2;pm.tx = -1 - projectionFrame.x * pm.a;pm.ty = 1 - projectionFrame.y * pm.d;}};RenderTarget.prototype.resize = function(width, height){width = width | 0;height = height | 0;if(this.size.width === width && this.size.height === height){return;}this.size.width = width;this.size.height = height;if(!this.root){var gl=this.gl;gl.bindTexture(gl.TEXTURE_2D, this.texture);gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, width * this.resolution, height * this.resolution, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);if(this.stencilBuffer){gl.bindRenderbuffer(gl.RENDERBUFFER, this.stencilBuffer);gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_STENCIL, width * this.resolution, height * this.resolution);}}var projectionFrame=this.frame || this.size;this.calculateProjection(projectionFrame);};RenderTarget.prototype.destroy = function(){var gl=this.gl;gl.deleteRenderbuffer(this.stencilBuffer);gl.deleteFramebuffer(this.frameBuffer);gl.deleteTexture(this.texture);this.frameBuffer = null;this.texture = null;};}, {"../../../const":22, "../../../math":33, "../../../utils":77, "./StencilMaskStack":66}], 66:[function(require, module, exports){function StencilMaskStack(){this.stencilStack = [];this.reverse = true;this.count = 0;}StencilMaskStack.prototype.constructor = StencilMaskStack;module.exports = StencilMaskStack;}, {}], 67:[function(require, module, exports){var math=require("../math"), Texture=require("../textures/Texture"), Container=require("../display/Container"), CanvasTinter=require("../renderers/canvas/utils/CanvasTinter"), utils=require("../utils"), CONST=require("../const"), tempPoint=new math.Point(), GroupD8=math.GroupD8, canvasRenderWorldTransform=new math.Matrix();function Sprite(texture){Container.call(this);this.anchor = new math.Point();this._texture = null;this._width = 0;this._height = 0;this.tint = 16777215;this.blendMode = CONST.BLEND_MODES.NORMAL;this.shader = null;this.cachedTint = 16777215;this.texture = texture || Texture.EMPTY;}Sprite.prototype = Object.create(Container.prototype);Sprite.prototype.constructor = Sprite;module.exports = Sprite;Object.defineProperties(Sprite.prototype, {width:{get:function get(){return Math.abs(this.scale.x) * this.texture._frame.width;}, set:function set(value){var sign=utils.sign(this.scale.x) || 1;this.scale.x = sign * value / this.texture._frame.width;this._width = value;}}, height:{get:function get(){return Math.abs(this.scale.y) * this.texture._frame.height;}, set:function set(value){var sign=utils.sign(this.scale.y) || 1;this.scale.y = sign * value / this.texture._frame.height;this._height = value;}}, texture:{get:function get(){return this._texture;}, set:function set(value){if(this._texture === value){return;}this._texture = value;this.cachedTint = 16777215;if(value){if(value.baseTexture.hasLoaded){this._onTextureUpdate();}else {value.once("update", this._onTextureUpdate, this);}}}}});Sprite.prototype._onTextureUpdate = function(){if(this._width){this.scale.x = utils.sign(this.scale.x) * this._width / this.texture.frame.width;}if(this._height){this.scale.y = utils.sign(this.scale.y) * this._height / this.texture.frame.height;}};Sprite.prototype._renderWebGL = function(renderer){renderer.setObjectRenderer(renderer.plugins.sprite);renderer.plugins.sprite.render(this);};Sprite.prototype.getBounds = function(matrix){if(!this._currentBounds){var width=this._texture._frame.width;var height=this._texture._frame.height;var w0=width * (1 - this.anchor.x);var w1=width * -this.anchor.x;var h0=height * (1 - this.anchor.y);var h1=height * -this.anchor.y;var worldTransform=matrix || this.worldTransform;var a=worldTransform.a;var b=worldTransform.b;var c=worldTransform.c;var d=worldTransform.d;var tx=worldTransform.tx;var ty=worldTransform.ty;var minX, maxX, minY, maxY;var x1=a * w1 + c * h1 + tx;var y1=d * h1 + b * w1 + ty;var x2=a * w0 + c * h1 + tx;var y2=d * h1 + b * w0 + ty;var x3=a * w0 + c * h0 + tx;var y3=d * h0 + b * w0 + ty;var x4=a * w1 + c * h0 + tx;var y4=d * h0 + b * w1 + ty;minX = x1;minX = x2 < minX?x2:minX;minX = x3 < minX?x3:minX;minX = x4 < minX?x4:minX;minY = y1;minY = y2 < minY?y2:minY;minY = y3 < minY?y3:minY;minY = y4 < minY?y4:minY;maxX = x1;maxX = x2 > maxX?x2:maxX;maxX = x3 > maxX?x3:maxX;maxX = x4 > maxX?x4:maxX;maxY = y1;maxY = y2 > maxY?y2:maxY;maxY = y3 > maxY?y3:maxY;maxY = y4 > maxY?y4:maxY;if(this.children.length){var childBounds=this.containerGetBounds();w0 = childBounds.x;w1 = childBounds.x + childBounds.width;h0 = childBounds.y;h1 = childBounds.y + childBounds.height;minX = minX < w0?minX:w0;minY = minY < h0?minY:h0;maxX = maxX > w1?maxX:w1;maxY = maxY > h1?maxY:h1;}var bounds=this._bounds;bounds.x = minX;bounds.width = maxX - minX;bounds.y = minY;bounds.height = maxY - minY;this._currentBounds = bounds;}return this._currentBounds;};Sprite.prototype.getLocalBounds = function(){this._bounds.x = -this._texture._frame.width * this.anchor.x;this._bounds.y = -this._texture._frame.height * this.anchor.y;this._bounds.width = this._texture._frame.width;this._bounds.height = this._texture._frame.height;return this._bounds;};Sprite.prototype.containsPoint = function(point){this.worldTransform.applyInverse(point, tempPoint);var width=this._texture._frame.width;var height=this._texture._frame.height;var x1=-width * this.anchor.x;var y1;if(tempPoint.x > x1 && tempPoint.x < x1 + width){y1 = -height * this.anchor.y;if(tempPoint.y > y1 && tempPoint.y < y1 + height){return true;}}return false;};Sprite.prototype._renderCanvas = function(renderer){if(this.texture.crop.width <= 0 || this.texture.crop.height <= 0){return;}var compositeOperation=renderer.blendModes[this.blendMode];if(compositeOperation !== renderer.context.globalCompositeOperation){renderer.context.globalCompositeOperation = compositeOperation;}if(this.texture.valid){var texture=this._texture, wt=this.worldTransform, dx, dy, width=texture.crop.width, height=texture.crop.height;renderer.context.globalAlpha = this.worldAlpha;var smoothingEnabled=texture.baseTexture.scaleMode === CONST.SCALE_MODES.LINEAR;if(renderer.smoothProperty && renderer.context[renderer.smoothProperty] !== smoothingEnabled){renderer.context[renderer.smoothProperty] = smoothingEnabled;}if((texture.rotate & 3) === 2){width = texture.crop.height;height = texture.crop.width;}if(texture.trim){dx = texture.crop.width / 2 + texture.trim.x - this.anchor.x * texture.trim.width;dy = texture.crop.height / 2 + texture.trim.y - this.anchor.y * texture.trim.height;}else {dx = (0.5 - this.anchor.x) * texture._frame.width;dy = (0.5 - this.anchor.y) * texture._frame.height;}if(texture.rotate){wt.copy(canvasRenderWorldTransform);wt = canvasRenderWorldTransform;GroupD8.matrixAppendRotationInv(wt, texture.rotate, dx, dy);dx = 0;dy = 0;}dx -= width / 2;dy -= height / 2;if(renderer.roundPixels){renderer.context.setTransform(wt.a, wt.b, wt.c, wt.d, wt.tx * renderer.resolution | 0, wt.ty * renderer.resolution | 0);dx = dx | 0;dy = dy | 0;}else {renderer.context.setTransform(wt.a, wt.b, wt.c, wt.d, wt.tx * renderer.resolution, wt.ty * renderer.resolution);}var resolution=texture.baseTexture.resolution;if(this.tint !== 16777215){if(this.cachedTint !== this.tint){this.cachedTint = this.tint;this.tintedTexture = CanvasTinter.getTintedTexture(this, this.tint);}renderer.context.drawImage(this.tintedTexture, 0, 0, width * resolution, height * resolution, dx * renderer.resolution, dy * renderer.resolution, width * renderer.resolution, height * renderer.resolution);}else {renderer.context.drawImage(texture.baseTexture.source, texture.crop.x * resolution, texture.crop.y * resolution, width * resolution, height * resolution, dx * renderer.resolution, dy * renderer.resolution, width * renderer.resolution, height * renderer.resolution);}}};Sprite.prototype.destroy = function(destroyTexture, destroyBaseTexture){Container.prototype.destroy.call(this);this.anchor = null;if(destroyTexture){this._texture.destroy(destroyBaseTexture);}this._texture = null;this.shader = null;};Sprite.fromFrame = function(frameId){var texture=utils.TextureCache[frameId];if(!texture){throw new Error("The frameId \"" + frameId + "\" does not exist in the texture cache");}return new Sprite(texture);};Sprite.fromImage = function(imageId, crossorigin, scaleMode){return new Sprite(Texture.fromImage(imageId, crossorigin, scaleMode));};}, {"../const":22, "../display/Container":23, "../math":33, "../renderers/canvas/utils/CanvasTinter":48, "../textures/Texture":72, "../utils":77}], 68:[function(require, module, exports){var ObjectRenderer=require("../../renderers/webgl/utils/ObjectRenderer"), WebGLRenderer=require("../../renderers/webgl/WebGLRenderer"), CONST=require("../../const");function SpriteRenderer(renderer){ObjectRenderer.call(this, renderer);this.vertSize = 5;this.vertByteSize = this.vertSize * 4;this.size = CONST.SPRITE_BATCH_SIZE;var numVerts=this.size * 4 * this.vertByteSize;var numIndices=this.size * 6;this.vertices = new ArrayBuffer(numVerts);this.positions = new Float32Array(this.vertices);this.colors = new Uint32Array(this.vertices);this.indices = new Uint16Array(numIndices);for(var i=0, j=0; i < numIndices; i += 6, j += 4) {this.indices[i + 0] = j + 0;this.indices[i + 1] = j + 1;this.indices[i + 2] = j + 2;this.indices[i + 3] = j + 0;this.indices[i + 4] = j + 2;this.indices[i + 5] = j + 3;}this.currentBatchSize = 0;this.sprites = [];this.shader = null;}SpriteRenderer.prototype = Object.create(ObjectRenderer.prototype);SpriteRenderer.prototype.constructor = SpriteRenderer;module.exports = SpriteRenderer;WebGLRenderer.registerPlugin("sprite", SpriteRenderer);SpriteRenderer.prototype.onContextChange = function(){var gl=this.renderer.gl;this.shader = this.renderer.shaderManager.defaultShader;this.vertexBuffer = gl.createBuffer();this.indexBuffer = gl.createBuffer();gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.indexBuffer);gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, this.indices, gl.STATIC_DRAW);gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);gl.bufferData(gl.ARRAY_BUFFER, this.vertices, gl.DYNAMIC_DRAW);this.currentBlendMode = 99999;};SpriteRenderer.prototype.render = function(sprite){var texture=sprite._texture;if(this.currentBatchSize >= this.size){this.flush();}var uvs=texture._uvs;if(!uvs){return;}var aX=sprite.anchor.x;var aY=sprite.anchor.y;var w0, w1, h0, h1;if(texture.trim && sprite.tileScale === undefined){var trim=texture.trim;w1 = trim.x - aX * trim.width;w0 = w1 + texture.crop.width;h1 = trim.y - aY * trim.height;h0 = h1 + texture.crop.height;}else {w0 = texture._frame.width * (1 - aX);w1 = texture._frame.width * -aX;h0 = texture._frame.height * (1 - aY);h1 = texture._frame.height * -aY;}var index=this.currentBatchSize * this.vertByteSize;var worldTransform=sprite.worldTransform;var a=worldTransform.a;var b=worldTransform.b;var c=worldTransform.c;var d=worldTransform.d;var tx=worldTransform.tx;var ty=worldTransform.ty;var colors=this.colors;var positions=this.positions;if(this.renderer.roundPixels){var resolution=this.renderer.resolution;positions[index] = ((a * w1 + c * h1 + tx) * resolution | 0) / resolution;positions[index + 1] = ((d * h1 + b * w1 + ty) * resolution | 0) / resolution;positions[index + 5] = ((a * w0 + c * h1 + tx) * resolution | 0) / resolution;positions[index + 6] = ((d * h1 + b * w0 + ty) * resolution | 0) / resolution;positions[index + 10] = ((a * w0 + c * h0 + tx) * resolution | 0) / resolution;positions[index + 11] = ((d * h0 + b * w0 + ty) * resolution | 0) / resolution;positions[index + 15] = ((a * w1 + c * h0 + tx) * resolution | 0) / resolution;positions[index + 16] = ((d * h0 + b * w1 + ty) * resolution | 0) / resolution;}else {positions[index] = a * w1 + c * h1 + tx;positions[index + 1] = d * h1 + b * w1 + ty;positions[index + 5] = a * w0 + c * h1 + tx;positions[index + 6] = d * h1 + b * w0 + ty;positions[index + 10] = a * w0 + c * h0 + tx;positions[index + 11] = d * h0 + b * w0 + ty;positions[index + 15] = a * w1 + c * h0 + tx;positions[index + 16] = d * h0 + b * w1 + ty;}positions[index + 2] = uvs.x0;positions[index + 3] = uvs.y0;positions[index + 7] = uvs.x1;positions[index + 8] = uvs.y1;positions[index + 12] = uvs.x2;positions[index + 13] = uvs.y2;positions[index + 17] = uvs.x3;positions[index + 18] = uvs.y3;var tint=sprite.tint;colors[index + 4] = colors[index + 9] = colors[index + 14] = colors[index + 19] = (tint >> 16) + (tint & 65280) + ((tint & 255) << 16) + (sprite.worldAlpha * 255 << 24);this.sprites[this.currentBatchSize++] = sprite;};SpriteRenderer.prototype.flush = function(){if(this.currentBatchSize === 0){return;}var gl=this.renderer.gl;var shader;if(this.currentBatchSize > this.size * 0.5){gl.bufferSubData(gl.ARRAY_BUFFER, 0, this.vertices);}else {var view=this.positions.subarray(0, this.currentBatchSize * this.vertByteSize);gl.bufferSubData(gl.ARRAY_BUFFER, 0, view);}var nextTexture, nextBlendMode, nextShader;var batchSize=0;var start=0;var currentBaseTexture=null;var currentBlendMode=this.renderer.blendModeManager.currentBlendMode;var currentShader=null;var blendSwap=false;var shaderSwap=false;var sprite;for(var i=0, j=this.currentBatchSize; i < j; i++) {sprite = this.sprites[i];nextTexture = sprite._texture.baseTexture;nextBlendMode = sprite.blendMode;nextShader = sprite.shader || this.shader;blendSwap = currentBlendMode !== nextBlendMode;shaderSwap = currentShader !== nextShader;if(currentBaseTexture !== nextTexture || blendSwap || shaderSwap){this.renderBatch(currentBaseTexture, batchSize, start);start = i;batchSize = 0;currentBaseTexture = nextTexture;if(blendSwap){currentBlendMode = nextBlendMode;this.renderer.blendModeManager.setBlendMode(currentBlendMode);}if(shaderSwap){currentShader = nextShader;shader = currentShader.shaders?currentShader.shaders[gl.id]:currentShader;if(!shader){shader = currentShader.getShader(this.renderer);}this.renderer.shaderManager.setShader(shader);shader.uniforms.projectionMatrix.value = this.renderer.currentRenderTarget.projectionMatrix.toArray(true);shader.syncUniforms();gl.activeTexture(gl.TEXTURE0);}}batchSize++;}this.renderBatch(currentBaseTexture, batchSize, start);this.currentBatchSize = 0;};SpriteRenderer.prototype.renderBatch = function(texture, size, startIndex){if(size === 0){return;}var gl=this.renderer.gl;if(!texture._glTextures[gl.id]){this.renderer.updateTexture(texture);}else {gl.bindTexture(gl.TEXTURE_2D, texture._glTextures[gl.id]);}gl.drawElements(gl.TRIANGLES, size * 6, gl.UNSIGNED_SHORT, startIndex * 6 * 2);this.renderer.drawCount++;};SpriteRenderer.prototype.start = function(){var gl=this.renderer.gl;gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.indexBuffer);var stride=this.vertByteSize;gl.vertexAttribPointer(this.shader.attributes.aVertexPosition, 2, gl.FLOAT, false, stride, 0);gl.vertexAttribPointer(this.shader.attributes.aTextureCoord, 2, gl.FLOAT, false, stride, 2 * 4);gl.vertexAttribPointer(this.shader.attributes.aColor, 4, gl.UNSIGNED_BYTE, true, stride, 4 * 4);};SpriteRenderer.prototype.destroy = function(){this.renderer.gl.deleteBuffer(this.vertexBuffer);this.renderer.gl.deleteBuffer(this.indexBuffer);ObjectRenderer.prototype.destroy.call(this);this.shader.destroy();this.renderer = null;this.vertices = null;this.positions = null;this.colors = null;this.indices = null;this.vertexBuffer = null;this.indexBuffer = null;this.sprites = null;this.shader = null;};}, {"../../const":22, "../../renderers/webgl/WebGLRenderer":49, "../../renderers/webgl/utils/ObjectRenderer":63}], 69:[function(require, module, exports){var Sprite=require("../sprites/Sprite"), Texture=require("../textures/Texture"), math=require("../math"), utils=require("../utils"), CONST=require("../const");function Text(text, style, resolution){this.canvas = document.createElement("canvas");this.context = this.canvas.getContext("2d");this.resolution = resolution || CONST.RESOLUTION;this._text = null;this._style = null;var texture=Texture.fromCanvas(this.canvas);texture.trim = new math.Rectangle();Sprite.call(this, texture);this.text = text;this.style = style;}Text.prototype = Object.create(Sprite.prototype);Text.prototype.constructor = Text;module.exports = Text;Text.fontPropertiesCache = {};Text.fontPropertiesCanvas = document.createElement("canvas");Text.fontPropertiesContext = Text.fontPropertiesCanvas.getContext("2d");Object.defineProperties(Text.prototype, {width:{get:function get(){if(this.dirty){this.updateText();}return this.scale.x * this._texture._frame.width;}, set:function set(value){this.scale.x = value / this._texture._frame.width;this._width = value;}}, height:{get:function get(){if(this.dirty){this.updateText();}return this.scale.y * this._texture._frame.height;}, set:function set(value){this.scale.y = value / this._texture._frame.height;this._height = value;}}, style:{get:function get(){return this._style;}, set:function set(style){style = style || {};if(typeof style.fill === "number"){style.fill = utils.hex2string(style.fill);}if(typeof style.stroke === "number"){style.stroke = utils.hex2string(style.stroke);}if(typeof style.dropShadowColor === "number"){style.dropShadowColor = utils.hex2string(style.dropShadowColor);}style.font = style.font || "bold 20pt Arial";style.fill = style.fill || "black";style.align = style.align || "left";style.stroke = style.stroke || "black";style.strokeThickness = style.strokeThickness || 0;style.wordWrap = style.wordWrap || false;style.wordWrapWidth = style.wordWrapWidth || 100;style.breakWords = style.breakWords || false;style.letterSpacing = style.letterSpacing || 0;style.dropShadow = style.dropShadow || false;style.dropShadowColor = style.dropShadowColor || "#000000";style.dropShadowAngle = style.dropShadowAngle !== undefined?style.dropShadowAngle:Math.PI / 6;style.dropShadowDistance = style.dropShadowDistance !== undefined?style.dropShadowDistance:5;style.dropShadowBlur = style.dropShadowBlur !== undefined?style.dropShadowBlur:0;style.padding = style.padding || 0;style.textBaseline = style.textBaseline || "alphabetic";style.lineJoin = style.lineJoin || "miter";style.miterLimit = style.miterLimit || 10;this._style = style;this.dirty = true;}}, text:{get:function get(){return this._text;}, set:function set(text){text = text.toString() || " ";if(this._text === text){return;}this._text = text;this.dirty = true;}}});Text.prototype.updateText = function(){var style=this._style;this.context.font = style.font;var outputText=style.wordWrap?this.wordWrap(this._text):this._text;var lines=outputText.split(/(?:\r\n|\r|\n)/);var lineWidths=new Array(lines.length);var maxLineWidth=0;var fontProperties=this.determineFontProperties(style.font);for(var i=0; i < lines.length; i++) {var lineWidth=this.context.measureText(lines[i]).width + (lines[i].length - 1) * style.letterSpacing;lineWidths[i] = lineWidth;maxLineWidth = Math.max(maxLineWidth, lineWidth);}var width=maxLineWidth + style.strokeThickness;if(style.dropShadow){width += style.dropShadowDistance;}this.canvas.width = Math.ceil((width + this.context.lineWidth) * this.resolution);var lineHeight=this.style.lineHeight || fontProperties.fontSize + style.strokeThickness;var height=lineHeight * lines.length;if(style.dropShadow){height += style.dropShadowDistance;}this.canvas.height = Math.ceil((height + this._style.padding * 2) * this.resolution);this.context.scale(this.resolution, this.resolution);if(navigator.isCocoonJS){this.context.clearRect(0, 0, this.canvas.width, this.canvas.height);}this.context.font = style.font;this.context.strokeStyle = style.stroke;this.context.lineWidth = style.strokeThickness;this.context.textBaseline = style.textBaseline;this.context.lineJoin = style.lineJoin;this.context.miterLimit = style.miterLimit;var linePositionX;var linePositionY;if(style.dropShadow){if(style.dropShadowBlur > 0){this.context.shadowColor = style.dropShadowColor;this.context.shadowBlur = style.dropShadowBlur;}else {this.context.fillStyle = style.dropShadowColor;}var xShadowOffset=Math.cos(style.dropShadowAngle) * style.dropShadowDistance;var yShadowOffset=Math.sin(style.dropShadowAngle) * style.dropShadowDistance;for(i = 0; i < lines.length; i++) {linePositionX = style.strokeThickness / 2;linePositionY = style.strokeThickness / 2 + i * lineHeight + fontProperties.ascent;if(style.align === "right"){linePositionX += maxLineWidth - lineWidths[i];}else if(style.align === "center"){linePositionX += (maxLineWidth - lineWidths[i]) / 2;}if(style.fill){this.drawLetterSpacing(lines[i], linePositionX + xShadowOffset, linePositionY + yShadowOffset + style.padding);}}}this.context.fillStyle = style.fill;for(i = 0; i < lines.length; i++) {linePositionX = style.strokeThickness / 2;linePositionY = style.strokeThickness / 2 + i * lineHeight + fontProperties.ascent;if(style.align === "right"){linePositionX += maxLineWidth - lineWidths[i];}else if(style.align === "center"){linePositionX += (maxLineWidth - lineWidths[i]) / 2;}if(style.stroke && style.strokeThickness){this.drawLetterSpacing(lines[i], linePositionX, linePositionY + style.padding, true);}if(style.fill){this.drawLetterSpacing(lines[i], linePositionX, linePositionY + style.padding);}}this.updateTexture();};Text.prototype.drawLetterSpacing = function(text, x, y, isStroke){var style=this._style;var letterSpacing=style.letterSpacing;if(letterSpacing === 0){if(isStroke){this.context.strokeText(text, x, y);}else {this.context.fillText(text, x, y);}return;}var characters=String.prototype.split.call(text, ""), index=0, current, currentPosition=x;while(index < text.length) {current = characters[index++];if(isStroke){this.context.strokeText(current, currentPosition, y);}else {this.context.fillText(current, currentPosition, y);}currentPosition += this.context.measureText(current).width + letterSpacing;}};Text.prototype.updateTexture = function(){var texture=this._texture;var style=this._style;texture.baseTexture.hasLoaded = true;texture.baseTexture.resolution = this.resolution;texture.baseTexture.width = this.canvas.width / this.resolution;texture.baseTexture.height = this.canvas.height / this.resolution;texture.crop.width = texture._frame.width = this.canvas.width / this.resolution;texture.crop.height = texture._frame.height = this.canvas.height / this.resolution;texture.trim.x = 0;texture.trim.y = -style.padding;texture.trim.width = texture._frame.width;texture.trim.height = texture._frame.height - style.padding * 2;this._width = this.canvas.width / this.resolution;this._height = this.canvas.height / this.resolution;texture.baseTexture.emit("update", texture.baseTexture);this.dirty = false;};Text.prototype.renderWebGL = function(renderer){if(this.dirty){this.updateText();}Sprite.prototype.renderWebGL.call(this, renderer);};Text.prototype._renderCanvas = function(renderer){if(this.dirty){this.updateText();}Sprite.prototype._renderCanvas.call(this, renderer);};Text.prototype.determineFontProperties = function(fontStyle){var properties=Text.fontPropertiesCache[fontStyle];if(!properties){properties = {};var canvas=Text.fontPropertiesCanvas;var context=Text.fontPropertiesContext;context.font = fontStyle;var width=Math.ceil(context.measureText("|MÉq").width);var baseline=Math.ceil(context.measureText("M").width);var height=2 * baseline;baseline = baseline * 1.4 | 0;canvas.width = width;canvas.height = height;context.fillStyle = "#f00";context.fillRect(0, 0, width, height);context.font = fontStyle;context.textBaseline = "alphabetic";context.fillStyle = "#000";context.fillText("|MÉq", 0, baseline);var imagedata=context.getImageData(0, 0, width, height).data;var pixels=imagedata.length;var line=width * 4;var i, j;var idx=0;var stop=false;for(i = 0; i < baseline; i++) {for(j = 0; j < line; j += 4) {if(imagedata[idx + j] !== 255){stop = true;break;}}if(!stop){idx += line;}else {break;}}properties.ascent = baseline - i;idx = pixels - line;stop = false;for(i = height; i > baseline; i--) {for(j = 0; j < line; j += 4) {if(imagedata[idx + j] !== 255){stop = true;break;}}if(!stop){idx -= line;}else {break;}}properties.descent = i - baseline;properties.fontSize = properties.ascent + properties.descent;Text.fontPropertiesCache[fontStyle] = properties;}return properties;};Text.prototype.wordWrap = function(text){var result="";var lines=text.split("\n");var wordWrapWidth=this._style.wordWrapWidth;for(var i=0; i < lines.length; i++) {var spaceLeft=wordWrapWidth;var words=lines[i].split(" ");for(var j=0; j < words.length; j++) {var wordWidth=this.context.measureText(words[j]).width;if(this._style.breakWords && wordWidth > wordWrapWidth){var characters=words[j].split("");for(var c=0; c < characters.length; c++) {var characterWidth=this.context.measureText(characters[c]).width;if(characterWidth > spaceLeft){result += "\n" + characters[c];spaceLeft = wordWrapWidth - characterWidth;}else {if(c === 0){result += " ";}result += characters[c];spaceLeft -= characterWidth;}}}else {var wordWidthWithSpace=wordWidth + this.context.measureText(" ").width;if(j === 0 || wordWidthWithSpace > spaceLeft){if(j > 0){result += "\n";}result += words[j];spaceLeft = wordWrapWidth - wordWidth;}else {spaceLeft -= wordWidthWithSpace;result += " " + words[j];}}}if(i < lines.length - 1){result += "\n";}}return result;};Text.prototype.getBounds = function(matrix){if(this.dirty){this.updateText();}return Sprite.prototype.getBounds.call(this, matrix);};Text.prototype.destroy = function(destroyBaseTexture){this.context = null;this.canvas = null;this._style = null;this._texture.destroy(destroyBaseTexture === undefined?true:destroyBaseTexture);};}, {"../const":22, "../math":33, "../sprites/Sprite":67, "../textures/Texture":72, "../utils":77}], 70:[function(require, module, exports){var utils=require("../utils"), CONST=require("../const"), EventEmitter=require("eventemitter3");function BaseTexture(source, scaleMode, resolution){EventEmitter.call(this);this.uid = utils.uid();this.resolution = resolution || 1;this.width = 100;this.height = 100;this.realWidth = 100;this.realHeight = 100;this.scaleMode = scaleMode || CONST.SCALE_MODES.DEFAULT;this.hasLoaded = false;this.isLoading = false;this.source = null;this.premultipliedAlpha = true;this.imageUrl = null;this.isPowerOfTwo = false;this.mipmap = false;this._glTextures = {};if(source){this.loadSource(source);}}BaseTexture.prototype = Object.create(EventEmitter.prototype);BaseTexture.prototype.constructor = BaseTexture;module.exports = BaseTexture;BaseTexture.prototype.update = function(){this.realWidth = this.source.naturalWidth || this.source.width;this.realHeight = this.source.naturalHeight || this.source.height;this.width = this.realWidth / this.resolution;this.height = this.realHeight / this.resolution;this.isPowerOfTwo = utils.isPowerOfTwo(this.realWidth, this.realHeight);this.emit("update", this);};BaseTexture.prototype.loadSource = function(source){var wasLoading=this.isLoading;this.hasLoaded = false;this.isLoading = false;if(wasLoading && this.source){this.source.onload = null;this.source.onerror = null;}this.source = source;if((this.source.complete || this.source.getContext) && this.source.width && this.source.height){this._sourceLoaded();}else if(!source.getContext){this.isLoading = true;var scope=this;source.onload = function(){source.onload = null;source.onerror = null;if(!scope.isLoading){return;}scope.isLoading = false;scope._sourceLoaded();scope.emit("loaded", scope);};source.onerror = function(){source.onload = null;source.onerror = null;if(!scope.isLoading){return;}scope.isLoading = false;scope.emit("error", scope);};if(source.complete && source.src){this.isLoading = false;source.onload = null;source.onerror = null;if(source.width && source.height){this._sourceLoaded();if(wasLoading){this.emit("loaded", this);}}else {if(wasLoading){this.emit("error", this);}}}}};BaseTexture.prototype._sourceLoaded = function(){this.hasLoaded = true;this.update();};BaseTexture.prototype.destroy = function(){if(this.imageUrl){delete utils.BaseTextureCache[this.imageUrl];delete utils.TextureCache[this.imageUrl];this.imageUrl = null;if(!navigator.isCocoonJS){this.source.src = "";}}else if(this.source && this.source._pixiId){delete utils.BaseTextureCache[this.source._pixiId];}this.source = null;this.dispose();};BaseTexture.prototype.dispose = function(){this.emit("dispose", this);};BaseTexture.prototype.updateSourceImage = function(newSrc){this.source.src = newSrc;this.loadSource(this.source);};BaseTexture.fromImage = function(imageUrl, crossorigin, scaleMode){var baseTexture=utils.BaseTextureCache[imageUrl];if(crossorigin === undefined && imageUrl.indexOf("data:") !== 0){crossorigin = true;}if(!baseTexture){var image=new Image();if(crossorigin){image.crossOrigin = "";}baseTexture = new BaseTexture(image, scaleMode);baseTexture.imageUrl = imageUrl;image.src = imageUrl;utils.BaseTextureCache[imageUrl] = baseTexture;baseTexture.resolution = utils.getResolutionOfUrl(imageUrl);}return baseTexture;};BaseTexture.fromCanvas = function(canvas, scaleMode){if(!canvas._pixiId){canvas._pixiId = "canvas_" + utils.uid();}var baseTexture=utils.BaseTextureCache[canvas._pixiId];if(!baseTexture){baseTexture = new BaseTexture(canvas, scaleMode);utils.BaseTextureCache[canvas._pixiId] = baseTexture;}return baseTexture;};}, {"../const":22, "../utils":77, eventemitter3:10}], 71:[function(require, module, exports){var BaseTexture=require("./BaseTexture"), Texture=require("./Texture"), RenderTarget=require("../renderers/webgl/utils/RenderTarget"), FilterManager=require("../renderers/webgl/managers/FilterManager"), CanvasBuffer=require("../renderers/canvas/utils/CanvasBuffer"), math=require("../math"), CONST=require("../const"), tempMatrix=new math.Matrix();function RenderTexture(renderer, width, height, scaleMode, resolution){if(!renderer){throw new Error("Unable to create RenderTexture, you must pass a renderer into the constructor.");}width = width || 100;height = height || 100;resolution = resolution || CONST.RESOLUTION;var baseTexture=new BaseTexture();baseTexture.width = width;baseTexture.height = height;baseTexture.resolution = resolution;baseTexture.scaleMode = scaleMode || CONST.SCALE_MODES.DEFAULT;baseTexture.hasLoaded = true;Texture.call(this, baseTexture, new math.Rectangle(0, 0, width, height));this.width = width;this.height = height;this.resolution = resolution;this.render = null;this.renderer = renderer;if(this.renderer.type === CONST.RENDERER_TYPE.WEBGL){var gl=this.renderer.gl;this.textureBuffer = new RenderTarget(gl, this.width, this.height, baseTexture.scaleMode, this.resolution);this.baseTexture._glTextures[gl.id] = this.textureBuffer.texture;this.filterManager = new FilterManager(this.renderer);this.filterManager.onContextChange();this.filterManager.resize(width, height);this.render = this.renderWebGL;this.renderer.currentRenderer.start();this.renderer.currentRenderTarget.activate();}else {this.render = this.renderCanvas;this.textureBuffer = new CanvasBuffer(this.width * this.resolution, this.height * this.resolution);this.baseTexture.source = this.textureBuffer.canvas;}this.valid = true;this._updateUvs();}RenderTexture.prototype = Object.create(Texture.prototype);RenderTexture.prototype.constructor = RenderTexture;module.exports = RenderTexture;RenderTexture.prototype.resize = function(width, height, updateBase){if(width === this.width && height === this.height){return;}this.valid = width > 0 && height > 0;this.width = this._frame.width = this.crop.width = width;this.height = this._frame.height = this.crop.height = height;if(updateBase){this.baseTexture.width = this.width;this.baseTexture.height = this.height;}if(!this.valid){return;}this.textureBuffer.resize(this.width, this.height);if(this.filterManager){this.filterManager.resize(this.width, this.height);}};RenderTexture.prototype.clear = function(){if(!this.valid){return;}if(this.renderer.type === CONST.RENDERER_TYPE.WEBGL){this.renderer.gl.bindFramebuffer(this.renderer.gl.FRAMEBUFFER, this.textureBuffer.frameBuffer);}this.textureBuffer.clear();};RenderTexture.prototype.renderWebGL = function(displayObject, matrix, clear, updateTransform){if(!this.valid){return;}updateTransform = updateTransform !== undefined?updateTransform:true;this.textureBuffer.transform = matrix;this.textureBuffer.activate();displayObject.worldAlpha = 1;if(updateTransform){displayObject.worldTransform.identity();displayObject.currentBounds = null;var children=displayObject.children;var i, j;for(i = 0, j = children.length; i < j; ++i) {children[i].updateTransform();}}var temp=this.renderer.filterManager;this.renderer.filterManager = this.filterManager;this.renderer.renderDisplayObject(displayObject, this.textureBuffer, clear);this.renderer.filterManager = temp;};RenderTexture.prototype.renderCanvas = function(displayObject, matrix, clear, updateTransform){if(!this.valid){return;}updateTransform = !!updateTransform;var wt=tempMatrix;wt.identity();if(matrix){wt.append(matrix);}var cachedWt=displayObject.worldTransform;displayObject.worldTransform = wt;displayObject.worldAlpha = 1;var children=displayObject.children;var i, j;for(i = 0, j = children.length; i < j; ++i) {children[i].updateTransform();}if(clear){this.textureBuffer.clear();}var context=this.textureBuffer.context;var realResolution=this.renderer.resolution;this.renderer.resolution = this.resolution;this.renderer.renderDisplayObject(displayObject, context);this.renderer.resolution = realResolution;if(displayObject.worldTransform === wt){displayObject.worldTransform = cachedWt;}};RenderTexture.prototype.destroy = function(){Texture.prototype.destroy.call(this, true);this.textureBuffer.destroy();if(this.filterManager){this.filterManager.destroy();}this.renderer = null;};RenderTexture.prototype.getImage = function(){var image=new Image();image.src = this.getBase64();return image;};RenderTexture.prototype.getBase64 = function(){return this.getCanvas().toDataURL();};RenderTexture.prototype.getCanvas = function(){if(this.renderer.type === CONST.RENDERER_TYPE.WEBGL){var gl=this.renderer.gl;var width=this.textureBuffer.size.width;var height=this.textureBuffer.size.height;var webGLPixels=new Uint8Array(4 * width * height);gl.bindFramebuffer(gl.FRAMEBUFFER, this.textureBuffer.frameBuffer);gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, webGLPixels);gl.bindFramebuffer(gl.FRAMEBUFFER, null);var tempCanvas=new CanvasBuffer(width, height);var canvasData=tempCanvas.context.getImageData(0, 0, width, height);canvasData.data.set(webGLPixels);tempCanvas.context.putImageData(canvasData, 0, 0);return tempCanvas.canvas;}else {return this.textureBuffer.canvas;}};RenderTexture.prototype.getPixels = function(){var width, height;if(this.renderer.type === CONST.RENDERER_TYPE.WEBGL){var gl=this.renderer.gl;width = this.textureBuffer.size.width;height = this.textureBuffer.size.height;var webGLPixels=new Uint8Array(4 * width * height);gl.bindFramebuffer(gl.FRAMEBUFFER, this.textureBuffer.frameBuffer);gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, webGLPixels);gl.bindFramebuffer(gl.FRAMEBUFFER, null);return webGLPixels;}else {width = this.textureBuffer.canvas.width;height = this.textureBuffer.canvas.height;return this.textureBuffer.canvas.getContext("2d").getImageData(0, 0, width, height).data;}};RenderTexture.prototype.getPixel = function(x, y){if(this.renderer.type === CONST.RENDERER_TYPE.WEBGL){var gl=this.renderer.gl;var webGLPixels=new Uint8Array(4);gl.bindFramebuffer(gl.FRAMEBUFFER, this.textureBuffer.frameBuffer);gl.readPixels(x, y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, webGLPixels);gl.bindFramebuffer(gl.FRAMEBUFFER, null);return webGLPixels;}else {return this.textureBuffer.canvas.getContext("2d").getImageData(x, y, 1, 1).data;}};}, {"../const":22, "../math":33, "../renderers/canvas/utils/CanvasBuffer":45, "../renderers/webgl/managers/FilterManager":54, "../renderers/webgl/utils/RenderTarget":65, "./BaseTexture":70, "./Texture":72}], 72:[function(require, module, exports){var BaseTexture=require("./BaseTexture"), VideoBaseTexture=require("./VideoBaseTexture"), TextureUvs=require("./TextureUvs"), EventEmitter=require("eventemitter3"), math=require("../math"), utils=require("../utils");function Texture(baseTexture, frame, crop, trim, rotate){EventEmitter.call(this);this.noFrame = false;if(!frame){this.noFrame = true;frame = new math.Rectangle(0, 0, 1, 1);}if(baseTexture instanceof Texture){baseTexture = baseTexture.baseTexture;}this.baseTexture = baseTexture;this._frame = frame;this.trim = trim;this.valid = false;this.requiresUpdate = false;this._uvs = null;this.width = 0;this.height = 0;this.crop = crop || frame;this._rotate = +(rotate || 0);if(rotate === true){this._rotate = 2;}else {if(this._rotate % 2 !== 0){throw "attempt to use diamond-shaped UVs. If you are sure, set rotation manually";}}if(baseTexture.hasLoaded){if(this.noFrame){frame = new math.Rectangle(0, 0, baseTexture.width, baseTexture.height);baseTexture.on("update", this.onBaseTextureUpdated, this);}this.frame = frame;}else {baseTexture.once("loaded", this.onBaseTextureLoaded, this);}}Texture.prototype = Object.create(EventEmitter.prototype);Texture.prototype.constructor = Texture;module.exports = Texture;Object.defineProperties(Texture.prototype, {frame:{get:function get(){return this._frame;}, set:function set(frame){this._frame = frame;this.noFrame = false;this.width = frame.width;this.height = frame.height;if(!this.trim && !this.rotate && (frame.x + frame.width > this.baseTexture.width || frame.y + frame.height > this.baseTexture.height)){throw new Error("Texture Error: frame does not fit inside the base Texture dimensions " + this);}this.valid = frame && frame.width && frame.height && this.baseTexture.hasLoaded;if(this.trim){this.width = this.trim.width;this.height = this.trim.height;this._frame.width = this.trim.width;this._frame.height = this.trim.height;}else {this.crop = frame;}if(this.valid){this._updateUvs();}}}, rotate:{get:function get(){return this._rotate;}, set:function set(rotate){this._rotate = rotate;if(this.valid){this._updateUvs();}}}});Texture.prototype.update = function(){this.baseTexture.update();};Texture.prototype.onBaseTextureLoaded = function(baseTexture){if(this.noFrame){this.frame = new math.Rectangle(0, 0, baseTexture.width, baseTexture.height);}else {this.frame = this._frame;}this.emit("update", this);};Texture.prototype.onBaseTextureUpdated = function(baseTexture){this._frame.width = baseTexture.width;this._frame.height = baseTexture.height;this.emit("update", this);};Texture.prototype.destroy = function(destroyBase){if(this.baseTexture){if(destroyBase){this.baseTexture.destroy();}this.baseTexture.off("update", this.onBaseTextureUpdated, this);this.baseTexture.off("loaded", this.onBaseTextureLoaded, this);this.baseTexture = null;}this._frame = null;this._uvs = null;this.trim = null;this.crop = null;this.valid = false;this.off("dispose", this.dispose, this);this.off("update", this.update, this);};Texture.prototype.clone = function(){return new Texture(this.baseTexture, this.frame, this.crop, this.trim, this.rotate);};Texture.prototype._updateUvs = function(){if(!this._uvs){this._uvs = new TextureUvs();}this._uvs.set(this.crop, this.baseTexture, this.rotate);};Texture.fromImage = function(imageUrl, crossorigin, scaleMode){var texture=utils.TextureCache[imageUrl];if(!texture){texture = new Texture(BaseTexture.fromImage(imageUrl, crossorigin, scaleMode));utils.TextureCache[imageUrl] = texture;}return texture;};Texture.fromFrame = function(frameId){var texture=utils.TextureCache[frameId];if(!texture){throw new Error("The frameId \"" + frameId + "\" does not exist in the texture cache");}return texture;};Texture.fromCanvas = function(canvas, scaleMode){return new Texture(BaseTexture.fromCanvas(canvas, scaleMode));};Texture.fromVideo = function(video, scaleMode){if(typeof video === "string"){return Texture.fromVideoUrl(video, scaleMode);}else {return new Texture(VideoBaseTexture.fromVideo(video, scaleMode));}};Texture.fromVideoUrl = function(videoUrl, scaleMode){return new Texture(VideoBaseTexture.fromUrl(videoUrl, scaleMode));};Texture.addTextureToCache = function(texture, id){utils.TextureCache[id] = texture;};Texture.removeTextureFromCache = function(id){var texture=utils.TextureCache[id];delete utils.TextureCache[id];delete utils.BaseTextureCache[id];return texture;};Texture.EMPTY = new Texture(new BaseTexture());}, {"../math":33, "../utils":77, "./BaseTexture":70, "./TextureUvs":73, "./VideoBaseTexture":74, eventemitter3:10}], 73:[function(require, module, exports){function TextureUvs(){this.x0 = 0;this.y0 = 0;this.x1 = 1;this.y1 = 0;this.x2 = 1;this.y2 = 1;this.x3 = 0;this.y3 = 1;}module.exports = TextureUvs;var GroupD8=require("../math/GroupD8");TextureUvs.prototype.set = function(frame, baseFrame, rotate){var tw=baseFrame.width;var th=baseFrame.height;if(rotate){var swapWidthHeight=GroupD8.isSwapWidthHeight(rotate);var w2=(swapWidthHeight?frame.height:frame.width) / 2 / tw;var h2=(swapWidthHeight?frame.width:frame.height) / 2 / th;var cX=frame.x / tw + w2;var cY=frame.y / th + h2;rotate = GroupD8.add(rotate, GroupD8.NW);this.x0 = cX + w2 * GroupD8.uX(rotate);this.y0 = cY + h2 * GroupD8.uY(rotate);rotate = GroupD8.add(rotate, 2);this.x1 = cX + w2 * GroupD8.uX(rotate);this.y1 = cY + h2 * GroupD8.uY(rotate);rotate = GroupD8.add(rotate, 2);this.x2 = cX + w2 * GroupD8.uX(rotate);this.y2 = cY + h2 * GroupD8.uY(rotate);rotate = GroupD8.add(rotate, 2);this.x3 = cX + w2 * GroupD8.uX(rotate);this.y3 = cY + h2 * GroupD8.uY(rotate);}else {this.x0 = frame.x / tw;this.y0 = frame.y / th;this.x1 = (frame.x + frame.width) / tw;this.y1 = frame.y / th;this.x2 = (frame.x + frame.width) / tw;this.y2 = (frame.y + frame.height) / th;this.x3 = frame.x / tw;this.y3 = (frame.y + frame.height) / th;}};}, {"../math/GroupD8":30}], 74:[function(require, module, exports){var BaseTexture=require("./BaseTexture"), utils=require("../utils");function VideoBaseTexture(source, scaleMode){if(!source){throw new Error("No video source element specified.");}if((source.readyState === source.HAVE_ENOUGH_DATA || source.readyState === source.HAVE_FUTURE_DATA) && source.width && source.height){source.complete = true;}BaseTexture.call(this, source, scaleMode);this.autoUpdate = false;this._onUpdate = this._onUpdate.bind(this);this._onCanPlay = this._onCanPlay.bind(this);if(!source.complete){source.addEventListener("canplay", this._onCanPlay);source.addEventListener("canplaythrough", this._onCanPlay);source.addEventListener("play", this._onPlayStart.bind(this));source.addEventListener("pause", this._onPlayStop.bind(this));}this.__loaded = false;}VideoBaseTexture.prototype = Object.create(BaseTexture.prototype);VideoBaseTexture.prototype.constructor = VideoBaseTexture;module.exports = VideoBaseTexture;VideoBaseTexture.prototype._onUpdate = function(){if(this.autoUpdate){window.requestAnimationFrame(this._onUpdate);this.update();}};VideoBaseTexture.prototype._onPlayStart = function(){if(!this.autoUpdate){window.requestAnimationFrame(this._onUpdate);this.autoUpdate = true;}};VideoBaseTexture.prototype._onPlayStop = function(){this.autoUpdate = false;};VideoBaseTexture.prototype._onCanPlay = function(){this.hasLoaded = true;if(this.source){this.source.removeEventListener("canplay", this._onCanPlay);this.source.removeEventListener("canplaythrough", this._onCanPlay);this.width = this.source.videoWidth;this.height = this.source.videoHeight;this.source.play();if(!this.__loaded){this.__loaded = true;this.emit("loaded", this);}}};VideoBaseTexture.prototype.destroy = function(){if(this.source && this.source._pixiId){delete utils.BaseTextureCache[this.source._pixiId];delete this.source._pixiId;}BaseTexture.prototype.destroy.call(this);};VideoBaseTexture.fromVideo = function(video, scaleMode){if(!video._pixiId){video._pixiId = "video_" + utils.uid();}var baseTexture=utils.BaseTextureCache[video._pixiId];if(!baseTexture){baseTexture = new VideoBaseTexture(video, scaleMode);utils.BaseTextureCache[video._pixiId] = baseTexture;}return baseTexture;};VideoBaseTexture.fromUrl = function(videoSrc, scaleMode){var video=document.createElement("video");if(Array.isArray(videoSrc)){for(var i=0; i < videoSrc.length; ++i) {video.appendChild(createSource(videoSrc[i].src || videoSrc[i], videoSrc[i].mime));}}else {video.appendChild(createSource(videoSrc.src || videoSrc, videoSrc.mime));}video.load();video.play();return VideoBaseTexture.fromVideo(video, scaleMode);};VideoBaseTexture.fromUrls = VideoBaseTexture.fromUrl;function createSource(path, type){if(!type){type = "video/" + path.substr(path.lastIndexOf(".") + 1);}var source=document.createElement("source");source.src = path;source.type = type;return source;}}, {"../utils":77, "./BaseTexture":70}], 75:[function(require, module, exports){var CONST=require("../const"), EventEmitter=require("eventemitter3"), TICK="tick";function Ticker(){var _this=this;this._tick = function _tick(time){_this._requestId = null;if(_this.started){_this.update(time);if(_this.started && _this._requestId === null && _this._emitter.listeners(TICK, true)){_this._requestId = requestAnimationFrame(_this._tick);}}};this._emitter = new EventEmitter();this._requestId = null;this._maxElapsedMS = 100;this.autoStart = false;this.deltaTime = 1;this.elapsedMS = 1 / CONST.TARGET_FPMS;this.lastTime = 0;this.speed = 1;this.started = false;}Object.defineProperties(Ticker.prototype, {FPS:{get:function get(){return 1000 / this.elapsedMS;}}, minFPS:{get:function get(){return 1000 / this._maxElapsedMS;}, set:function set(fps){var minFPMS=Math.min(Math.max(0, fps) / 1000, CONST.TARGET_FPMS);this._maxElapsedMS = 1 / minFPMS;}}});Ticker.prototype._requestIfNeeded = function _requestIfNeeded(){if(this._requestId === null && this._emitter.listeners(TICK, true)){this.lastTime = performance.now();this._requestId = requestAnimationFrame(this._tick);}};Ticker.prototype._cancelIfNeeded = function _cancelIfNeeded(){if(this._requestId !== null){cancelAnimationFrame(this._requestId);this._requestId = null;}};Ticker.prototype._startIfPossible = function _startIfPossible(){if(this.started){this._requestIfNeeded();}else if(this.autoStart){this.start();}};Ticker.prototype.add = function add(fn, context){this._emitter.on(TICK, fn, context);this._startIfPossible();return this;};Ticker.prototype.addOnce = function addOnce(fn, context){this._emitter.once(TICK, fn, context);this._startIfPossible();return this;};Ticker.prototype.remove = function remove(fn, context){this._emitter.off(TICK, fn, context);if(!this._emitter.listeners(TICK, true)){this._cancelIfNeeded();}return this;};Ticker.prototype.start = function start(){if(!this.started){this.started = true;this._requestIfNeeded();}};Ticker.prototype.stop = function stop(){if(this.started){this.started = false;this._cancelIfNeeded();}};Ticker.prototype.update = function update(currentTime){var elapsedMS;currentTime = currentTime || performance.now();elapsedMS = this.elapsedMS = currentTime - this.lastTime;if(elapsedMS > this._maxElapsedMS){elapsedMS = this._maxElapsedMS;}this.deltaTime = elapsedMS * CONST.TARGET_FPMS * this.speed;this._emitter.emit(TICK, this.deltaTime);this.lastTime = currentTime;};module.exports = Ticker;}, {"../const":22, eventemitter3:10}], 76:[function(require, module, exports){var Ticker=require("./Ticker");var shared=new Ticker();shared.autoStart = true;module.exports = {shared:shared, Ticker:Ticker};}, {"./Ticker":75}], 77:[function(require, module, exports){var CONST=require("../const");var utils=module.exports = {_uid:0, _saidHello:false, EventEmitter:require("eventemitter3"), pluginTarget:require("./pluginTarget"), async:require("async"), uid:function uid(){return ++utils._uid;}, hex2rgb:function hex2rgb(hex, out){out = out || [];out[0] = (hex >> 16 & 255) / 255;out[1] = (hex >> 8 & 255) / 255;out[2] = (hex & 255) / 255;return out;}, hex2string:function hex2string(hex){hex = hex.toString(16);hex = "000000".substr(0, 6 - hex.length) + hex;return "#" + hex;}, rgb2hex:function rgb2hex(rgb){return (rgb[0] * 255 << 16) + (rgb[1] * 255 << 8) + rgb[2] * 255;}, canUseNewCanvasBlendModes:function canUseNewCanvasBlendModes(){if(typeof document === "undefined"){return false;}var pngHead="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAQAAAABAQMAAADD8p2OAAAAA1BMVEX/";var pngEnd="AAAACklEQVQI12NgAAAAAgAB4iG8MwAAAABJRU5ErkJggg==";var magenta=new Image();magenta.src = pngHead + "AP804Oa6" + pngEnd;var yellow=new Image();yellow.src = pngHead + "/wCKxvRF" + pngEnd;var canvas=document.createElement("canvas");canvas.width = 6;canvas.height = 1;var context=canvas.getContext("2d");context.globalCompositeOperation = "multiply";context.drawImage(magenta, 0, 0);context.drawImage(yellow, 2, 0);var data=context.getImageData(2, 0, 1, 1).data;return data[0] === 255 && data[1] === 0 && data[2] === 0;}, getNextPowerOfTwo:function getNextPowerOfTwo(number){if(number > 0 && (number & number - 1) === 0){return number;}else {var result=1;while(result < number) {result <<= 1;}return result;}}, isPowerOfTwo:function isPowerOfTwo(width, height){return width > 0 && (width & width - 1) === 0 && height > 0 && (height & height - 1) === 0;}, getResolutionOfUrl:function getResolutionOfUrl(url){var resolution=CONST.RETINA_PREFIX.exec(url);if(resolution){return parseFloat(resolution[1]);}return 1;}, sayHello:function sayHello(type){if(utils._saidHello){return;}if(navigator.userAgent.toLowerCase().indexOf("chrome") > -1){var args=["\n %c %c %c Pixi.js " + CONST.VERSION + " - ✰ " + type + " ✰  %c " + " %c " + " http://www.pixijs.com/  %c %c ♥%c♥%c♥ \n\n", "background: #ff66a5; padding:5px 0;", "background: #ff66a5; padding:5px 0;", "color: #ff66a5; background: #030307; padding:5px 0;", "background: #ff66a5; padding:5px 0;", "background: #ffc3dc; padding:5px 0;", "background: #ff66a5; padding:5px 0;", "color: #ff2424; background: #fff; padding:5px 0;", "color: #ff2424; background: #fff; padding:5px 0;", "color: #ff2424; background: #fff; padding:5px 0;"];window.console.log.apply(console, args);}else if(window.console){window.console.log("Pixi.js " + CONST.VERSION + " - " + type + " - http://www.pixijs.com/");}utils._saidHello = true;}, isWebGLSupported:function isWebGLSupported(){var contextOptions={stencil:true};try{if(!window.WebGLRenderingContext){return false;}var canvas=document.createElement("canvas"), gl=canvas.getContext("webgl", contextOptions) || canvas.getContext("experimental-webgl", contextOptions);return !!(gl && gl.getContextAttributes().stencil);}catch(e) {return false;}}, sign:function sign(n){return n?n < 0?-1:1:0;}, removeItems:function removeItems(arr, startIdx, removeCount){var length=arr.length;if(startIdx >= length || removeCount === 0){return;}removeCount = startIdx + removeCount > length?length - startIdx:removeCount;for(var i=startIdx, len=length - removeCount; i < len; ++i) {arr[i] = arr[i + removeCount];}arr.length = len;}, TextureCache:{}, BaseTextureCache:{}};}, {"../const":22, "./pluginTarget":78, async:1, eventemitter3:10}], 78:[function(require, module, exports){function pluginTarget(obj){obj.__plugins = {};obj.registerPlugin = function(pluginName, ctor){obj.__plugins[pluginName] = ctor;};obj.prototype.initPlugins = function(){this.plugins = this.plugins || {};for(var o in obj.__plugins) {this.plugins[o] = new obj.__plugins[o](this);}};obj.prototype.destroyPlugins = function(){for(var o in this.plugins) {this.plugins[o].destroy();this.plugins[o] = null;}this.plugins = null;};}module.exports = {mixin:function mixin(obj){pluginTarget(obj);}};}, {}], 79:[function(require, module, exports){var core=require("./core"), mesh=require("./mesh"), extras=require("./extras"), filters=require("./filters");core.SpriteBatch = function(){throw new ReferenceError("SpriteBatch does not exist any more, please use the new ParticleContainer instead.");};core.AssetLoader = function(){throw new ReferenceError("The loader system was overhauled in pixi v3, please see the new PIXI.loaders.Loader class.");};Object.defineProperties(core, {Stage:{get:function get(){console.warn("You do not need to use a PIXI Stage any more, you can simply render any container.");return core.Container;}}, DisplayObjectContainer:{get:function get(){console.warn("DisplayObjectContainer has been shortened to Container, please use Container from now on.");return core.Container;}}, Strip:{get:function get(){console.warn("The Strip class has been renamed to Mesh and moved to mesh.Mesh, please use mesh.Mesh from now on.");return mesh.Mesh;}}, Rope:{get:function get(){console.warn("The Rope class has been moved to mesh.Rope, please use mesh.Rope from now on.");return mesh.Rope;}}, MovieClip:{get:function get(){console.warn("The MovieClip class has been moved to extras.MovieClip, please use extras.MovieClip from now on.");return extras.MovieClip;}}, TilingSprite:{get:function get(){console.warn("The TilingSprite class has been moved to extras.TilingSprite, please use extras.TilingSprite from now on.");return extras.TilingSprite;}}, BitmapText:{get:function get(){console.warn("The BitmapText class has been moved to extras.BitmapText, please use extras.BitmapText from now on.");return extras.BitmapText;}}, blendModes:{get:function get(){console.warn("The blendModes has been moved to BLEND_MODES, please use BLEND_MODES from now on.");return core.BLEND_MODES;}}, scaleModes:{get:function get(){console.warn("The scaleModes has been moved to SCALE_MODES, please use SCALE_MODES from now on.");return core.SCALE_MODES;}}, BaseTextureCache:{get:function get(){console.warn("The BaseTextureCache class has been moved to utils.BaseTextureCache, please use utils.BaseTextureCache from now on.");return core.utils.BaseTextureCache;}}, TextureCache:{get:function get(){console.warn("The TextureCache class has been moved to utils.TextureCache, please use utils.TextureCache from now on.");return core.utils.TextureCache;}}, math:{get:function get(){console.warn("The math namespace is deprecated, please access members already accessible on PIXI.");return core;}}});core.Sprite.prototype.setTexture = function(texture){this.texture = texture;console.warn("setTexture is now deprecated, please use the texture property, e.g : sprite.texture = texture;");};extras.BitmapText.prototype.setText = function(text){this.text = text;console.warn("setText is now deprecated, please use the text property, e.g : myBitmapText.text = 'my text';");};core.Text.prototype.setText = function(text){this.text = text;console.warn("setText is now deprecated, please use the text property, e.g : myText.text = 'my text';");};core.Text.prototype.setStyle = function(style){this.style = style;console.warn("setStyle is now deprecated, please use the style property, e.g : myText.style = style;");};core.Texture.prototype.setFrame = function(frame){this.frame = frame;console.warn("setFrame is now deprecated, please use the frame property, e.g : myTexture.frame = frame;");};Object.defineProperties(filters, {AbstractFilter:{get:function get(){console.warn("filters.AbstractFilter is an undocumented alias, please use AbstractFilter from now on.");return core.AbstractFilter;}}, FXAAFilter:{get:function get(){console.warn("filters.FXAAFilter is an undocumented alias, please use FXAAFilter from now on.");return core.FXAAFilter;}}, SpriteMaskFilter:{get:function get(){console.warn("filters.SpriteMaskFilter is an undocumented alias, please use SpriteMaskFilter from now on.");return core.SpriteMaskFilter;}}});core.utils.uuid = function(){console.warn("utils.uuid() is deprecated, please use utils.uid() from now on.");return core.utils.uid();};}, {"./core":29, "./extras":86, "./filters":103, "./mesh":128}], 80:[function(require, module, exports){var core=require("../core");function BitmapText(text, style){core.Container.call(this);style = style || {};this.textWidth = 0;this.textHeight = 0;this._glyphs = [];this._font = {tint:style.tint !== undefined?style.tint:16777215, align:style.align || "left", name:null, size:0};this.font = style.font;this._text = text;this.maxWidth = 0;this.maxLineHeight = 0;this.dirty = false;this.updateText();}BitmapText.prototype = Object.create(core.Container.prototype);BitmapText.prototype.constructor = BitmapText;module.exports = BitmapText;Object.defineProperties(BitmapText.prototype, {tint:{get:function get(){return this._font.tint;}, set:function set(value){this._font.tint = typeof value === "number" && value >= 0?value:16777215;this.dirty = true;}}, align:{get:function get(){return this._font.align;}, set:function set(value){this._font.align = value || "left";this.dirty = true;}}, font:{get:function get(){return this._font;}, set:function set(value){if(!value){return;}if(typeof value === "string"){value = value.split(" ");this._font.name = value.length === 1?value[0]:value.slice(1).join(" ");this._font.size = value.length >= 2?parseInt(value[0], 10):BitmapText.fonts[this._font.name].size;}else {this._font.name = value.name;this._font.size = typeof value.size === "number"?value.size:parseInt(value.size, 10);}this.dirty = true;}}, text:{get:function get(){return this._text;}, set:function set(value){value = value.toString() || " ";if(this._text === value){return;}this._text = value;this.dirty = true;}}});BitmapText.prototype.updateText = function(){var data=BitmapText.fonts[this._font.name];var pos=new core.Point();var prevCharCode=null;var chars=[];var lastLineWidth=0;var maxLineWidth=0;var lineWidths=[];var line=0;var scale=this._font.size / data.size;var lastSpace=-1;var maxLineHeight=0;for(var i=0; i < this.text.length; i++) {var charCode=this.text.charCodeAt(i);lastSpace = /(\s)/.test(this.text.charAt(i))?i:lastSpace;if(/(?:\r\n|\r|\n)/.test(this.text.charAt(i))){lineWidths.push(lastLineWidth);maxLineWidth = Math.max(maxLineWidth, lastLineWidth);line++;pos.x = 0;pos.y += data.lineHeight;prevCharCode = null;continue;}if(lastSpace !== -1 && this.maxWidth > 0 && pos.x * scale > this.maxWidth){core.utils.removeItems(chars, lastSpace, i - lastSpace);i = lastSpace;lastSpace = -1;lineWidths.push(lastLineWidth);maxLineWidth = Math.max(maxLineWidth, lastLineWidth);line++;pos.x = 0;pos.y += data.lineHeight;prevCharCode = null;continue;}var charData=data.chars[charCode];if(!charData){continue;}if(prevCharCode && charData.kerning[prevCharCode]){pos.x += charData.kerning[prevCharCode];}chars.push({texture:charData.texture, line:line, charCode:charCode, position:new core.Point(pos.x + charData.xOffset, pos.y + charData.yOffset)});lastLineWidth = pos.x + (charData.texture.width + charData.xOffset);pos.x += charData.xAdvance;maxLineHeight = Math.max(maxLineHeight, charData.yOffset + charData.texture.height);prevCharCode = charCode;}lineWidths.push(lastLineWidth);maxLineWidth = Math.max(maxLineWidth, lastLineWidth);var lineAlignOffsets=[];for(i = 0; i <= line; i++) {var alignOffset=0;if(this._font.align === "right"){alignOffset = maxLineWidth - lineWidths[i];}else if(this._font.align === "center"){alignOffset = (maxLineWidth - lineWidths[i]) / 2;}lineAlignOffsets.push(alignOffset);}var lenChars=chars.length;var tint=this.tint;for(i = 0; i < lenChars; i++) {var c=this._glyphs[i];if(c){c.texture = chars[i].texture;}else {c = new core.Sprite(chars[i].texture);this._glyphs.push(c);}c.position.x = (chars[i].position.x + lineAlignOffsets[chars[i].line]) * scale;c.position.y = chars[i].position.y * scale;c.scale.x = c.scale.y = scale;c.tint = tint;if(!c.parent){this.addChild(c);}}for(i = lenChars; i < this._glyphs.length; ++i) {this.removeChild(this._glyphs[i]);}this.textWidth = maxLineWidth * scale;this.textHeight = (pos.y + data.lineHeight) * scale;this.maxLineHeight = maxLineHeight * scale;};BitmapText.prototype.updateTransform = function(){this.validate();this.containerUpdateTransform();};BitmapText.prototype.getLocalBounds = function(){this.validate();return core.Container.prototype.getLocalBounds.call(this);};BitmapText.prototype.validate = function(){if(this.dirty){this.updateText();this.dirty = false;}};BitmapText.fonts = {};}, {"../core":29}], 81:[function(require, module, exports){var core=require("../core");function MovieClip(textures){core.Sprite.call(this, textures[0] instanceof core.Texture?textures[0]:textures[0].texture);this._textures = null;this._durations = null;this.textures = textures;this.animationSpeed = 1;this.loop = true;this.onComplete = null;this._currentTime = 0;this.playing = false;}MovieClip.prototype = Object.create(core.Sprite.prototype);MovieClip.prototype.constructor = MovieClip;module.exports = MovieClip;Object.defineProperties(MovieClip.prototype, {totalFrames:{get:function get(){return this._textures.length;}}, textures:{get:function get(){return this._textures;}, set:function set(value){if(value[0] instanceof core.Texture){this._textures = value;this._durations = null;}else {this._textures = [];this._durations = [];for(var i=0; i < value.length; i++) {this._textures.push(value[i].texture);this._durations.push(value[i].time);}}}}, currentFrame:{get:function get(){var currentFrame=Math.floor(this._currentTime) % this._textures.length;if(currentFrame < 0){currentFrame += this._textures.length;}return currentFrame;}}});MovieClip.prototype.stop = function(){if(!this.playing){return;}this.playing = false;core.ticker.shared.remove(this.update, this);};MovieClip.prototype.play = function(){if(this.playing){return;}this.playing = true;core.ticker.shared.add(this.update, this);};MovieClip.prototype.gotoAndStop = function(frameNumber){this.stop();this._currentTime = frameNumber;this._texture = this._textures[this.currentFrame];};MovieClip.prototype.gotoAndPlay = function(frameNumber){this._currentTime = frameNumber;this.play();};MovieClip.prototype.update = function(deltaTime){var elapsed=this.animationSpeed * deltaTime;if(this._durations !== null){var lag=this._currentTime % 1 * this._durations[this.currentFrame];lag += elapsed / 60 * 1000;while(lag < 0) {this._currentTime--;lag += this._durations[this.currentFrame];}var sign=Math.sign(this.animationSpeed * deltaTime);this._currentTime = Math.floor(this._currentTime);while(lag >= this._durations[this.currentFrame]) {lag -= this._durations[this.currentFrame] * sign;this._currentTime += sign;}this._currentTime += lag / this._durations[this.currentFrame];}else {this._currentTime += elapsed;}if(this._currentTime < 0 && !this.loop){this.gotoAndStop(0);if(this.onComplete){this.onComplete();}}else if(this._currentTime >= this._textures.length && !this.loop){this.gotoAndStop(this._textures.length - 1);if(this.onComplete){this.onComplete();}}else {this._texture = this._textures[this.currentFrame];}};MovieClip.prototype.destroy = function(){this.stop();core.Sprite.prototype.destroy.call(this);};MovieClip.fromFrames = function(frames){var textures=[];for(var i=0; i < frames.length; ++i) {textures.push(new core.Texture.fromFrame(frames[i]));}return new MovieClip(textures);};MovieClip.fromImages = function(images){var textures=[];for(var i=0; i < images.length; ++i) {textures.push(new core.Texture.fromImage(images[i]));}return new MovieClip(textures);};}, {"../core":29}], 82:[function(require, module, exports){var core=require("../core"), tempPoint=new core.Point(), CanvasTinter=require("../core/renderers/canvas/utils/CanvasTinter");function TilingSprite(texture, width, height){core.Sprite.call(this, texture);this.tileScale = new core.Point(1, 1);this.tilePosition = new core.Point(0, 0);this._width = width || 100;this._height = height || 100;this._uvs = new core.TextureUvs();this._canvasPattern = null;this.shader = new core.AbstractFilter(["precision lowp float;", "attribute vec2 aVertexPosition;", "attribute vec2 aTextureCoord;", "attribute vec4 aColor;", "uniform mat3 projectionMatrix;", "uniform vec4 uFrame;", "uniform vec4 uTransform;", "varying vec2 vTextureCoord;", "varying vec4 vColor;", "void main(void){", "   gl_Position = vec4((projectionMatrix * vec3(aVertexPosition, 1.0)).xy, 0.0, 1.0);", "   vec2 coord = aTextureCoord;", "   coord -= uTransform.xy;", "   coord /= uTransform.zw;", "   vTextureCoord = coord;", "   vColor = vec4(aColor.rgb * aColor.a, aColor.a);", "}"].join("\n"), ["precision lowp float;", "varying vec2 vTextureCoord;", "varying vec4 vColor;", "uniform sampler2D uSampler;", "uniform vec4 uFrame;", "uniform vec2 uPixelSize;", "void main(void){", "   vec2 coord = mod(vTextureCoord, uFrame.zw);", "   coord = clamp(coord, uPixelSize, uFrame.zw - uPixelSize);", "   coord += uFrame.xy;", "   gl_FragColor =  texture2D(uSampler, coord) * vColor ;", "}"].join("\n"), {uFrame:{type:"4fv", value:[0, 0, 1, 1]}, uTransform:{type:"4fv", value:[0, 0, 1, 1]}, uPixelSize:{type:"2fv", value:[1, 1]}});}TilingSprite.prototype = Object.create(core.Sprite.prototype);TilingSprite.prototype.constructor = TilingSprite;module.exports = TilingSprite;Object.defineProperties(TilingSprite.prototype, {width:{get:function get(){return this._width;}, set:function set(value){this._width = value;}}, height:{get:function get(){return this._height;}, set:function set(value){this._height = value;}}});TilingSprite.prototype._onTextureUpdate = function(){return;};TilingSprite.prototype._renderWebGL = function(renderer){var texture=this._texture;if(!texture || !texture._uvs){return;}var tempUvs=texture._uvs, tempWidth=texture._frame.width, tempHeight=texture._frame.height, tw=texture.baseTexture.width, th=texture.baseTexture.height;texture._uvs = this._uvs;texture._frame.width = this.width;texture._frame.height = this.height;this.shader.uniforms.uPixelSize.value[0] = 1 / tw;this.shader.uniforms.uPixelSize.value[1] = 1 / th;this.shader.uniforms.uFrame.value[0] = tempUvs.x0;this.shader.uniforms.uFrame.value[1] = tempUvs.y0;this.shader.uniforms.uFrame.value[2] = tempUvs.x1 - tempUvs.x0;this.shader.uniforms.uFrame.value[3] = tempUvs.y2 - tempUvs.y0;this.shader.uniforms.uTransform.value[0] = this.tilePosition.x % (tempWidth * this.tileScale.x) / this._width;this.shader.uniforms.uTransform.value[1] = this.tilePosition.y % (tempHeight * this.tileScale.y) / this._height;this.shader.uniforms.uTransform.value[2] = tw / this._width * this.tileScale.x;this.shader.uniforms.uTransform.value[3] = th / this._height * this.tileScale.y;renderer.setObjectRenderer(renderer.plugins.sprite);renderer.plugins.sprite.render(this);texture._uvs = tempUvs;texture._frame.width = tempWidth;texture._frame.height = tempHeight;};TilingSprite.prototype._renderCanvas = function(renderer){var texture=this._texture;if(!texture.baseTexture.hasLoaded){return;}var context=renderer.context, transform=this.worldTransform, resolution=renderer.resolution, baseTexture=texture.baseTexture, modX=this.tilePosition.x / this.tileScale.x % texture._frame.width, modY=this.tilePosition.y / this.tileScale.y % texture._frame.height;if(!this._canvasPattern){var tempCanvas=new core.CanvasBuffer(texture._frame.width * resolution, texture._frame.height * resolution);if(this.tint !== 16777215){if(this.cachedTint !== this.tint){this.cachedTint = this.tint;this.tintedTexture = CanvasTinter.getTintedTexture(this, this.tint);}tempCanvas.context.drawImage(this.tintedTexture, 0, 0);}else {tempCanvas.context.drawImage(baseTexture.source, -texture._frame.x * resolution, -texture._frame.y * resolution);}this._canvasPattern = tempCanvas.context.createPattern(tempCanvas.canvas, "repeat");}context.globalAlpha = this.worldAlpha;context.setTransform(transform.a * resolution, transform.b * resolution, transform.c * resolution, transform.d * resolution, transform.tx * resolution, transform.ty * resolution);context.scale(this.tileScale.x / resolution, this.tileScale.y / resolution);context.translate(modX + this.anchor.x * -this._width, modY + this.anchor.y * -this._height);var compositeOperation=renderer.blendModes[this.blendMode];if(compositeOperation !== renderer.context.globalCompositeOperation){context.globalCompositeOperation = compositeOperation;}context.fillStyle = this._canvasPattern;context.fillRect(-modX, -modY, this._width * resolution / this.tileScale.x, this._height * resolution / this.tileScale.y);};TilingSprite.prototype.getBounds = function(){var width=this._width;var height=this._height;var w0=width * (1 - this.anchor.x);var w1=width * -this.anchor.x;var h0=height * (1 - this.anchor.y);var h1=height * -this.anchor.y;var worldTransform=this.worldTransform;var a=worldTransform.a;var b=worldTransform.b;var c=worldTransform.c;var d=worldTransform.d;var tx=worldTransform.tx;var ty=worldTransform.ty;var x1=a * w1 + c * h1 + tx;var y1=d * h1 + b * w1 + ty;var x2=a * w0 + c * h1 + tx;var y2=d * h1 + b * w0 + ty;var x3=a * w0 + c * h0 + tx;var y3=d * h0 + b * w0 + ty;var x4=a * w1 + c * h0 + tx;var y4=d * h0 + b * w1 + ty;var minX, maxX, minY, maxY;minX = x1;minX = x2 < minX?x2:minX;minX = x3 < minX?x3:minX;minX = x4 < minX?x4:minX;minY = y1;minY = y2 < minY?y2:minY;minY = y3 < minY?y3:minY;minY = y4 < minY?y4:minY;maxX = x1;maxX = x2 > maxX?x2:maxX;maxX = x3 > maxX?x3:maxX;maxX = x4 > maxX?x4:maxX;maxY = y1;maxY = y2 > maxY?y2:maxY;maxY = y3 > maxY?y3:maxY;maxY = y4 > maxY?y4:maxY;var bounds=this._bounds;bounds.x = minX;bounds.width = maxX - minX;bounds.y = minY;bounds.height = maxY - minY;this._currentBounds = bounds;return bounds;};TilingSprite.prototype.containsPoint = function(point){this.worldTransform.applyInverse(point, tempPoint);var width=this._width;var height=this._height;var x1=-width * this.anchor.x;var y1;if(tempPoint.x > x1 && tempPoint.x < x1 + width){y1 = -height * this.anchor.y;if(tempPoint.y > y1 && tempPoint.y < y1 + height){return true;}}return false;};TilingSprite.prototype.destroy = function(){core.Sprite.prototype.destroy.call(this);this.tileScale = null;this._tileScaleOffset = null;this.tilePosition = null;this._uvs = null;};TilingSprite.fromFrame = function(frameId, width, height){var texture=core.utils.TextureCache[frameId];if(!texture){throw new Error("The frameId \"" + frameId + "\" does not exist in the texture cache " + this);}return new TilingSprite(texture, width, height);};TilingSprite.fromImage = function(imageId, width, height, crossorigin, scaleMode){return new TilingSprite(core.Texture.fromImage(imageId, crossorigin, scaleMode), width, height);};}, {"../core":29, "../core/renderers/canvas/utils/CanvasTinter":48}], 83:[function(require, module, exports){var core=require("../core"), DisplayObject=core.DisplayObject, _tempMatrix=new core.Matrix();DisplayObject.prototype._cacheAsBitmap = false;DisplayObject.prototype._originalRenderWebGL = null;DisplayObject.prototype._originalRenderCanvas = null;DisplayObject.prototype._originalUpdateTransform = null;DisplayObject.prototype._originalHitTest = null;DisplayObject.prototype._originalDestroy = null;DisplayObject.prototype._cachedSprite = null;Object.defineProperties(DisplayObject.prototype, {cacheAsBitmap:{get:function get(){return this._cacheAsBitmap;}, set:function set(value){if(this._cacheAsBitmap === value){return;}this._cacheAsBitmap = value;if(value){this._originalRenderWebGL = this.renderWebGL;this._originalRenderCanvas = this.renderCanvas;this._originalUpdateTransform = this.updateTransform;this._originalGetBounds = this.getBounds;this._originalDestroy = this.destroy;this._originalContainsPoint = this.containsPoint;this.renderWebGL = this._renderCachedWebGL;this.renderCanvas = this._renderCachedCanvas;this.destroy = this._cacheAsBitmapDestroy;}else {if(this._cachedSprite){this._destroyCachedDisplayObject();}this.renderWebGL = this._originalRenderWebGL;this.renderCanvas = this._originalRenderCanvas;this.getBounds = this._originalGetBounds;this.destroy = this._originalDestroy;this.updateTransform = this._originalUpdateTransform;this.containsPoint = this._originalContainsPoint;}}}});DisplayObject.prototype._renderCachedWebGL = function(renderer){if(!this.visible || this.worldAlpha <= 0 || !this.renderable){return;}this._initCachedDisplayObject(renderer);this._cachedSprite.worldAlpha = this.worldAlpha;renderer.setObjectRenderer(renderer.plugins.sprite);renderer.plugins.sprite.render(this._cachedSprite);};DisplayObject.prototype._initCachedDisplayObject = function(renderer){if(this._cachedSprite){return;}renderer.currentRenderer.flush();var bounds=this.getLocalBounds().clone();if(this._filters){var padding=this._filters[0].padding;bounds.x -= padding;bounds.y -= padding;bounds.width += padding * 2;bounds.height += padding * 2;}var cachedRenderTarget=renderer.currentRenderTarget;var stack=renderer.filterManager.filterStack;var renderTexture=new core.RenderTexture(renderer, bounds.width | 0, bounds.height | 0);var m=_tempMatrix;m.tx = -bounds.x;m.ty = -bounds.y;this.renderWebGL = this._originalRenderWebGL;renderTexture.render(this, m, true, true);renderer.setRenderTarget(cachedRenderTarget);renderer.filterManager.filterStack = stack;this.renderWebGL = this._renderCachedWebGL;this.updateTransform = this.displayObjectUpdateTransform;this.getBounds = this._getCachedBounds;this._cachedSprite = new core.Sprite(renderTexture);this._cachedSprite.worldTransform = this.worldTransform;this._cachedSprite.anchor.x = -(bounds.x / bounds.width);this._cachedSprite.anchor.y = -(bounds.y / bounds.height);this.updateTransform();this.containsPoint = this._cachedSprite.containsPoint.bind(this._cachedSprite);};DisplayObject.prototype._renderCachedCanvas = function(renderer){if(!this.visible || this.worldAlpha <= 0 || !this.renderable){return;}this._initCachedDisplayObjectCanvas(renderer);this._cachedSprite.worldAlpha = this.worldAlpha;this._cachedSprite.renderCanvas(renderer);};DisplayObject.prototype._initCachedDisplayObjectCanvas = function(renderer){if(this._cachedSprite){return;}var bounds=this.getLocalBounds();var cachedRenderTarget=renderer.context;var renderTexture=new core.RenderTexture(renderer, bounds.width | 0, bounds.height | 0);var m=_tempMatrix;m.tx = -bounds.x;m.ty = -bounds.y;this.renderCanvas = this._originalRenderCanvas;renderTexture.render(this, m, true);renderer.context = cachedRenderTarget;this.renderCanvas = this._renderCachedCanvas;this.updateTransform = this.displayObjectUpdateTransform;this.getBounds = this._getCachedBounds;this._cachedSprite = new core.Sprite(renderTexture);this._cachedSprite.worldTransform = this.worldTransform;this._cachedSprite.anchor.x = -(bounds.x / bounds.width);this._cachedSprite.anchor.y = -(bounds.y / bounds.height);this.updateTransform();this.containsPoint = this._cachedSprite.containsPoint.bind(this._cachedSprite);};DisplayObject.prototype._getCachedBounds = function(){this._cachedSprite._currentBounds = null;return this._cachedSprite.getBounds();};DisplayObject.prototype._destroyCachedDisplayObject = function(){this._cachedSprite._texture.destroy();this._cachedSprite = null;};DisplayObject.prototype._cacheAsBitmapDestroy = function(){this.cacheAsBitmap = false;this._originalDestroy();};}, {"../core":29}], 84:[function(require, module, exports){var core=require("../core");core.DisplayObject.prototype.name = null;core.Container.prototype.getChildByName = function(name){for(var i=0; i < this.children.length; i++) {if(this.children[i].name === name){return this.children[i];}}return null;};}, {"../core":29}], 85:[function(require, module, exports){var core=require("../core");core.DisplayObject.prototype.getGlobalPosition = function(point){point = point || new core.Point();if(this.parent){this.displayObjectUpdateTransform();point.x = this.worldTransform.tx;point.y = this.worldTransform.ty;}else {point.x = this.position.x;point.y = this.position.y;}return point;};}, {"../core":29}], 86:[function(require, module, exports){require("./cacheAsBitmap");require("./getChildByName");require("./getGlobalPosition");module.exports = {MovieClip:require("./MovieClip"), TilingSprite:require("./TilingSprite"), BitmapText:require("./BitmapText")};}, {"./BitmapText":80, "./MovieClip":81, "./TilingSprite":82, "./cacheAsBitmap":83, "./getChildByName":84, "./getGlobalPosition":85}], 87:[function(require, module, exports){var core=require("../../core");function AsciiFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nuniform vec4 dimensions;\nuniform float pixelSize;\nuniform sampler2D uSampler;\n\nfloat character(float n, vec2 p)\n{\n    p = floor(p*vec2(4.0, -4.0) + 2.5);\n    if (clamp(p.x, 0.0, 4.0) == p.x && clamp(p.y, 0.0, 4.0) == p.y)\n    {\n        if (int(mod(n/exp2(p.x + 5.0*p.y), 2.0)) == 1) return 1.0;\n    }\n    return 0.0;\n}\n\nvoid main()\n{\n    vec2 uv = gl_FragCoord.xy;\n\n    vec3 col = texture2D(uSampler, floor( uv / pixelSize ) * pixelSize / dimensions.xy).rgb;\n\n    float gray = (col.r + col.g + col.b) / 3.0;\n\n    float n =  65536.0;             // .\n    if (gray > 0.2) n = 65600.0;    // :\n    if (gray > 0.3) n = 332772.0;   // *\n    if (gray > 0.4) n = 15255086.0; // o\n    if (gray > 0.5) n = 23385164.0; // &\n    if (gray > 0.6) n = 15252014.0; // 8\n    if (gray > 0.7) n = 13199452.0; // @\n    if (gray > 0.8) n = 11512810.0; // #\n\n    vec2 p = mod( uv / ( pixelSize * 0.5 ), 2.0) - vec2(1.0);\n    col = col * character(n, p);\n\n    gl_FragColor = vec4(col, 1.0);\n}\n", {dimensions:{type:"4fv", value:new Float32Array([0, 0, 0, 0])}, pixelSize:{type:"1f", value:8}});}AsciiFilter.prototype = Object.create(core.AbstractFilter.prototype);AsciiFilter.prototype.constructor = AsciiFilter;module.exports = AsciiFilter;Object.defineProperties(AsciiFilter.prototype, {size:{get:function get(){return this.uniforms.pixelSize.value;}, set:function set(value){this.uniforms.pixelSize.value = value;}}});}, {"../../core":29}], 88:[function(require, module, exports){var core=require("../../core"), BlurXFilter=require("../blur/BlurXFilter"), BlurYFilter=require("../blur/BlurYFilter");function BloomFilter(){core.AbstractFilter.call(this);this.blurXFilter = new BlurXFilter();this.blurYFilter = new BlurYFilter();this.defaultFilter = new core.AbstractFilter();}BloomFilter.prototype = Object.create(core.AbstractFilter.prototype);BloomFilter.prototype.constructor = BloomFilter;module.exports = BloomFilter;BloomFilter.prototype.applyFilter = function(renderer, input, output){var renderTarget=renderer.filterManager.getRenderTarget(true);this.defaultFilter.applyFilter(renderer, input, output);this.blurXFilter.applyFilter(renderer, input, renderTarget);renderer.blendModeManager.setBlendMode(core.BLEND_MODES.SCREEN);this.blurYFilter.applyFilter(renderer, renderTarget, output);renderer.blendModeManager.setBlendMode(core.BLEND_MODES.NORMAL);renderer.filterManager.returnRenderTarget(renderTarget);};Object.defineProperties(BloomFilter.prototype, {blur:{get:function get(){return this.blurXFilter.blur;}, set:function set(value){this.blurXFilter.blur = this.blurYFilter.blur = value;}}, blurX:{get:function get(){return this.blurXFilter.blur;}, set:function set(value){this.blurXFilter.blur = value;}}, blurY:{get:function get(){return this.blurYFilter.blur;}, set:function set(value){this.blurYFilter.blur = value;}}});}, {"../../core":29, "../blur/BlurXFilter":91, "../blur/BlurYFilter":92}], 89:[function(require, module, exports){var core=require("../../core");function BlurDirFilter(dirX, dirY){core.AbstractFilter.call(this, "attribute vec2 aVertexPosition;\nattribute vec2 aTextureCoord;\nattribute vec4 aColor;\n\nuniform float strength;\nuniform float dirX;\nuniform float dirY;\nuniform mat3 projectionMatrix;\n\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\nvarying vec2 vBlurTexCoords[3];\n\nvoid main(void)\n{\n    gl_Position = vec4((projectionMatrix * vec3((aVertexPosition), 1.0)).xy, 0.0, 1.0);\n    vTextureCoord = aTextureCoord;\n\n    vBlurTexCoords[0] = aTextureCoord + vec2( (0.004 * strength) * dirX, (0.004 * strength) * dirY );\n    vBlurTexCoords[1] = aTextureCoord + vec2( (0.008 * strength) * dirX, (0.008 * strength) * dirY );\n    vBlurTexCoords[2] = aTextureCoord + vec2( (0.012 * strength) * dirX, (0.012 * strength) * dirY );\n\n    vColor = vec4(aColor.rgb * aColor.a, aColor.a);\n}\n", "precision lowp float;\n\nvarying vec2 vTextureCoord;\nvarying vec2 vBlurTexCoords[3];\nvarying vec4 vColor;\n\nuniform sampler2D uSampler;\n\nvoid main(void)\n{\n    gl_FragColor = vec4(0.0);\n\n    gl_FragColor += texture2D(uSampler, vTextureCoord     ) * 0.3989422804014327;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 0]) * 0.2419707245191454;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 1]) * 0.05399096651318985;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 2]) * 0.004431848411938341;\n}\n", {strength:{type:"1f", value:1}, dirX:{type:"1f", value:dirX || 0}, dirY:{type:"1f", value:dirY || 0}});this.defaultFilter = new core.AbstractFilter();this.passes = 1;this.dirX = dirX || 0;this.dirY = dirY || 0;this.strength = 4;}BlurDirFilter.prototype = Object.create(core.AbstractFilter.prototype);BlurDirFilter.prototype.constructor = BlurDirFilter;module.exports = BlurDirFilter;BlurDirFilter.prototype.applyFilter = function(renderer, input, output, clear){var shader=this.getShader(renderer);this.uniforms.strength.value = this.strength / 4 / this.passes * (input.frame.width / input.size.width);if(this.passes === 1){renderer.filterManager.applyFilter(shader, input, output, clear);}else {var renderTarget=renderer.filterManager.getRenderTarget(true);renderer.filterManager.applyFilter(shader, input, renderTarget, clear);for(var i=0; i < this.passes - 2; i++) {renderer.filterManager.applyFilter(shader, renderTarget, renderTarget, clear);}renderer.filterManager.applyFilter(shader, renderTarget, output, clear);renderer.filterManager.returnRenderTarget(renderTarget);}};Object.defineProperties(BlurDirFilter.prototype, {blur:{get:function get(){return this.strength;}, set:function set(value){this.padding = value * 0.5;this.strength = value;}}, dirX:{get:function get(){return this.dirX;}, set:function set(value){this.uniforms.dirX.value = value;}}, dirY:{get:function get(){return this.dirY;}, set:function set(value){this.uniforms.dirY.value = value;}}});}, {"../../core":29}], 90:[function(require, module, exports){var core=require("../../core"), BlurXFilter=require("./BlurXFilter"), BlurYFilter=require("./BlurYFilter");function BlurFilter(){core.AbstractFilter.call(this);this.blurXFilter = new BlurXFilter();this.blurYFilter = new BlurYFilter();}BlurFilter.prototype = Object.create(core.AbstractFilter.prototype);BlurFilter.prototype.constructor = BlurFilter;module.exports = BlurFilter;BlurFilter.prototype.applyFilter = function(renderer, input, output){var renderTarget=renderer.filterManager.getRenderTarget(true);this.blurXFilter.applyFilter(renderer, input, renderTarget);this.blurYFilter.applyFilter(renderer, renderTarget, output);renderer.filterManager.returnRenderTarget(renderTarget);};Object.defineProperties(BlurFilter.prototype, {blur:{get:function get(){return this.blurXFilter.blur;}, set:function set(value){this.padding = Math.abs(value) * 0.5;this.blurXFilter.blur = this.blurYFilter.blur = value;}}, passes:{get:function get(){return this.blurXFilter.passes;}, set:function set(value){this.blurXFilter.passes = this.blurYFilter.passes = value;}}, blurX:{get:function get(){return this.blurXFilter.blur;}, set:function set(value){this.blurXFilter.blur = value;}}, blurY:{get:function get(){return this.blurYFilter.blur;}, set:function set(value){this.blurYFilter.blur = value;}}});}, {"../../core":29, "./BlurXFilter":91, "./BlurYFilter":92}], 91:[function(require, module, exports){var core=require("../../core");function BlurXFilter(){core.AbstractFilter.call(this, "attribute vec2 aVertexPosition;\nattribute vec2 aTextureCoord;\nattribute vec4 aColor;\n\nuniform float strength;\nuniform mat3 projectionMatrix;\n\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\nvarying vec2 vBlurTexCoords[6];\n\nvoid main(void)\n{\n    gl_Position = vec4((projectionMatrix * vec3((aVertexPosition), 1.0)).xy, 0.0, 1.0);\n    vTextureCoord = aTextureCoord;\n\n    vBlurTexCoords[ 0] = aTextureCoord + vec2(-0.012 * strength, 0.0);\n    vBlurTexCoords[ 1] = aTextureCoord + vec2(-0.008 * strength, 0.0);\n    vBlurTexCoords[ 2] = aTextureCoord + vec2(-0.004 * strength, 0.0);\n    vBlurTexCoords[ 3] = aTextureCoord + vec2( 0.004 * strength, 0.0);\n    vBlurTexCoords[ 4] = aTextureCoord + vec2( 0.008 * strength, 0.0);\n    vBlurTexCoords[ 5] = aTextureCoord + vec2( 0.012 * strength, 0.0);\n\n    vColor = vec4(aColor.rgb * aColor.a, aColor.a);\n}\n", "precision lowp float;\n\nvarying vec2 vTextureCoord;\nvarying vec2 vBlurTexCoords[6];\nvarying vec4 vColor;\n\nuniform sampler2D uSampler;\n\nvoid main(void)\n{\n    gl_FragColor = vec4(0.0);\n\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 0])*0.004431848411938341;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 1])*0.05399096651318985;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 2])*0.2419707245191454;\n    gl_FragColor += texture2D(uSampler, vTextureCoord     )*0.3989422804014327;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 3])*0.2419707245191454;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 4])*0.05399096651318985;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 5])*0.004431848411938341;\n}\n", {strength:{type:"1f", value:1}});this.passes = 1;this.strength = 4;}BlurXFilter.prototype = Object.create(core.AbstractFilter.prototype);BlurXFilter.prototype.constructor = BlurXFilter;module.exports = BlurXFilter;BlurXFilter.prototype.applyFilter = function(renderer, input, output, clear){var shader=this.getShader(renderer);this.uniforms.strength.value = this.strength / 4 / this.passes * (input.frame.width / input.size.width);if(this.passes === 1){renderer.filterManager.applyFilter(shader, input, output, clear);}else {var renderTarget=renderer.filterManager.getRenderTarget(true);var flip=input;var flop=renderTarget;for(var i=0; i < this.passes - 1; i++) {renderer.filterManager.applyFilter(shader, flip, flop, true);var temp=flop;flop = flip;flip = temp;}renderer.filterManager.applyFilter(shader, flip, output, clear);renderer.filterManager.returnRenderTarget(renderTarget);}};Object.defineProperties(BlurXFilter.prototype, {blur:{get:function get(){return this.strength;}, set:function set(value){this.padding = Math.abs(value) * 0.5;this.strength = value;}}});}, {"../../core":29}], 92:[function(require, module, exports){var core=require("../../core");function BlurYFilter(){core.AbstractFilter.call(this, "attribute vec2 aVertexPosition;\nattribute vec2 aTextureCoord;\nattribute vec4 aColor;\n\nuniform float strength;\nuniform mat3 projectionMatrix;\n\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\nvarying vec2 vBlurTexCoords[6];\n\nvoid main(void)\n{\n    gl_Position = vec4((projectionMatrix * vec3((aVertexPosition), 1.0)).xy, 0.0, 1.0);\n    vTextureCoord = aTextureCoord;\n\n    vBlurTexCoords[ 0] = aTextureCoord + vec2(0.0, -0.012 * strength);\n    vBlurTexCoords[ 1] = aTextureCoord + vec2(0.0, -0.008 * strength);\n    vBlurTexCoords[ 2] = aTextureCoord + vec2(0.0, -0.004 * strength);\n    vBlurTexCoords[ 3] = aTextureCoord + vec2(0.0,  0.004 * strength);\n    vBlurTexCoords[ 4] = aTextureCoord + vec2(0.0,  0.008 * strength);\n    vBlurTexCoords[ 5] = aTextureCoord + vec2(0.0,  0.012 * strength);\n\n   vColor = vec4(aColor.rgb * aColor.a, aColor.a);\n}\n", "precision lowp float;\n\nvarying vec2 vTextureCoord;\nvarying vec2 vBlurTexCoords[6];\nvarying vec4 vColor;\n\nuniform sampler2D uSampler;\n\nvoid main(void)\n{\n    gl_FragColor = vec4(0.0);\n\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 0])*0.004431848411938341;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 1])*0.05399096651318985;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 2])*0.2419707245191454;\n    gl_FragColor += texture2D(uSampler, vTextureCoord     )*0.3989422804014327;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 3])*0.2419707245191454;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 4])*0.05399096651318985;\n    gl_FragColor += texture2D(uSampler, vBlurTexCoords[ 5])*0.004431848411938341;\n}\n", {strength:{type:"1f", value:1}});this.passes = 1;this.strength = 4;}BlurYFilter.prototype = Object.create(core.AbstractFilter.prototype);BlurYFilter.prototype.constructor = BlurYFilter;module.exports = BlurYFilter;BlurYFilter.prototype.applyFilter = function(renderer, input, output, clear){var shader=this.getShader(renderer);this.uniforms.strength.value = Math.abs(this.strength) / 4 / this.passes * (input.frame.height / input.size.height);if(this.passes === 1){renderer.filterManager.applyFilter(shader, input, output, clear);}else {var renderTarget=renderer.filterManager.getRenderTarget(true);var flip=input;var flop=renderTarget;for(var i=0; i < this.passes - 1; i++) {renderer.filterManager.applyFilter(shader, flip, flop, true);var temp=flop;flop = flip;flip = temp;}renderer.filterManager.applyFilter(shader, flip, output, clear);renderer.filterManager.returnRenderTarget(renderTarget);}};Object.defineProperties(BlurYFilter.prototype, {blur:{get:function get(){return this.strength;}, set:function set(value){this.padding = Math.abs(value) * 0.5;this.strength = value;}}});}, {"../../core":29}], 93:[function(require, module, exports){var core=require("../../core");function SmartBlurFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying vec2 vTextureCoord;\n\nuniform sampler2D uSampler;\nuniform vec2 delta;\n\nfloat random(vec3 scale, float seed)\n{\n    return fract(sin(dot(gl_FragCoord.xyz + seed, scale)) * 43758.5453 + seed);\n}\n\nvoid main(void)\n{\n    vec4 color = vec4(0.0);\n    float total = 0.0;\n\n    float offset = random(vec3(12.9898, 78.233, 151.7182), 0.0);\n\n    for (float t = -30.0; t <= 30.0; t++)\n    {\n        float percent = (t + offset - 0.5) / 30.0;\n        float weight = 1.0 - abs(percent);\n        vec4 sample = texture2D(uSampler, vTextureCoord + delta * percent);\n        sample.rgb *= sample.a;\n        color += sample * weight;\n        total += weight;\n    }\n\n    gl_FragColor = color / total;\n    gl_FragColor.rgb /= gl_FragColor.a + 0.00001;\n}\n", {delta:{type:"v2", value:{x:0.1, y:0}}});}SmartBlurFilter.prototype = Object.create(core.AbstractFilter.prototype);SmartBlurFilter.prototype.constructor = SmartBlurFilter;module.exports = SmartBlurFilter;}, {"../../core":29}], 94:[function(require, module, exports){var core=require("../../core");function ColorMatrixFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying vec2 vTextureCoord;\nuniform sampler2D uSampler;\nuniform float m[25];\n\nvoid main(void)\n{\n\n    vec4 c = texture2D(uSampler, vTextureCoord);\n\n    gl_FragColor.r = (m[0] * c.r);\n        gl_FragColor.r += (m[1] * c.g);\n        gl_FragColor.r += (m[2] * c.b);\n        gl_FragColor.r += (m[3] * c.a);\n        gl_FragColor.r += m[4] * c.a;\n\n    gl_FragColor.g = (m[5] * c.r);\n        gl_FragColor.g += (m[6] * c.g);\n        gl_FragColor.g += (m[7] * c.b);\n        gl_FragColor.g += (m[8] * c.a);\n        gl_FragColor.g += m[9] * c.a;\n\n     gl_FragColor.b = (m[10] * c.r);\n        gl_FragColor.b += (m[11] * c.g);\n        gl_FragColor.b += (m[12] * c.b);\n        gl_FragColor.b += (m[13] * c.a);\n        gl_FragColor.b += m[14] * c.a;\n\n     gl_FragColor.a = (m[15] * c.r);\n        gl_FragColor.a += (m[16] * c.g);\n        gl_FragColor.a += (m[17] * c.b);\n        gl_FragColor.a += (m[18] * c.a);\n        gl_FragColor.a += m[19] * c.a;\n\n}\n", {m:{type:"1fv", value:[1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0]}});}ColorMatrixFilter.prototype = Object.create(core.AbstractFilter.prototype);ColorMatrixFilter.prototype.constructor = ColorMatrixFilter;module.exports = ColorMatrixFilter;ColorMatrixFilter.prototype._loadMatrix = function(matrix, multiply){multiply = !!multiply;var newMatrix=matrix;if(multiply){this._multiply(newMatrix, this.uniforms.m.value, matrix);newMatrix = this._colorMatrix(newMatrix);}this.uniforms.m.value = newMatrix;};ColorMatrixFilter.prototype._multiply = function(out, a, b){out[0] = a[0] * b[0] + a[1] * b[5] + a[2] * b[10] + a[3] * b[15];out[1] = a[0] * b[1] + a[1] * b[6] + a[2] * b[11] + a[3] * b[16];out[2] = a[0] * b[2] + a[1] * b[7] + a[2] * b[12] + a[3] * b[17];out[3] = a[0] * b[3] + a[1] * b[8] + a[2] * b[13] + a[3] * b[18];out[4] = a[0] * b[4] + a[1] * b[9] + a[2] * b[14] + a[3] * b[19];out[5] = a[5] * b[0] + a[6] * b[5] + a[7] * b[10] + a[8] * b[15];out[6] = a[5] * b[1] + a[6] * b[6] + a[7] * b[11] + a[8] * b[16];out[7] = a[5] * b[2] + a[6] * b[7] + a[7] * b[12] + a[8] * b[17];out[8] = a[5] * b[3] + a[6] * b[8] + a[7] * b[13] + a[8] * b[18];out[9] = a[5] * b[4] + a[6] * b[9] + a[7] * b[14] + a[8] * b[19];out[10] = a[10] * b[0] + a[11] * b[5] + a[12] * b[10] + a[13] * b[15];out[11] = a[10] * b[1] + a[11] * b[6] + a[12] * b[11] + a[13] * b[16];out[12] = a[10] * b[2] + a[11] * b[7] + a[12] * b[12] + a[13] * b[17];out[13] = a[10] * b[3] + a[11] * b[8] + a[12] * b[13] + a[13] * b[18];out[14] = a[10] * b[4] + a[11] * b[9] + a[12] * b[14] + a[13] * b[19];out[15] = a[15] * b[0] + a[16] * b[5] + a[17] * b[10] + a[18] * b[15];out[16] = a[15] * b[1] + a[16] * b[6] + a[17] * b[11] + a[18] * b[16];out[17] = a[15] * b[2] + a[16] * b[7] + a[17] * b[12] + a[18] * b[17];out[18] = a[15] * b[3] + a[16] * b[8] + a[17] * b[13] + a[18] * b[18];out[19] = a[15] * b[4] + a[16] * b[9] + a[17] * b[14] + a[18] * b[19];return out;};ColorMatrixFilter.prototype._colorMatrix = function(matrix){var m=new Float32Array(matrix);m[4] /= 255;m[9] /= 255;m[14] /= 255;m[19] /= 255;return m;};ColorMatrixFilter.prototype.brightness = function(b, multiply){var matrix=[b, 0, 0, 0, 0, 0, b, 0, 0, 0, 0, 0, b, 0, 0, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.greyscale = function(scale, multiply){var matrix=[scale, scale, scale, 0, 0, scale, scale, scale, 0, 0, scale, scale, scale, 0, 0, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.grayscale = ColorMatrixFilter.prototype.greyscale;ColorMatrixFilter.prototype.blackAndWhite = function(multiply){var matrix=[0.3, 0.6, 0.1, 0, 0, 0.3, 0.6, 0.1, 0, 0, 0.3, 0.6, 0.1, 0, 0, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.hue = function(rotation, multiply){rotation = (rotation || 0) / 180 * Math.PI;var cos=Math.cos(rotation), sin=Math.sin(rotation);var lumR=0.213, lumG=0.715, lumB=0.072;var matrix=[lumR + cos * (1 - lumR) + sin * -lumR, lumG + cos * -lumG + sin * -lumG, lumB + cos * -lumB + sin * (1 - lumB), 0, 0, lumR + cos * -lumR + sin * 0.143, lumG + cos * (1 - lumG) + sin * 0.14, lumB + cos * -lumB + sin * -0.283, 0, 0, lumR + cos * -lumR + sin * -(1 - lumR), lumG + cos * -lumG + sin * lumG, lumB + cos * (1 - lumB) + sin * lumB, 0, 0, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.contrast = function(amount, multiply){var v=(amount || 0) + 1;var o=-128 * (v - 1);var matrix=[v, 0, 0, 0, o, 0, v, 0, 0, o, 0, 0, v, 0, o, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.saturate = function(amount, multiply){var x=(amount || 0) * 2 / 3 + 1;var y=(x - 1) * -0.5;var matrix=[x, y, y, 0, 0, y, x, y, 0, 0, y, y, x, 0, 0, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.desaturate = function(multiply){this.saturate(-1);};ColorMatrixFilter.prototype.negative = function(multiply){var matrix=[0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.sepia = function(multiply){var matrix=[0.393, 0.7689999, 0.18899999, 0, 0, 0.349, 0.6859999, 0.16799999, 0, 0, 0.272, 0.5339999, 0.13099999, 0, 0, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.technicolor = function(multiply){var matrix=[1.9125277891456083, -0.8545344976951645, -0.09155508482755585, 0, 11.793603434377337, -0.3087833385928097, 1.7658908555458428, -0.10601743074722245, 0, -70.35205161461398, -0.231103377548616, -0.7501899197440212, 1.847597816108189, 0, 30.950940869491138, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.polaroid = function(multiply){var matrix=[1.438, -0.062, -0.062, 0, 0, -0.122, 1.378, -0.122, 0, 0, -0.016, -0.016, 1.483, 0, 0, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.toBGR = function(multiply){var matrix=[0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.kodachrome = function(multiply){var matrix=[1.1285582396593525, -0.3967382283601348, -0.03992559172921793, 0, 63.72958762196502, -0.16404339962244616, 1.0835251566291304, -0.05498805115633132, 0, 24.732407896706203, -0.16786010706155763, -0.5603416277695248, 1.6014850761964943, 0, 35.62982807460946, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.browni = function(multiply){var matrix=[0.5997023498159715, 0.34553243048391263, -0.2708298674538042, 0, 47.43192855600873, -0.037703249837783157, 0.8609577587992641, 0.15059552388459913, 0, -36.96841498319127, 0.24113635128153335, -0.07441037908422492, 0.44972182064877153, 0, -7.562075277591283, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.vintage = function(multiply){var matrix=[0.6279345635605994, 0.3202183420819367, -0.03965408211312453, 0, 9.651285835294123, 0.02578397704808868, 0.6441188644374771, 0.03259127616149294, 0, 7.462829176470591, 0.0466055556782719, -0.0851232987247891, 0.5241648018700465, 0, 5.159190588235296, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.colorTone = function(desaturation, toned, lightColor, darkColor, multiply){desaturation = desaturation || 0.2;toned = toned || 0.15;lightColor = lightColor || 16770432;darkColor = darkColor || 3375104;var lR=(lightColor >> 16 & 255) / 255;var lG=(lightColor >> 8 & 255) / 255;var lB=(lightColor & 255) / 255;var dR=(darkColor >> 16 & 255) / 255;var dG=(darkColor >> 8 & 255) / 255;var dB=(darkColor & 255) / 255;var matrix=[0.3, 0.59, 0.11, 0, 0, lR, lG, lB, desaturation, 0, dR, dG, dB, toned, 0, lR - dR, lG - dG, lB - dB, 0, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.night = function(intensity, multiply){intensity = intensity || 0.1;var matrix=[intensity * -2, -intensity, 0, 0, 0, -intensity, 0, intensity, 0, 0, 0, intensity, intensity * 2, 0, 0, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.predator = function(amount, multiply){var matrix=[11.224130630493164 * amount, -4.794486999511719 * amount, -2.8746118545532227 * amount, 0 * amount, 0.40342438220977783 * amount, -3.6330697536468506 * amount, 9.193157196044922 * amount, -2.951810836791992 * amount, 0 * amount, -1.316135048866272 * amount, -3.2184197902679443 * amount, -4.2375030517578125 * amount, 7.476448059082031 * amount, 0 * amount, 0.8044459223747253 * amount, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.lsd = function(multiply){var matrix=[2, -0.4, 0.5, 0, 0, -0.5, 2, -0.4, 0, 0, -0.4, -0.5, 3, 0, 0, 0, 0, 0, 1, 0];this._loadMatrix(matrix, multiply);};ColorMatrixFilter.prototype.reset = function(){var matrix=[1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0];this._loadMatrix(matrix, false);};Object.defineProperties(ColorMatrixFilter.prototype, {matrix:{get:function get(){return this.uniforms.m.value;}, set:function set(value){this.uniforms.m.value = value;}}});}, {"../../core":29}], 95:[function(require, module, exports){var core=require("../../core");function ColorStepFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying vec2 vTextureCoord;\n\nuniform sampler2D uSampler;\nuniform float step;\n\nvoid main(void)\n{\n    vec4 color = texture2D(uSampler, vTextureCoord);\n\n    color = floor(color * step) / step;\n\n    gl_FragColor = color;\n}\n", {step:{type:"1f", value:5}});}ColorStepFilter.prototype = Object.create(core.AbstractFilter.prototype);ColorStepFilter.prototype.constructor = ColorStepFilter;module.exports = ColorStepFilter;Object.defineProperties(ColorStepFilter.prototype, {step:{get:function get(){return this.uniforms.step.value;}, set:function set(value){this.uniforms.step.value = value;}}});}, {"../../core":29}], 96:[function(require, module, exports){var core=require("../../core");function ConvolutionFilter(matrix, width, height){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying mediump vec2 vTextureCoord;\n\nuniform sampler2D uSampler;\nuniform vec2 texelSize;\nuniform float matrix[9];\n\nvoid main(void)\n{\n   vec4 c11 = texture2D(uSampler, vTextureCoord - texelSize); // top left\n   vec4 c12 = texture2D(uSampler, vec2(vTextureCoord.x, vTextureCoord.y - texelSize.y)); // top center\n   vec4 c13 = texture2D(uSampler, vec2(vTextureCoord.x + texelSize.x, vTextureCoord.y - texelSize.y)); // top right\n\n   vec4 c21 = texture2D(uSampler, vec2(vTextureCoord.x - texelSize.x, vTextureCoord.y)); // mid left\n   vec4 c22 = texture2D(uSampler, vTextureCoord); // mid center\n   vec4 c23 = texture2D(uSampler, vec2(vTextureCoord.x + texelSize.x, vTextureCoord.y)); // mid right\n\n   vec4 c31 = texture2D(uSampler, vec2(vTextureCoord.x - texelSize.x, vTextureCoord.y + texelSize.y)); // bottom left\n   vec4 c32 = texture2D(uSampler, vec2(vTextureCoord.x, vTextureCoord.y + texelSize.y)); // bottom center\n   vec4 c33 = texture2D(uSampler, vTextureCoord + texelSize); // bottom right\n\n   gl_FragColor =\n       c11 * matrix[0] + c12 * matrix[1] + c13 * matrix[2] +\n       c21 * matrix[3] + c22 * matrix[4] + c23 * matrix[5] +\n       c31 * matrix[6] + c32 * matrix[7] + c33 * matrix[8];\n\n   gl_FragColor.a = c22.a;\n}\n", {matrix:{type:"1fv", value:new Float32Array(matrix)}, texelSize:{type:"v2", value:{x:1 / width, y:1 / height}}});}ConvolutionFilter.prototype = Object.create(core.AbstractFilter.prototype);ConvolutionFilter.prototype.constructor = ConvolutionFilter;module.exports = ConvolutionFilter;Object.defineProperties(ConvolutionFilter.prototype, {matrix:{get:function get(){return this.uniforms.matrix.value;}, set:function set(value){this.uniforms.matrix.value = new Float32Array(value);}}, width:{get:function get(){return 1 / this.uniforms.texelSize.value.x;}, set:function set(value){this.uniforms.texelSize.value.x = 1 / value;}}, height:{get:function get(){return 1 / this.uniforms.texelSize.value.y;}, set:function set(value){this.uniforms.texelSize.value.y = 1 / value;}}});}, {"../../core":29}], 97:[function(require, module, exports){var core=require("../../core");function CrossHatchFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying vec2 vTextureCoord;\n\nuniform sampler2D uSampler;\n\nvoid main(void)\n{\n    float lum = length(texture2D(uSampler, vTextureCoord.xy).rgb);\n\n    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);\n\n    if (lum < 1.00)\n    {\n        if (mod(gl_FragCoord.x + gl_FragCoord.y, 10.0) == 0.0)\n        {\n            gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);\n        }\n    }\n\n    if (lum < 0.75)\n    {\n        if (mod(gl_FragCoord.x - gl_FragCoord.y, 10.0) == 0.0)\n        {\n            gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);\n        }\n    }\n\n    if (lum < 0.50)\n    {\n        if (mod(gl_FragCoord.x + gl_FragCoord.y - 5.0, 10.0) == 0.0)\n        {\n            gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);\n        }\n    }\n\n    if (lum < 0.3)\n    {\n        if (mod(gl_FragCoord.x - gl_FragCoord.y - 5.0, 10.0) == 0.0)\n        {\n            gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);\n        }\n    }\n}\n");}CrossHatchFilter.prototype = Object.create(core.AbstractFilter.prototype);CrossHatchFilter.prototype.constructor = CrossHatchFilter;module.exports = CrossHatchFilter;}, {"../../core":29}], 98:[function(require, module, exports){var core=require("../../core");function DisplacementFilter(sprite, scale){var maskMatrix=new core.Matrix();sprite.renderable = false;core.AbstractFilter.call(this, "attribute vec2 aVertexPosition;\nattribute vec2 aTextureCoord;\nattribute vec4 aColor;\n\nuniform mat3 projectionMatrix;\nuniform mat3 otherMatrix;\n\nvarying vec2 vMapCoord;\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\n\nvoid main(void)\n{\n   gl_Position = vec4((projectionMatrix * vec3(aVertexPosition, 1.0)).xy, 0.0, 1.0);\n   vTextureCoord = aTextureCoord;\n   vMapCoord = ( otherMatrix * vec3( aTextureCoord, 1.0)  ).xy;\n   vColor = vec4(aColor.rgb * aColor.a, aColor.a);\n}\n", "precision mediump float;\n\nvarying vec2 vMapCoord;\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\n\nuniform vec2 scale;\n\nuniform sampler2D uSampler;\nuniform sampler2D mapSampler;\n\nvoid main(void)\n{\n   vec4 map =  texture2D(mapSampler, vMapCoord);\n\n   map -= 0.5;\n   map.xy *= scale;\n\n   gl_FragColor = texture2D(uSampler, vec2(vTextureCoord.x + map.x, vTextureCoord.y + map.y));\n}\n", {mapSampler:{type:"sampler2D", value:sprite.texture}, otherMatrix:{type:"mat3", value:maskMatrix.toArray(true)}, scale:{type:"v2", value:{x:1, y:1}}});this.maskSprite = sprite;this.maskMatrix = maskMatrix;if(scale === null || scale === undefined){scale = 20;}this.scale = new core.Point(scale, scale);}DisplacementFilter.prototype = Object.create(core.AbstractFilter.prototype);DisplacementFilter.prototype.constructor = DisplacementFilter;module.exports = DisplacementFilter;DisplacementFilter.prototype.applyFilter = function(renderer, input, output){var filterManager=renderer.filterManager;filterManager.calculateMappedMatrix(input.frame, this.maskSprite, this.maskMatrix);this.uniforms.otherMatrix.value = this.maskMatrix.toArray(true);this.uniforms.scale.value.x = this.scale.x * (1 / input.frame.width);this.uniforms.scale.value.y = this.scale.y * (1 / input.frame.height);var shader=this.getShader(renderer);filterManager.applyFilter(shader, input, output);};Object.defineProperties(DisplacementFilter.prototype, {map:{get:function get(){return this.uniforms.mapSampler.value;}, set:function set(value){this.uniforms.mapSampler.value = value;}}});}, {"../../core":29}], 99:[function(require, module, exports){var core=require("../../core");function DotScreenFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\n\nuniform vec4 dimensions;\nuniform sampler2D uSampler;\n\nuniform float angle;\nuniform float scale;\n\nfloat pattern()\n{\n   float s = sin(angle), c = cos(angle);\n   vec2 tex = vTextureCoord * dimensions.xy;\n   vec2 point = vec2(\n       c * tex.x - s * tex.y,\n       s * tex.x + c * tex.y\n   ) * scale;\n   return (sin(point.x) * sin(point.y)) * 4.0;\n}\n\nvoid main()\n{\n   vec4 color = texture2D(uSampler, vTextureCoord);\n   float average = (color.r + color.g + color.b) / 3.0;\n   gl_FragColor = vec4(vec3(average * 10.0 - 5.0 + pattern()), color.a);\n}\n", {scale:{type:"1f", value:1}, angle:{type:"1f", value:5}, dimensions:{type:"4fv", value:[0, 0, 0, 0]}});}DotScreenFilter.prototype = Object.create(core.AbstractFilter.prototype);DotScreenFilter.prototype.constructor = DotScreenFilter;module.exports = DotScreenFilter;Object.defineProperties(DotScreenFilter.prototype, {scale:{get:function get(){return this.uniforms.scale.value;}, set:function set(value){this.uniforms.scale.value = value;}}, angle:{get:function get(){return this.uniforms.angle.value;}, set:function set(value){this.uniforms.angle.value = value;}}});}, {"../../core":29}], 100:[function(require, module, exports){var core=require("../../core");function BlurYTintFilter(){core.AbstractFilter.call(this, "attribute vec2 aVertexPosition;\nattribute vec2 aTextureCoord;\nattribute vec4 aColor;\n\nuniform float strength;\nuniform vec2 offset;\n\nuniform mat3 projectionMatrix;\n\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\nvarying vec2 vBlurTexCoords[6];\n\nvoid main(void)\n{\n    gl_Position = vec4((projectionMatrix * vec3((aVertexPosition+offset), 1.0)).xy, 0.0, 1.0);\n    vTextureCoord = aTextureCoord;\n\n    vBlurTexCoords[ 0] = aTextureCoord + vec2(0.0, -0.012 * strength);\n    vBlurTexCoords[ 1] = aTextureCoord + vec2(0.0, -0.008 * strength);\n    vBlurTexCoords[ 2] = aTextureCoord + vec2(0.0, -0.004 * strength);\n    vBlurTexCoords[ 3] = aTextureCoord + vec2(0.0,  0.004 * strength);\n    vBlurTexCoords[ 4] = aTextureCoord + vec2(0.0,  0.008 * strength);\n    vBlurTexCoords[ 5] = aTextureCoord + vec2(0.0,  0.012 * strength);\n\n   vColor = vec4(aColor.rgb * aColor.a, aColor.a);\n}\n", "precision lowp float;\n\nvarying vec2 vTextureCoord;\nvarying vec2 vBlurTexCoords[6];\nvarying vec4 vColor;\n\nuniform vec3 color;\nuniform float alpha;\n\nuniform sampler2D uSampler;\n\nvoid main(void)\n{\n    vec4 sum = vec4(0.0);\n\n    sum += texture2D(uSampler, vBlurTexCoords[ 0])*0.004431848411938341;\n    sum += texture2D(uSampler, vBlurTexCoords[ 1])*0.05399096651318985;\n    sum += texture2D(uSampler, vBlurTexCoords[ 2])*0.2419707245191454;\n    sum += texture2D(uSampler, vTextureCoord     )*0.3989422804014327;\n    sum += texture2D(uSampler, vBlurTexCoords[ 3])*0.2419707245191454;\n    sum += texture2D(uSampler, vBlurTexCoords[ 4])*0.05399096651318985;\n    sum += texture2D(uSampler, vBlurTexCoords[ 5])*0.004431848411938341;\n\n    gl_FragColor = vec4( color.rgb * sum.a * alpha, sum.a * alpha );\n}\n", {blur:{type:"1f", value:1 / 512}, color:{type:"c", value:[0, 0, 0]}, alpha:{type:"1f", value:0.7}, offset:{type:"2f", value:[5, 5]}, strength:{type:"1f", value:1}});this.passes = 1;this.strength = 4;}BlurYTintFilter.prototype = Object.create(core.AbstractFilter.prototype);BlurYTintFilter.prototype.constructor = BlurYTintFilter;module.exports = BlurYTintFilter;BlurYTintFilter.prototype.applyFilter = function(renderer, input, output, clear){var shader=this.getShader(renderer);this.uniforms.strength.value = this.strength / 4 / this.passes * (input.frame.height / input.size.height);if(this.passes === 1){renderer.filterManager.applyFilter(shader, input, output, clear);}else {var renderTarget=renderer.filterManager.getRenderTarget(true);var flip=input;var flop=renderTarget;for(var i=0; i < this.passes - 1; i++) {renderer.filterManager.applyFilter(shader, flip, flop, clear);var temp=flop;flop = flip;flip = temp;}renderer.filterManager.applyFilter(shader, flip, output, clear);renderer.filterManager.returnRenderTarget(renderTarget);}};Object.defineProperties(BlurYTintFilter.prototype, {blur:{get:function get(){return this.strength;}, set:function set(value){this.padding = value * 0.5;this.strength = value;}}});}, {"../../core":29}], 101:[function(require, module, exports){var core=require("../../core"), BlurXFilter=require("../blur/BlurXFilter"), BlurYTintFilter=require("./BlurYTintFilter");function DropShadowFilter(){core.AbstractFilter.call(this);this.blurXFilter = new BlurXFilter();this.blurYTintFilter = new BlurYTintFilter();this.defaultFilter = new core.AbstractFilter();this.padding = 30;this._dirtyPosition = true;this._angle = 45 * Math.PI / 180;this._distance = 10;this.alpha = 0.75;this.hideObject = false;this.blendMode = core.BLEND_MODES.MULTIPLY;}DropShadowFilter.prototype = Object.create(core.AbstractFilter.prototype);DropShadowFilter.prototype.constructor = DropShadowFilter;module.exports = DropShadowFilter;DropShadowFilter.prototype.applyFilter = function(renderer, input, output){var renderTarget=renderer.filterManager.getRenderTarget(true);if(this._dirtyPosition){this._dirtyPosition = false;this.blurYTintFilter.uniforms.offset.value[0] = Math.sin(this._angle) * this._distance;this.blurYTintFilter.uniforms.offset.value[1] = Math.cos(this._angle) * this._distance;}this.blurXFilter.applyFilter(renderer, input, renderTarget);renderer.blendModeManager.setBlendMode(this.blendMode);this.blurYTintFilter.applyFilter(renderer, renderTarget, output);renderer.blendModeManager.setBlendMode(core.BLEND_MODES.NORMAL);if(!this.hideObject){this.defaultFilter.applyFilter(renderer, input, output);}renderer.filterManager.returnRenderTarget(renderTarget);};Object.defineProperties(DropShadowFilter.prototype, {blur:{get:function get(){return this.blurXFilter.blur;}, set:function set(value){this.blurXFilter.blur = this.blurYTintFilter.blur = value;}}, blurX:{get:function get(){return this.blurXFilter.blur;}, set:function set(value){this.blurXFilter.blur = value;}}, blurY:{get:function get(){return this.blurYTintFilter.blur;}, set:function set(value){this.blurYTintFilter.blur = value;}}, color:{get:function get(){return core.utils.rgb2hex(this.blurYTintFilter.uniforms.color.value);}, set:function set(value){this.blurYTintFilter.uniforms.color.value = core.utils.hex2rgb(value);}}, alpha:{get:function get(){return this.blurYTintFilter.uniforms.alpha.value;}, set:function set(value){this.blurYTintFilter.uniforms.alpha.value = value;}}, distance:{get:function get(){return this._distance;}, set:function set(value){this._dirtyPosition = true;this._distance = value;}}, angle:{get:function get(){return this._angle;}, set:function set(value){this._dirtyPosition = true;this._angle = value;}}});}, {"../../core":29, "../blur/BlurXFilter":91, "./BlurYTintFilter":100}], 102:[function(require, module, exports){var core=require("../../core");function GrayFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\n\nuniform sampler2D uSampler;\nuniform float gray;\n\nvoid main(void)\n{\n   gl_FragColor = texture2D(uSampler, vTextureCoord);\n   gl_FragColor.rgb = mix(gl_FragColor.rgb, vec3(0.2126*gl_FragColor.r + 0.7152*gl_FragColor.g + 0.0722*gl_FragColor.b), gray);\n}\n", {gray:{type:"1f", value:1}});}GrayFilter.prototype = Object.create(core.AbstractFilter.prototype);GrayFilter.prototype.constructor = GrayFilter;module.exports = GrayFilter;Object.defineProperties(GrayFilter.prototype, {gray:{get:function get(){return this.uniforms.gray.value;}, set:function set(value){this.uniforms.gray.value = value;}}});}, {"../../core":29}], 103:[function(require, module, exports){module.exports = {AsciiFilter:require("./ascii/AsciiFilter"), BloomFilter:require("./bloom/BloomFilter"), BlurFilter:require("./blur/BlurFilter"), BlurXFilter:require("./blur/BlurXFilter"), BlurYFilter:require("./blur/BlurYFilter"), BlurDirFilter:require("./blur/BlurDirFilter"), ColorMatrixFilter:require("./color/ColorMatrixFilter"), ColorStepFilter:require("./color/ColorStepFilter"), ConvolutionFilter:require("./convolution/ConvolutionFilter"), CrossHatchFilter:require("./crosshatch/CrossHatchFilter"), DisplacementFilter:require("./displacement/DisplacementFilter"), DotScreenFilter:require("./dot/DotScreenFilter"), GrayFilter:require("./gray/GrayFilter"), DropShadowFilter:require("./dropshadow/DropShadowFilter"), InvertFilter:require("./invert/InvertFilter"), NoiseFilter:require("./noise/NoiseFilter"), PixelateFilter:require("./pixelate/PixelateFilter"), RGBSplitFilter:require("./rgb/RGBSplitFilter"), ShockwaveFilter:require("./shockwave/ShockwaveFilter"), SepiaFilter:require("./sepia/SepiaFilter"), SmartBlurFilter:require("./blur/SmartBlurFilter"), TiltShiftFilter:require("./tiltshift/TiltShiftFilter"), TiltShiftXFilter:require("./tiltshift/TiltShiftXFilter"), TiltShiftYFilter:require("./tiltshift/TiltShiftYFilter"), TwistFilter:require("./twist/TwistFilter")};}, {"./ascii/AsciiFilter":87, "./bloom/BloomFilter":88, "./blur/BlurDirFilter":89, "./blur/BlurFilter":90, "./blur/BlurXFilter":91, "./blur/BlurYFilter":92, "./blur/SmartBlurFilter":93, "./color/ColorMatrixFilter":94, "./color/ColorStepFilter":95, "./convolution/ConvolutionFilter":96, "./crosshatch/CrossHatchFilter":97, "./displacement/DisplacementFilter":98, "./dot/DotScreenFilter":99, "./dropshadow/DropShadowFilter":101, "./gray/GrayFilter":102, "./invert/InvertFilter":104, "./noise/NoiseFilter":105, "./pixelate/PixelateFilter":106, "./rgb/RGBSplitFilter":107, "./sepia/SepiaFilter":108, "./shockwave/ShockwaveFilter":109, "./tiltshift/TiltShiftFilter":111, "./tiltshift/TiltShiftXFilter":112, "./tiltshift/TiltShiftYFilter":113, "./twist/TwistFilter":114}], 104:[function(require, module, exports){var core=require("../../core");function InvertFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying vec2 vTextureCoord;\n\nuniform float invert;\nuniform sampler2D uSampler;\n\nvoid main(void)\n{\n    gl_FragColor = texture2D(uSampler, vTextureCoord);\n\n    gl_FragColor.rgb = mix( (vec3(1)-gl_FragColor.rgb) * gl_FragColor.a, gl_FragColor.rgb, 1.0 - invert);\n}\n", {invert:{type:"1f", value:1}});}InvertFilter.prototype = Object.create(core.AbstractFilter.prototype);InvertFilter.prototype.constructor = InvertFilter;module.exports = InvertFilter;Object.defineProperties(InvertFilter.prototype, {invert:{get:function get(){return this.uniforms.invert.value;}, set:function set(value){this.uniforms.invert.value = value;}}});}, {"../../core":29}], 105:[function(require, module, exports){var core=require("../../core");function NoiseFilter(){core.AbstractFilter.call(this, null, "precision highp float;\n\nvarying vec2 vTextureCoord;\nvarying vec4 vColor;\n\nuniform float noise;\nuniform sampler2D uSampler;\n\nfloat rand(vec2 co)\n{\n    return fract(sin(dot(co.xy, vec2(12.9898, 78.233))) * 43758.5453);\n}\n\nvoid main()\n{\n    vec4 color = texture2D(uSampler, vTextureCoord);\n\n    float diff = (rand(vTextureCoord) - 0.5) * noise;\n\n    color.r += diff;\n    color.g += diff;\n    color.b += diff;\n\n    gl_FragColor = color;\n}\n", {noise:{type:"1f", value:0.5}});}NoiseFilter.prototype = Object.create(core.AbstractFilter.prototype);NoiseFilter.prototype.constructor = NoiseFilter;module.exports = NoiseFilter;Object.defineProperties(NoiseFilter.prototype, {noise:{get:function get(){return this.uniforms.noise.value;}, set:function set(value){this.uniforms.noise.value = value;}}});}, {"../../core":29}], 106:[function(require, module, exports){var core=require("../../core");function PixelateFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying vec2 vTextureCoord;\n\nuniform vec4 dimensions;\nuniform vec2 pixelSize;\nuniform sampler2D uSampler;\n\nvoid main(void)\n{\n    vec2 coord = vTextureCoord;\n\n    vec2 size = dimensions.xy / pixelSize;\n\n    vec2 color = floor( ( vTextureCoord * size ) ) / size + pixelSize/dimensions.xy * 0.5;\n\n    gl_FragColor = texture2D(uSampler, color);\n}\n", {dimensions:{type:"4fv", value:new Float32Array([0, 0, 0, 0])}, pixelSize:{type:"v2", value:{x:10, y:10}}});}PixelateFilter.prototype = Object.create(core.AbstractFilter.prototype);PixelateFilter.prototype.constructor = PixelateFilter;module.exports = PixelateFilter;Object.defineProperties(PixelateFilter.prototype, {size:{get:function get(){return this.uniforms.pixelSize.value;}, set:function set(value){this.uniforms.pixelSize.value = value;}}});}, {"../../core":29}], 107:[function(require, module, exports){var core=require("../../core");function RGBSplitFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying vec2 vTextureCoord;\n\nuniform sampler2D uSampler;\nuniform vec4 dimensions;\nuniform vec2 red;\nuniform vec2 green;\nuniform vec2 blue;\n\nvoid main(void)\n{\n   gl_FragColor.r = texture2D(uSampler, vTextureCoord + red/dimensions.xy).r;\n   gl_FragColor.g = texture2D(uSampler, vTextureCoord + green/dimensions.xy).g;\n   gl_FragColor.b = texture2D(uSampler, vTextureCoord + blue/dimensions.xy).b;\n   gl_FragColor.a = texture2D(uSampler, vTextureCoord).a;\n}\n", {red:{type:"v2", value:{x:20, y:20}}, green:{type:"v2", value:{x:-20, y:20}}, blue:{type:"v2", value:{x:20, y:-20}}, dimensions:{type:"4fv", value:[0, 0, 0, 0]}});}RGBSplitFilter.prototype = Object.create(core.AbstractFilter.prototype);RGBSplitFilter.prototype.constructor = RGBSplitFilter;module.exports = RGBSplitFilter;Object.defineProperties(RGBSplitFilter.prototype, {red:{get:function get(){return this.uniforms.red.value;}, set:function set(value){this.uniforms.red.value = value;}}, green:{get:function get(){return this.uniforms.green.value;}, set:function set(value){this.uniforms.green.value = value;}}, blue:{get:function get(){return this.uniforms.blue.value;}, set:function set(value){this.uniforms.blue.value = value;}}});}, {"../../core":29}], 108:[function(require, module, exports){var core=require("../../core");function SepiaFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying vec2 vTextureCoord;\n\nuniform sampler2D uSampler;\nuniform float sepia;\n\nconst mat3 sepiaMatrix = mat3(0.3588, 0.7044, 0.1368, 0.2990, 0.5870, 0.1140, 0.2392, 0.4696, 0.0912);\n\nvoid main(void)\n{\n   gl_FragColor = texture2D(uSampler, vTextureCoord);\n   gl_FragColor.rgb = mix( gl_FragColor.rgb, gl_FragColor.rgb * sepiaMatrix, sepia);\n}\n", {sepia:{type:"1f", value:1}});}SepiaFilter.prototype = Object.create(core.AbstractFilter.prototype);SepiaFilter.prototype.constructor = SepiaFilter;module.exports = SepiaFilter;Object.defineProperties(SepiaFilter.prototype, {sepia:{get:function get(){return this.uniforms.sepia.value;}, set:function set(value){this.uniforms.sepia.value = value;}}});}, {"../../core":29}], 109:[function(require, module, exports){var core=require("../../core");function ShockwaveFilter(){core.AbstractFilter.call(this, null, "precision lowp float;\n\nvarying vec2 vTextureCoord;\n\nuniform sampler2D uSampler;\n\nuniform vec2 center;\nuniform vec3 params; // 10.0, 0.8, 0.1\nuniform float time;\n\nvoid main()\n{\n    vec2 uv = vTextureCoord;\n    vec2 texCoord = uv;\n\n    float dist = distance(uv, center);\n\n    if ( (dist <= (time + params.z)) && (dist >= (time - params.z)) )\n    {\n        float diff = (dist - time);\n        float powDiff = 1.0 - pow(abs(diff*params.x), params.y);\n\n        float diffTime = diff  * powDiff;\n        vec2 diffUV = normalize(uv - center);\n        texCoord = uv + (diffUV * diffTime);\n    }\n\n    gl_FragColor = texture2D(uSampler, texCoord);\n}\n", {center:{type:"v2", value:{x:0.5, y:0.5}}, params:{type:"v3", value:{x:10, y:0.8, z:0.1}}, time:{type:"1f", value:0}});}ShockwaveFilter.prototype = Object.create(core.AbstractFilter.prototype);ShockwaveFilter.prototype.constructor = ShockwaveFilter;module.exports = ShockwaveFilter;Object.defineProperties(ShockwaveFilter.prototype, {center:{get:function get(){return this.uniforms.center.value;}, set:function set(value){this.uniforms.center.value = value;}}, params:{get:function get(){return this.uniforms.params.value;}, set:function set(value){this.uniforms.params.value = value;}}, time:{get:function get(){return this.uniforms.time.value;}, set:function set(value){this.uniforms.time.value = value;}}});}, {"../../core":29}], 110:[function(require, module, exports){var core=require("../../core");function TiltShiftAxisFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying vec2 vTextureCoord;\n\nuniform sampler2D uSampler;\nuniform float blur;\nuniform float gradientBlur;\nuniform vec2 start;\nuniform vec2 end;\nuniform vec2 delta;\nuniform vec2 texSize;\n\nfloat random(vec3 scale, float seed)\n{\n    return fract(sin(dot(gl_FragCoord.xyz + seed, scale)) * 43758.5453 + seed);\n}\n\nvoid main(void)\n{\n    vec4 color = vec4(0.0);\n    float total = 0.0;\n\n    float offset = random(vec3(12.9898, 78.233, 151.7182), 0.0);\n    vec2 normal = normalize(vec2(start.y - end.y, end.x - start.x));\n    float radius = smoothstep(0.0, 1.0, abs(dot(vTextureCoord * texSize - start, normal)) / gradientBlur) * blur;\n\n    for (float t = -30.0; t <= 30.0; t++)\n    {\n        float percent = (t + offset - 0.5) / 30.0;\n        float weight = 1.0 - abs(percent);\n        vec4 sample = texture2D(uSampler, vTextureCoord + delta / texSize * percent * radius);\n        sample.rgb *= sample.a;\n        color += sample * weight;\n        total += weight;\n    }\n\n    gl_FragColor = color / total;\n    gl_FragColor.rgb /= gl_FragColor.a + 0.00001;\n}\n", {blur:{type:"1f", value:100}, gradientBlur:{type:"1f", value:600}, start:{type:"v2", value:{x:0, y:window.innerHeight / 2}}, end:{type:"v2", value:{x:600, y:window.innerHeight / 2}}, delta:{type:"v2", value:{x:30, y:30}}, texSize:{type:"v2", value:{x:window.innerWidth, y:window.innerHeight}}});this.updateDelta();}TiltShiftAxisFilter.prototype = Object.create(core.AbstractFilter.prototype);TiltShiftAxisFilter.prototype.constructor = TiltShiftAxisFilter;module.exports = TiltShiftAxisFilter;TiltShiftAxisFilter.prototype.updateDelta = function(){this.uniforms.delta.value.x = 0;this.uniforms.delta.value.y = 0;};Object.defineProperties(TiltShiftAxisFilter.prototype, {blur:{get:function get(){return this.uniforms.blur.value;}, set:function set(value){this.uniforms.blur.value = value;}}, gradientBlur:{get:function get(){return this.uniforms.gradientBlur.value;}, set:function set(value){this.uniforms.gradientBlur.value = value;}}, start:{get:function get(){return this.uniforms.start.value;}, set:function set(value){this.uniforms.start.value = value;this.updateDelta();}}, end:{get:function get(){return this.uniforms.end.value;}, set:function set(value){this.uniforms.end.value = value;this.updateDelta();}}});}, {"../../core":29}], 111:[function(require, module, exports){var core=require("../../core"), TiltShiftXFilter=require("./TiltShiftXFilter"), TiltShiftYFilter=require("./TiltShiftYFilter");function TiltShiftFilter(){core.AbstractFilter.call(this);this.tiltShiftXFilter = new TiltShiftXFilter();this.tiltShiftYFilter = new TiltShiftYFilter();}TiltShiftFilter.prototype = Object.create(core.AbstractFilter.prototype);TiltShiftFilter.prototype.constructor = TiltShiftFilter;module.exports = TiltShiftFilter;TiltShiftFilter.prototype.applyFilter = function(renderer, input, output){var renderTarget=renderer.filterManager.getRenderTarget(true);this.tiltShiftXFilter.applyFilter(renderer, input, renderTarget);this.tiltShiftYFilter.applyFilter(renderer, renderTarget, output);renderer.filterManager.returnRenderTarget(renderTarget);};Object.defineProperties(TiltShiftFilter.prototype, {blur:{get:function get(){return this.tiltShiftXFilter.blur;}, set:function set(value){this.tiltShiftXFilter.blur = this.tiltShiftYFilter.blur = value;}}, gradientBlur:{get:function get(){return this.tiltShiftXFilter.gradientBlur;}, set:function set(value){this.tiltShiftXFilter.gradientBlur = this.tiltShiftYFilter.gradientBlur = value;}}, start:{get:function get(){return this.tiltShiftXFilter.start;}, set:function set(value){this.tiltShiftXFilter.start = this.tiltShiftYFilter.start = value;}}, end:{get:function get(){return this.tiltShiftXFilter.end;}, set:function set(value){this.tiltShiftXFilter.end = this.tiltShiftYFilter.end = value;}}});}, {"../../core":29, "./TiltShiftXFilter":112, "./TiltShiftYFilter":113}], 112:[function(require, module, exports){var TiltShiftAxisFilter=require("./TiltShiftAxisFilter");function TiltShiftXFilter(){TiltShiftAxisFilter.call(this);}TiltShiftXFilter.prototype = Object.create(TiltShiftAxisFilter.prototype);TiltShiftXFilter.prototype.constructor = TiltShiftXFilter;module.exports = TiltShiftXFilter;TiltShiftXFilter.prototype.updateDelta = function(){var dx=this.uniforms.end.value.x - this.uniforms.start.value.x;var dy=this.uniforms.end.value.y - this.uniforms.start.value.y;var d=Math.sqrt(dx * dx + dy * dy);this.uniforms.delta.value.x = dx / d;this.uniforms.delta.value.y = dy / d;};}, {"./TiltShiftAxisFilter":110}], 113:[function(require, module, exports){var TiltShiftAxisFilter=require("./TiltShiftAxisFilter");function TiltShiftYFilter(){TiltShiftAxisFilter.call(this);}TiltShiftYFilter.prototype = Object.create(TiltShiftAxisFilter.prototype);TiltShiftYFilter.prototype.constructor = TiltShiftYFilter;module.exports = TiltShiftYFilter;TiltShiftYFilter.prototype.updateDelta = function(){var dx=this.uniforms.end.value.x - this.uniforms.start.value.x;var dy=this.uniforms.end.value.y - this.uniforms.start.value.y;var d=Math.sqrt(dx * dx + dy * dy);this.uniforms.delta.value.x = -dy / d;this.uniforms.delta.value.y = dx / d;};}, {"./TiltShiftAxisFilter":110}], 114:[function(require, module, exports){var core=require("../../core");function TwistFilter(){core.AbstractFilter.call(this, null, "precision mediump float;\n\nvarying vec2 vTextureCoord;\n\nuniform sampler2D uSampler;\nuniform float radius;\nuniform float angle;\nuniform vec2 offset;\n\nvoid main(void)\n{\n   vec2 coord = vTextureCoord - offset;\n   float dist = length(coord);\n\n   if (dist < radius)\n   {\n       float ratio = (radius - dist) / radius;\n       float angleMod = ratio * ratio * angle;\n       float s = sin(angleMod);\n       float c = cos(angleMod);\n       coord = vec2(coord.x * c - coord.y * s, coord.x * s + coord.y * c);\n   }\n\n   gl_FragColor = texture2D(uSampler, coord+offset);\n}\n", {radius:{type:"1f", value:0.5}, angle:{type:"1f", value:5}, offset:{type:"v2", value:{x:0.5, y:0.5}}});}TwistFilter.prototype = Object.create(core.AbstractFilter.prototype);TwistFilter.prototype.constructor = TwistFilter;module.exports = TwistFilter;Object.defineProperties(TwistFilter.prototype, {offset:{get:function get(){return this.uniforms.offset.value;}, set:function set(value){this.uniforms.offset.value = value;}}, radius:{get:function get(){return this.uniforms.radius.value;}, set:function set(value){this.uniforms.radius.value = value;}}, angle:{get:function get(){return this.uniforms.angle.value;}, set:function set(value){this.uniforms.angle.value = value;}}});}, {"../../core":29}], 115:[function(require, module, exports){(function(global){require("./polyfill");var core=module.exports = require("./core");core.extras = require("./extras");core.filters = require("./filters");core.interaction = require("./interaction");core.loaders = require("./loaders");core.mesh = require("./mesh");core.accessibility = require("./accessibility");core.loader = new core.loaders.Loader();Object.assign(core, require("./deprecation"));global.PIXI = core;}).call(this, typeof global !== "undefined"?global:typeof self !== "undefined"?self:typeof window !== "undefined"?window:{});}, {"./accessibility":21, "./core":29, "./deprecation":79, "./extras":86, "./filters":103, "./interaction":118, "./loaders":121, "./mesh":128, "./polyfill":133}], 116:[function(require, module, exports){var core=require("../core");function InteractionData(){this.global = new core.Point();this.target = null;this.originalEvent = null;}InteractionData.prototype.constructor = InteractionData;module.exports = InteractionData;InteractionData.prototype.getLocalPosition = function(displayObject, point, globalPos){return displayObject.worldTransform.applyInverse(globalPos || this.global, point);};}, {"../core":29}], 117:[function(require, module, exports){var core=require("../core"), InteractionData=require("./InteractionData");Object.assign(core.DisplayObject.prototype, require("./interactiveTarget"));function InteractionManager(renderer, options){options = options || {};this.renderer = renderer;this.autoPreventDefault = options.autoPreventDefault !== undefined?options.autoPreventDefault:true;this.interactionFrequency = options.interactionFrequency || 10;this.mouse = new InteractionData();this.eventData = {stopped:false, target:null, type:null, data:this.mouse, stopPropagation:function stopPropagation(){this.stopped = true;}};this.interactiveDataPool = [];this.interactionDOMElement = null;this.moveWhenInside = false;this.eventsAdded = false;this.onMouseUp = this.onMouseUp.bind(this);this.processMouseUp = this.processMouseUp.bind(this);this.onMouseDown = this.onMouseDown.bind(this);this.processMouseDown = this.processMouseDown.bind(this);this.onMouseMove = this.onMouseMove.bind(this);this.processMouseMove = this.processMouseMove.bind(this);this.onMouseOut = this.onMouseOut.bind(this);this.processMouseOverOut = this.processMouseOverOut.bind(this);this.onTouchStart = this.onTouchStart.bind(this);this.processTouchStart = this.processTouchStart.bind(this);this.onTouchEnd = this.onTouchEnd.bind(this);this.processTouchEnd = this.processTouchEnd.bind(this);this.onTouchMove = this.onTouchMove.bind(this);this.processTouchMove = this.processTouchMove.bind(this);this.last = 0;this.currentCursorStyle = "inherit";this._tempPoint = new core.Point();this.resolution = 1;this.setTargetElement(this.renderer.view, this.renderer.resolution);}InteractionManager.prototype.constructor = InteractionManager;module.exports = InteractionManager;InteractionManager.prototype.setTargetElement = function(element, resolution){this.removeEvents();this.interactionDOMElement = element;this.resolution = resolution || 1;this.addEvents();};InteractionManager.prototype.addEvents = function(){if(!this.interactionDOMElement){return;}core.ticker.shared.add(this.update, this);if(window.navigator.msPointerEnabled){this.interactionDOMElement.style["-ms-content-zooming"] = "none";this.interactionDOMElement.style["-ms-touch-action"] = "none";}window.document.addEventListener("mousemove", this.onMouseMove, true);this.interactionDOMElement.addEventListener("mousedown", this.onMouseDown, true);this.interactionDOMElement.addEventListener("mouseout", this.onMouseOut, true);this.interactionDOMElement.addEventListener("touchstart", this.onTouchStart, true);this.interactionDOMElement.addEventListener("touchend", this.onTouchEnd, true);this.interactionDOMElement.addEventListener("touchmove", this.onTouchMove, true);window.addEventListener("mouseup", this.onMouseUp, true);this.eventsAdded = true;};InteractionManager.prototype.removeEvents = function(){if(!this.interactionDOMElement){return;}core.ticker.shared.remove(this.update);if(window.navigator.msPointerEnabled){this.interactionDOMElement.style["-ms-content-zooming"] = "";this.interactionDOMElement.style["-ms-touch-action"] = "";}window.document.removeEventListener("mousemove", this.onMouseMove, true);this.interactionDOMElement.removeEventListener("mousedown", this.onMouseDown, true);this.interactionDOMElement.removeEventListener("mouseout", this.onMouseOut, true);this.interactionDOMElement.removeEventListener("touchstart", this.onTouchStart, true);this.interactionDOMElement.removeEventListener("touchend", this.onTouchEnd, true);this.interactionDOMElement.removeEventListener("touchmove", this.onTouchMove, true);this.interactionDOMElement = null;window.removeEventListener("mouseup", this.onMouseUp, true);this.eventsAdded = false;};InteractionManager.prototype.update = function(deltaTime){this._deltaTime += deltaTime;if(this._deltaTime < this.interactionFrequency){return;}this._deltaTime = 0;if(!this.interactionDOMElement){return;}if(this.didMove){this.didMove = false;return;}this.cursor = "inherit";this.processInteractive(this.mouse.global, this.renderer._lastObjectRendered, this.processMouseOverOut, true);if(this.currentCursorStyle !== this.cursor){this.currentCursorStyle = this.cursor;this.interactionDOMElement.style.cursor = this.cursor;}};InteractionManager.prototype.dispatchEvent = function(displayObject, eventString, eventData){if(!eventData.stopped){eventData.target = displayObject;eventData.type = eventString;displayObject.emit(eventString, eventData);if(displayObject[eventString]){displayObject[eventString](eventData);}}};InteractionManager.prototype.mapPositionToPoint = function(point, x, y){var rect=this.interactionDOMElement.getBoundingClientRect();point.x = (x - rect.left) * (this.interactionDOMElement.width / rect.width) / this.resolution;point.y = (y - rect.top) * (this.interactionDOMElement.height / rect.height) / this.resolution;};InteractionManager.prototype.processInteractive = function(point, displayObject, func, hitTest, interactive){if(!displayObject || !displayObject.visible){return false;}var hit=false, interactiveParent=interactive = displayObject.interactive || interactive;if(displayObject.hitArea){interactiveParent = false;}if(displayObject.interactiveChildren){var children=displayObject.children;for(var i=children.length - 1; i >= 0; i--) {var child=children[i];if(this.processInteractive(point, child, func, hitTest, interactiveParent)){if(!child.parent){continue;}hit = true;interactiveParent = false;hitTest = false;}}}if(interactive){if(hitTest && !hit){if(displayObject.hitArea){displayObject.worldTransform.applyInverse(point, this._tempPoint);hit = displayObject.hitArea.contains(this._tempPoint.x, this._tempPoint.y);}else if(displayObject.containsPoint){hit = displayObject.containsPoint(point);}}if(displayObject.interactive){func(displayObject, hit);}}return hit;};InteractionManager.prototype.onMouseDown = function(event){this.mouse.originalEvent = event;this.eventData.data = this.mouse;this.eventData.stopped = false;this.mapPositionToPoint(this.mouse.global, event.clientX, event.clientY);if(this.autoPreventDefault){this.mouse.originalEvent.preventDefault();}this.processInteractive(this.mouse.global, this.renderer._lastObjectRendered, this.processMouseDown, true);};InteractionManager.prototype.processMouseDown = function(displayObject, hit){var e=this.mouse.originalEvent;var isRightButton=e.button === 2 || e.which === 3;if(hit){displayObject[isRightButton?"_isRightDown":"_isLeftDown"] = true;this.dispatchEvent(displayObject, isRightButton?"rightdown":"mousedown", this.eventData);}};InteractionManager.prototype.onMouseUp = function(event){this.mouse.originalEvent = event;this.eventData.data = this.mouse;this.eventData.stopped = false;this.mapPositionToPoint(this.mouse.global, event.clientX, event.clientY);this.processInteractive(this.mouse.global, this.renderer._lastObjectRendered, this.processMouseUp, true);};InteractionManager.prototype.processMouseUp = function(displayObject, hit){var e=this.mouse.originalEvent;var isRightButton=e.button === 2 || e.which === 3;var isDown=isRightButton?"_isRightDown":"_isLeftDown";if(hit){this.dispatchEvent(displayObject, isRightButton?"rightup":"mouseup", this.eventData);if(displayObject[isDown]){displayObject[isDown] = false;this.dispatchEvent(displayObject, isRightButton?"rightclick":"click", this.eventData);}}else {if(displayObject[isDown]){displayObject[isDown] = false;this.dispatchEvent(displayObject, isRightButton?"rightupoutside":"mouseupoutside", this.eventData);}}};InteractionManager.prototype.onMouseMove = function(event){this.mouse.originalEvent = event;this.eventData.data = this.mouse;this.eventData.stopped = false;this.mapPositionToPoint(this.mouse.global, event.clientX, event.clientY);this.didMove = true;this.cursor = "inherit";this.processInteractive(this.mouse.global, this.renderer._lastObjectRendered, this.processMouseMove, true);if(this.currentCursorStyle !== this.cursor){this.currentCursorStyle = this.cursor;this.interactionDOMElement.style.cursor = this.cursor;}};InteractionManager.prototype.processMouseMove = function(displayObject, hit){this.processMouseOverOut(displayObject, hit);if(!this.moveWhenInside || hit){this.dispatchEvent(displayObject, "mousemove", this.eventData);}};InteractionManager.prototype.onMouseOut = function(event){this.mouse.originalEvent = event;this.eventData.stopped = false;this.mapPositionToPoint(this.mouse.global, event.clientX, event.clientY);this.interactionDOMElement.style.cursor = "inherit";this.mapPositionToPoint(this.mouse.global, event.clientX, event.clientY);this.processInteractive(this.mouse.global, this.renderer._lastObjectRendered, this.processMouseOverOut, false);};InteractionManager.prototype.processMouseOverOut = function(displayObject, hit){if(hit){if(!displayObject._over){displayObject._over = true;this.dispatchEvent(displayObject, "mouseover", this.eventData);}if(displayObject.buttonMode){this.cursor = displayObject.defaultCursor;}}else {if(displayObject._over){displayObject._over = false;this.dispatchEvent(displayObject, "mouseout", this.eventData);}}};InteractionManager.prototype.onTouchStart = function(event){if(this.autoPreventDefault){event.preventDefault();}var changedTouches=event.changedTouches;var cLength=changedTouches.length;for(var i=0; i < cLength; i++) {var touchEvent=changedTouches[i];var touchData=this.getTouchData(touchEvent);touchData.originalEvent = event;this.eventData.data = touchData;this.eventData.stopped = false;this.processInteractive(touchData.global, this.renderer._lastObjectRendered, this.processTouchStart, true);this.returnTouchData(touchData);}};InteractionManager.prototype.processTouchStart = function(displayObject, hit){if(hit){displayObject._touchDown = true;this.dispatchEvent(displayObject, "touchstart", this.eventData);}};InteractionManager.prototype.onTouchEnd = function(event){if(this.autoPreventDefault){event.preventDefault();}var changedTouches=event.changedTouches;var cLength=changedTouches.length;for(var i=0; i < cLength; i++) {var touchEvent=changedTouches[i];var touchData=this.getTouchData(touchEvent);touchData.originalEvent = event;this.eventData.data = touchData;this.eventData.stopped = false;this.processInteractive(touchData.global, this.renderer._lastObjectRendered, this.processTouchEnd, true);this.returnTouchData(touchData);}};InteractionManager.prototype.processTouchEnd = function(displayObject, hit){if(hit){this.dispatchEvent(displayObject, "touchend", this.eventData);if(displayObject._touchDown){displayObject._touchDown = false;this.dispatchEvent(displayObject, "tap", this.eventData);}}else {if(displayObject._touchDown){displayObject._touchDown = false;this.dispatchEvent(displayObject, "touchendoutside", this.eventData);}}};InteractionManager.prototype.onTouchMove = function(event){if(this.autoPreventDefault){event.preventDefault();}var changedTouches=event.changedTouches;var cLength=changedTouches.length;for(var i=0; i < cLength; i++) {var touchEvent=changedTouches[i];var touchData=this.getTouchData(touchEvent);touchData.originalEvent = event;this.eventData.data = touchData;this.eventData.stopped = false;this.processInteractive(touchData.global, this.renderer._lastObjectRendered, this.processTouchMove, this.moveWhenInside);this.returnTouchData(touchData);}};InteractionManager.prototype.processTouchMove = function(displayObject, hit){if(!this.moveWhenInside || hit){this.dispatchEvent(displayObject, "touchmove", this.eventData);}};InteractionManager.prototype.getTouchData = function(touchEvent){var touchData=this.interactiveDataPool.pop();if(!touchData){touchData = new InteractionData();}touchData.identifier = touchEvent.identifier;this.mapPositionToPoint(touchData.global, touchEvent.clientX, touchEvent.clientY);if(navigator.isCocoonJS){touchData.global.x = touchData.global.x / this.resolution;touchData.global.y = touchData.global.y / this.resolution;}touchEvent.globalX = touchData.global.x;touchEvent.globalY = touchData.global.y;return touchData;};InteractionManager.prototype.returnTouchData = function(touchData){this.interactiveDataPool.push(touchData);};InteractionManager.prototype.destroy = function(){this.removeEvents();this.renderer = null;this.mouse = null;this.eventData = null;this.interactiveDataPool = null;this.interactionDOMElement = null;this.onMouseUp = null;this.processMouseUp = null;this.onMouseDown = null;this.processMouseDown = null;this.onMouseMove = null;this.processMouseMove = null;this.onMouseOut = null;this.processMouseOverOut = null;this.onTouchStart = null;this.processTouchStart = null;this.onTouchEnd = null;this.processTouchEnd = null;this.onTouchMove = null;this.processTouchMove = null;this._tempPoint = null;};core.WebGLRenderer.registerPlugin("interaction", InteractionManager);core.CanvasRenderer.registerPlugin("interaction", InteractionManager);}, {"../core":29, "./InteractionData":116, "./interactiveTarget":119}], 118:[function(require, module, exports){module.exports = {InteractionData:require("./InteractionData"), InteractionManager:require("./InteractionManager"), interactiveTarget:require("./interactiveTarget")};}, {"./InteractionData":116, "./InteractionManager":117, "./interactiveTarget":119}], 119:[function(require, module, exports){var interactiveTarget={interactive:false, buttonMode:false, interactiveChildren:true, defaultCursor:"pointer", _over:false, _touchDown:false};module.exports = interactiveTarget;}, {}], 120:[function(require, module, exports){var Resource=require("resource-loader").Resource, core=require("../core"), extras=require("../extras"), path=require("path");function parse(resource, texture){var data={};var info=resource.data.getElementsByTagName("info")[0];var common=resource.data.getElementsByTagName("common")[0];data.font = info.getAttribute("face");data.size = parseInt(info.getAttribute("size"), 10);data.lineHeight = parseInt(common.getAttribute("lineHeight"), 10);data.chars = {};var letters=resource.data.getElementsByTagName("char");for(var i=0; i < letters.length; i++) {var charCode=parseInt(letters[i].getAttribute("id"), 10);var textureRect=new core.Rectangle(parseInt(letters[i].getAttribute("x"), 10) + texture.frame.x, parseInt(letters[i].getAttribute("y"), 10) + texture.frame.y, parseInt(letters[i].getAttribute("width"), 10), parseInt(letters[i].getAttribute("height"), 10));data.chars[charCode] = {xOffset:parseInt(letters[i].getAttribute("xoffset"), 10), yOffset:parseInt(letters[i].getAttribute("yoffset"), 10), xAdvance:parseInt(letters[i].getAttribute("xadvance"), 10), kerning:{}, texture:new core.Texture(texture.baseTexture, textureRect)};}var kernings=resource.data.getElementsByTagName("kerning");for(i = 0; i < kernings.length; i++) {var first=parseInt(kernings[i].getAttribute("first"), 10);var second=parseInt(kernings[i].getAttribute("second"), 10);var amount=parseInt(kernings[i].getAttribute("amount"), 10);if(data.chars[second]){data.chars[second].kerning[first] = amount;}}resource.bitmapFont = data;extras.BitmapText.fonts[data.font] = data;}module.exports = function(){return function(resource, next){if(!resource.data || !resource.isXml){return next();}if(resource.data.getElementsByTagName("page").length === 0 || resource.data.getElementsByTagName("info").length === 0 || resource.data.getElementsByTagName("info")[0].getAttribute("face") === null){return next();}var xmlUrl=path.dirname(resource.url);if(xmlUrl === "."){xmlUrl = "";}if(this.baseUrl && xmlUrl){if(this.baseUrl.charAt(this.baseUrl.length - 1) === "/"){xmlUrl += "/";}xmlUrl = xmlUrl.replace(this.baseUrl, "");}if(xmlUrl && xmlUrl.charAt(xmlUrl.length - 1) !== "/"){xmlUrl += "/";}var textureUrl=xmlUrl + resource.data.getElementsByTagName("page")[0].getAttribute("file");if(core.utils.TextureCache[textureUrl]){parse(resource, core.utils.TextureCache[textureUrl]);next();}else {var loadOptions={crossOrigin:resource.crossOrigin, loadType:Resource.LOAD_TYPE.IMAGE, metadata:resource.metadata.imageMetadata};this.add(resource.name + "_image", textureUrl, loadOptions, function(res){parse(resource, res.texture);next();});}};};}, {"../core":29, "../extras":86, path:2, "resource-loader":16}], 121:[function(require, module, exports){module.exports = {Loader:require("./loader"), bitmapFontParser:require("./bitmapFontParser"), spritesheetParser:require("./spritesheetParser"), textureParser:require("./textureParser"), Resource:require("resource-loader").Resource};}, {"./bitmapFontParser":120, "./loader":122, "./spritesheetParser":123, "./textureParser":124, "resource-loader":16}], 122:[function(require, module, exports){var ResourceLoader=require("resource-loader"), textureParser=require("./textureParser"), spritesheetParser=require("./spritesheetParser"), bitmapFontParser=require("./bitmapFontParser");function Loader(baseUrl, concurrency){ResourceLoader.call(this, baseUrl, concurrency);for(var i=0; i < Loader._pixiMiddleware.length; ++i) {this.use(Loader._pixiMiddleware[i]());}}Loader.prototype = Object.create(ResourceLoader.prototype);Loader.prototype.constructor = Loader;module.exports = Loader;Loader._pixiMiddleware = [ResourceLoader.middleware.parsing.blob, textureParser, spritesheetParser, bitmapFontParser];Loader.addPixiMiddleware = function(fn){Loader._pixiMiddleware.push(fn);};var Resource=ResourceLoader.Resource;Resource.setExtensionXhrType("fnt", Resource.XHR_RESPONSE_TYPE.DOCUMENT);}, {"./bitmapFontParser":120, "./spritesheetParser":123, "./textureParser":124, "resource-loader":16}], 123:[function(require, module, exports){var Resource=require("resource-loader").Resource, path=require("path"), core=require("../core"), async=require("async");var BATCH_SIZE=1000;module.exports = function(){return function(resource, next){var imageResourceName=resource.name + "_image";if(!resource.data || !resource.isJson || !resource.data.frames || this.resources[imageResourceName]){return next();}var loadOptions={crossOrigin:resource.crossOrigin, loadType:Resource.LOAD_TYPE.IMAGE, metadata:resource.metadata.imageMetadata};var route=path.dirname(resource.url.replace(this.baseUrl, ""));this.add(imageResourceName, route + "/" + resource.data.meta.image, loadOptions, function(res){resource.textures = {};var frames=resource.data.frames;var frameKeys=Object.keys(frames);var resolution=core.utils.getResolutionOfUrl(resource.url);var batchIndex=0;function processFrames(initialFrameIndex, maxFrames){var frameIndex=initialFrameIndex;while(frameIndex - initialFrameIndex < maxFrames && frameIndex < frameKeys.length) {var frame=frames[frameKeys[frameIndex]];var rect=frame.frame;if(rect){var size=null;var trim=null;if(frame.rotated){size = new core.Rectangle(rect.x, rect.y, rect.h, rect.w);}else {size = new core.Rectangle(rect.x, rect.y, rect.w, rect.h);}if(frame.trimmed){trim = new core.Rectangle(frame.spriteSourceSize.x / resolution, frame.spriteSourceSize.y / resolution, frame.sourceSize.w / resolution, frame.sourceSize.h / resolution);}if(frame.rotated){var temp=size.width;size.width = size.height;size.height = temp;}size.x /= resolution;size.y /= resolution;size.width /= resolution;size.height /= resolution;resource.textures[frameKeys[frameIndex]] = new core.Texture(res.texture.baseTexture, size, size.clone(), trim, frame.rotated);core.utils.TextureCache[frameKeys[frameIndex]] = resource.textures[frameKeys[frameIndex]];}frameIndex++;}}function shouldProcessNextBatch(){return batchIndex * BATCH_SIZE < frameKeys.length;}function processNextBatch(done){processFrames(batchIndex * BATCH_SIZE, BATCH_SIZE);batchIndex++;setTimeout(done, 0);}if(frameKeys.length <= BATCH_SIZE){processFrames(0, BATCH_SIZE);next();}else {async.whilst(shouldProcessNextBatch, processNextBatch, next);}});};};}, {"../core":29, async:1, path:2, "resource-loader":16}], 124:[function(require, module, exports){var core=require("../core");module.exports = function(){return function(resource, next){if(resource.data && resource.isImage){var baseTexture=new core.BaseTexture(resource.data, null, core.utils.getResolutionOfUrl(resource.url));baseTexture.imageUrl = resource.url;resource.texture = new core.Texture(baseTexture);core.utils.BaseTextureCache[resource.url] = baseTexture;core.utils.TextureCache[resource.url] = resource.texture;}next();};};}, {"../core":29}], 125:[function(require, module, exports){var core=require("../core"), tempPoint=new core.Point(), tempPolygon=new core.Polygon();function Mesh(texture, vertices, uvs, indices, drawMode){core.Container.call(this);this._texture = null;this.uvs = uvs || new Float32Array([0, 0, 1, 0, 1, 1, 0, 1]);this.vertices = vertices || new Float32Array([0, 0, 100, 0, 100, 100, 0, 100]);this.indices = indices || new Uint16Array([0, 1, 3, 2]);this.dirty = true;this.blendMode = core.BLEND_MODES.NORMAL;this.canvasPadding = 0;this.drawMode = drawMode || Mesh.DRAW_MODES.TRIANGLE_MESH;this.texture = texture;this.shader = null;}Mesh.prototype = Object.create(core.Container.prototype);Mesh.prototype.constructor = Mesh;module.exports = Mesh;Object.defineProperties(Mesh.prototype, {texture:{get:function get(){return this._texture;}, set:function set(value){if(this._texture === value){return;}this._texture = value;if(value){if(value.baseTexture.hasLoaded){this._onTextureUpdate();}else {value.once("update", this._onTextureUpdate, this);}}}}});Mesh.prototype._renderWebGL = function(renderer){renderer.setObjectRenderer(renderer.plugins.mesh);renderer.plugins.mesh.render(this);};Mesh.prototype._renderCanvas = function(renderer){var context=renderer.context;var transform=this.worldTransform;var res=renderer.resolution;if(renderer.roundPixels){context.setTransform(transform.a * res, transform.b * res, transform.c * res, transform.d * res, transform.tx * res | 0, transform.ty * res | 0);}else {context.setTransform(transform.a * res, transform.b * res, transform.c * res, transform.d * res, transform.tx * res, transform.ty * res);}if(this.drawMode === Mesh.DRAW_MODES.TRIANGLE_MESH){this._renderCanvasTriangleMesh(context);}else {this._renderCanvasTriangles(context);}};Mesh.prototype._renderCanvasTriangleMesh = function(context){var vertices=this.vertices;var uvs=this.uvs;var length=vertices.length / 2;for(var i=0; i < length - 2; i++) {var index=i * 2;this._renderCanvasDrawTriangle(context, vertices, uvs, index, index + 2, index + 4);}};Mesh.prototype._renderCanvasTriangles = function(context){var vertices=this.vertices;var uvs=this.uvs;var indices=this.indices;var length=indices.length;for(var i=0; i < length; i += 3) {var index0=indices[i] * 2, index1=indices[i + 1] * 2, index2=indices[i + 2] * 2;this._renderCanvasDrawTriangle(context, vertices, uvs, index0, index1, index2);}};Mesh.prototype._renderCanvasDrawTriangle = function(context, vertices, uvs, index0, index1, index2){var base=this._texture.baseTexture;var textureSource=base.source;var textureWidth=base.width;var textureHeight=base.height;var x0=vertices[index0], x1=vertices[index1], x2=vertices[index2];var y0=vertices[index0 + 1], y1=vertices[index1 + 1], y2=vertices[index2 + 1];var u0=uvs[index0] * base.width, u1=uvs[index1] * base.width, u2=uvs[index2] * base.width;var v0=uvs[index0 + 1] * base.height, v1=uvs[index1 + 1] * base.height, v2=uvs[index2 + 1] * base.height;if(this.canvasPadding > 0){var paddingX=this.canvasPadding / this.worldTransform.a;var paddingY=this.canvasPadding / this.worldTransform.d;var centerX=(x0 + x1 + x2) / 3;var centerY=(y0 + y1 + y2) / 3;var normX=x0 - centerX;var normY=y0 - centerY;var dist=Math.sqrt(normX * normX + normY * normY);x0 = centerX + normX / dist * (dist + paddingX);y0 = centerY + normY / dist * (dist + paddingY);normX = x1 - centerX;normY = y1 - centerY;dist = Math.sqrt(normX * normX + normY * normY);x1 = centerX + normX / dist * (dist + paddingX);y1 = centerY + normY / dist * (dist + paddingY);normX = x2 - centerX;normY = y2 - centerY;dist = Math.sqrt(normX * normX + normY * normY);x2 = centerX + normX / dist * (dist + paddingX);y2 = centerY + normY / dist * (dist + paddingY);}context.save();context.beginPath();context.moveTo(x0, y0);context.lineTo(x1, y1);context.lineTo(x2, y2);context.closePath();context.clip();var delta=u0 * v1 + v0 * u2 + u1 * v2 - v1 * u2 - v0 * u1 - u0 * v2;var deltaA=x0 * v1 + v0 * x2 + x1 * v2 - v1 * x2 - v0 * x1 - x0 * v2;var deltaB=u0 * x1 + x0 * u2 + u1 * x2 - x1 * u2 - x0 * u1 - u0 * x2;var deltaC=u0 * v1 * x2 + v0 * x1 * u2 + x0 * u1 * v2 - x0 * v1 * u2 - v0 * u1 * x2 - u0 * x1 * v2;var deltaD=y0 * v1 + v0 * y2 + y1 * v2 - v1 * y2 - v0 * y1 - y0 * v2;var deltaE=u0 * y1 + y0 * u2 + u1 * y2 - y1 * u2 - y0 * u1 - u0 * y2;var deltaF=u0 * v1 * y2 + v0 * y1 * u2 + y0 * u1 * v2 - y0 * v1 * u2 - v0 * u1 * y2 - u0 * y1 * v2;context.transform(deltaA / delta, deltaD / delta, deltaB / delta, deltaE / delta, deltaC / delta, deltaF / delta);context.drawImage(textureSource, 0, 0, textureWidth * base.resolution, textureHeight * base.resolution, 0, 0, textureWidth, textureHeight);context.restore();};Mesh.prototype.renderMeshFlat = function(Mesh){var context=this.context;var vertices=Mesh.vertices;var length=vertices.length / 2;context.beginPath();for(var i=1; i < length - 2; i++) {var index=i * 2;var x0=vertices[index], x1=vertices[index + 2], x2=vertices[index + 4];var y0=vertices[index + 1], y1=vertices[index + 3], y2=vertices[index + 5];context.moveTo(x0, y0);context.lineTo(x1, y1);context.lineTo(x2, y2);}context.fillStyle = "#FF0000";context.fill();context.closePath();};Mesh.prototype._onTextureUpdate = function(){this.updateFrame = true;};Mesh.prototype.getBounds = function(matrix){if(!this._currentBounds){var worldTransform=matrix || this.worldTransform;var a=worldTransform.a;var b=worldTransform.b;var c=worldTransform.c;var d=worldTransform.d;var tx=worldTransform.tx;var ty=worldTransform.ty;var maxX=-Infinity;var maxY=-Infinity;var minX=Infinity;var minY=Infinity;var vertices=this.vertices;for(var i=0, n=vertices.length; i < n; i += 2) {var rawX=vertices[i], rawY=vertices[i + 1];var x=a * rawX + c * rawY + tx;var y=d * rawY + b * rawX + ty;minX = x < minX?x:minX;minY = y < minY?y:minY;maxX = x > maxX?x:maxX;maxY = y > maxY?y:maxY;}if(minX === -Infinity || maxY === Infinity){return core.Rectangle.EMPTY;}var bounds=this._bounds;bounds.x = minX;bounds.width = maxX - minX;bounds.y = minY;bounds.height = maxY - minY;this._currentBounds = bounds;}return this._currentBounds;};Mesh.prototype.containsPoint = function(point){if(!this.getBounds().contains(point.x, point.y)){return false;}this.worldTransform.applyInverse(point, tempPoint);var vertices=this.vertices;var points=tempPolygon.points;var i, len;if(this.drawMode === Mesh.DRAW_MODES.TRIANGLES){var indices=this.indices;len = this.indices.length;for(i = 0; i < len; i += 3) {var ind0=indices[i] * 2, ind1=indices[i + 1] * 2, ind2=indices[i + 2] * 2;points[0] = vertices[ind0];points[1] = vertices[ind0 + 1];points[2] = vertices[ind1];points[3] = vertices[ind1 + 1];points[4] = vertices[ind2];points[5] = vertices[ind2 + 1];if(tempPolygon.contains(tempPoint.x, tempPoint.y)){return true;}}}else {len = vertices.length;for(i = 0; i < len; i += 6) {points[0] = vertices[i];points[1] = vertices[i + 1];points[2] = vertices[i + 2];points[3] = vertices[i + 3];points[4] = vertices[i + 4];points[5] = vertices[i + 5];if(tempPolygon.contains(tempPoint.x, tempPoint.y)){return true;}}}return false;};Mesh.DRAW_MODES = {TRIANGLE_MESH:0, TRIANGLES:1};}, {"../core":29}], 126:[function(require, module, exports){var Mesh=require("./Mesh");function Plane(texture, segmentsX, segmentsY){Mesh.call(this, texture);this._ready = true;this.segmentsX = segmentsX || 10;this.segmentsY = segmentsY || 10;this.drawMode = Mesh.DRAW_MODES.TRIANGLES;this.refresh();}Plane.prototype = Object.create(Mesh.prototype);Plane.prototype.constructor = Plane;module.exports = Plane;Plane.prototype.refresh = function(){var total=this.segmentsX * this.segmentsY;var verts=[];var colors=[];var uvs=[];var indices=[];var texture=this.texture;var segmentsXSub=this.segmentsX - 1;var segmentsYSub=this.segmentsY - 1;var i=0;var sizeX=texture.width / segmentsXSub;var sizeY=texture.height / segmentsYSub;for(i = 0; i < total; i++) {var x=i % this.segmentsX;var y=i / this.segmentsX | 0;verts.push(x * sizeX, y * sizeY);uvs.push(texture._uvs.x0 + (texture._uvs.x1 - texture._uvs.x0) * (x / (this.segmentsX - 1)), texture._uvs.y0 + (texture._uvs.y3 - texture._uvs.y0) * (y / (this.segmentsY - 1)));}var totalSub=segmentsXSub * segmentsYSub;for(i = 0; i < totalSub; i++) {var xpos=i % segmentsXSub;var ypos=i / segmentsXSub | 0;var value=ypos * this.segmentsX + xpos;var value2=ypos * this.segmentsX + xpos + 1;var value3=(ypos + 1) * this.segmentsX + xpos;var value4=(ypos + 1) * this.segmentsX + xpos + 1;indices.push(value, value2, value3);indices.push(value2, value4, value3);}this.vertices = new Float32Array(verts);this.uvs = new Float32Array(uvs);this.colors = new Float32Array(colors);this.indices = new Uint16Array(indices);};Plane.prototype._onTextureUpdate = function(){Mesh.prototype._onTextureUpdate.call(this);if(this._ready){this.refresh();}};}, {"./Mesh":125}], 127:[function(require, module, exports){var Mesh=require("./Mesh");var core=require("../core");function Rope(texture, points){Mesh.call(this, texture);this.points = points;this.vertices = new Float32Array(points.length * 4);this.uvs = new Float32Array(points.length * 4);this.colors = new Float32Array(points.length * 2);this.indices = new Uint16Array(points.length * 2);this._ready = true;this.refresh();}Rope.prototype = Object.create(Mesh.prototype);Rope.prototype.constructor = Rope;module.exports = Rope;Rope.prototype.refresh = function(){var points=this.points;if(points.length < 1 || !this._texture._uvs){return;}var uvs=this.uvs;var indices=this.indices;var colors=this.colors;var textureUvs=this._texture._uvs;var offset=new core.Point(textureUvs.x0, textureUvs.y0);var factor=new core.Point(textureUvs.x2 - textureUvs.x0, textureUvs.y2 - textureUvs.y0);uvs[0] = 0 + offset.x;uvs[1] = 0 + offset.y;uvs[2] = 0 + offset.x;uvs[3] = 1 * factor.y + offset.y;colors[0] = 1;colors[1] = 1;indices[0] = 0;indices[1] = 1;var total=points.length, point, index, amount;for(var i=1; i < total; i++) {point = points[i];index = i * 4;amount = i / (total - 1);uvs[index] = amount * factor.x + offset.x;uvs[index + 1] = 0 + offset.y;uvs[index + 2] = amount * factor.x + offset.x;uvs[index + 3] = 1 * factor.y + offset.y;index = i * 2;colors[index] = 1;colors[index + 1] = 1;index = i * 2;indices[index] = index;indices[index + 1] = index + 1;}this.dirty = true;};Rope.prototype._onTextureUpdate = function(){Mesh.prototype._onTextureUpdate.call(this);if(this._ready){this.refresh();}};Rope.prototype.updateTransform = function(){var points=this.points;if(points.length < 1){return;}var lastPoint=points[0];var nextPoint;var perpX=0;var perpY=0;var vertices=this.vertices;var total=points.length, point, index, ratio, perpLength, num;for(var i=0; i < total; i++) {point = points[i];index = i * 4;if(i < points.length - 1){nextPoint = points[i + 1];}else {nextPoint = point;}perpY = -(nextPoint.x - lastPoint.x);perpX = nextPoint.y - lastPoint.y;ratio = (1 - i / (total - 1)) * 10;if(ratio > 1){ratio = 1;}perpLength = Math.sqrt(perpX * perpX + perpY * perpY);num = this._texture.height / 2;perpX /= perpLength;perpY /= perpLength;perpX *= num;perpY *= num;vertices[index] = point.x + perpX;vertices[index + 1] = point.y + perpY;vertices[index + 2] = point.x - perpX;vertices[index + 3] = point.y - perpY;lastPoint = point;}this.containerUpdateTransform();};}, {"../core":29, "./Mesh":125}], 128:[function(require, module, exports){module.exports = {Mesh:require("./Mesh"), Plane:require("./Plane"), Rope:require("./Rope"), MeshRenderer:require("./webgl/MeshRenderer"), MeshShader:require("./webgl/MeshShader")};}, {"./Mesh":125, "./Plane":126, "./Rope":127, "./webgl/MeshRenderer":129, "./webgl/MeshShader":130}], 129:[function(require, module, exports){var core=require("../../core"), Mesh=require("../Mesh");function MeshRenderer(renderer){core.ObjectRenderer.call(this, renderer);this.indices = new Uint16Array(15000);for(var i=0, j=0; i < 15000; i += 6, j += 4) {this.indices[i + 0] = j + 0;this.indices[i + 1] = j + 1;this.indices[i + 2] = j + 2;this.indices[i + 3] = j + 0;this.indices[i + 4] = j + 2;this.indices[i + 5] = j + 3;}this.currentShader = null;}MeshRenderer.prototype = Object.create(core.ObjectRenderer.prototype);MeshRenderer.prototype.constructor = MeshRenderer;module.exports = MeshRenderer;core.WebGLRenderer.registerPlugin("mesh", MeshRenderer);MeshRenderer.prototype.onContextChange = function(){};MeshRenderer.prototype.render = function(mesh){if(!mesh._vertexBuffer){this._initWebGL(mesh);}var renderer=this.renderer, gl=renderer.gl, texture=mesh._texture.baseTexture, shader=mesh.shader;var drawMode=mesh.drawMode === Mesh.DRAW_MODES.TRIANGLE_MESH?gl.TRIANGLE_STRIP:gl.TRIANGLES;renderer.blendModeManager.setBlendMode(mesh.blendMode);if(!shader){shader = renderer.shaderManager.plugins.meshShader;}else {shader = shader.shaders[gl.id] || shader.getShader(renderer);}this.renderer.shaderManager.setShader(shader);shader.uniforms.translationMatrix.value = mesh.worldTransform.toArray(true);shader.uniforms.projectionMatrix.value = renderer.currentRenderTarget.projectionMatrix.toArray(true);shader.uniforms.alpha.value = mesh.worldAlpha;shader.syncUniforms();if(!mesh.dirty){gl.bindBuffer(gl.ARRAY_BUFFER, mesh._vertexBuffer);gl.bufferSubData(gl.ARRAY_BUFFER, 0, mesh.vertices);gl.vertexAttribPointer(shader.attributes.aVertexPosition, 2, gl.FLOAT, false, 0, 0);gl.bindBuffer(gl.ARRAY_BUFFER, mesh._uvBuffer);gl.vertexAttribPointer(shader.attributes.aTextureCoord, 2, gl.FLOAT, false, 0, 0);gl.activeTexture(gl.TEXTURE0);if(!texture._glTextures[gl.id]){this.renderer.updateTexture(texture);}else {gl.bindTexture(gl.TEXTURE_2D, texture._glTextures[gl.id]);}gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, mesh._indexBuffer);gl.bufferSubData(gl.ELEMENT_ARRAY_BUFFER, 0, mesh.indices);}else {mesh.dirty = false;gl.bindBuffer(gl.ARRAY_BUFFER, mesh._vertexBuffer);gl.bufferData(gl.ARRAY_BUFFER, mesh.vertices, gl.STATIC_DRAW);gl.vertexAttribPointer(shader.attributes.aVertexPosition, 2, gl.FLOAT, false, 0, 0);gl.bindBuffer(gl.ARRAY_BUFFER, mesh._uvBuffer);gl.bufferData(gl.ARRAY_BUFFER, mesh.uvs, gl.STATIC_DRAW);gl.vertexAttribPointer(shader.attributes.aTextureCoord, 2, gl.FLOAT, false, 0, 0);gl.activeTexture(gl.TEXTURE0);if(!texture._glTextures[gl.id]){this.renderer.updateTexture(texture);}else {gl.bindTexture(gl.TEXTURE_2D, texture._glTextures[gl.id]);}gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, mesh._indexBuffer);gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, mesh.indices, gl.STATIC_DRAW);}gl.drawElements(drawMode, mesh.indices.length, gl.UNSIGNED_SHORT, 0);};MeshRenderer.prototype._initWebGL = function(mesh){var gl=this.renderer.gl;mesh._vertexBuffer = gl.createBuffer();mesh._indexBuffer = gl.createBuffer();mesh._uvBuffer = gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER, mesh._vertexBuffer);gl.bufferData(gl.ARRAY_BUFFER, mesh.vertices, gl.DYNAMIC_DRAW);gl.bindBuffer(gl.ARRAY_BUFFER, mesh._uvBuffer);gl.bufferData(gl.ARRAY_BUFFER, mesh.uvs, gl.STATIC_DRAW);if(mesh.colors){mesh._colorBuffer = gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER, mesh._colorBuffer);gl.bufferData(gl.ARRAY_BUFFER, mesh.colors, gl.STATIC_DRAW);}gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, mesh._indexBuffer);gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, mesh.indices, gl.STATIC_DRAW);};MeshRenderer.prototype.flush = function(){};MeshRenderer.prototype.start = function(){this.currentShader = null;};MeshRenderer.prototype.destroy = function(){core.ObjectRenderer.prototype.destroy.call(this);};}, {"../../core":29, "../Mesh":125}], 130:[function(require, module, exports){var core=require("../../core");function MeshShader(shaderManager){core.Shader.call(this, shaderManager, ["precision lowp float;", "attribute vec2 aVertexPosition;", "attribute vec2 aTextureCoord;", "uniform mat3 translationMatrix;", "uniform mat3 projectionMatrix;", "varying vec2 vTextureCoord;", "void main(void){", "   gl_Position = vec4((projectionMatrix * translationMatrix * vec3(aVertexPosition, 1.0)).xy, 0.0, 1.0);", "   vTextureCoord = aTextureCoord;", "}"].join("\n"), ["precision lowp float;", "varying vec2 vTextureCoord;", "uniform float alpha;", "uniform sampler2D uSampler;", "void main(void){", "   gl_FragColor = texture2D(uSampler, vTextureCoord) * alpha ;", "}"].join("\n"), {alpha:{type:"1f", value:0}, translationMatrix:{type:"mat3", value:new Float32Array(9)}, projectionMatrix:{type:"mat3", value:new Float32Array(9)}}, {aVertexPosition:0, aTextureCoord:0});}MeshShader.prototype = Object.create(core.Shader.prototype);MeshShader.prototype.constructor = MeshShader;module.exports = MeshShader;core.ShaderManager.registerPlugin("meshShader", MeshShader);}, {"../../core":29}], 131:[function(require, module, exports){if(!Math.sign){Math.sign = function(x){x = +x;if(x === 0 || isNaN(x)){return x;}return x > 0?1:-1;};}}, {}], 132:[function(require, module, exports){if(!Object.assign){Object.assign = require("object-assign");}}, {"object-assign":11}], 133:[function(require, module, exports){require("./Object.assign");require("./requestAnimationFrame");require("./Math.sign");}, {"./Math.sign":131, "./Object.assign":132, "./requestAnimationFrame":134}], 134:[function(require, module, exports){(function(global){if(!(Date.now && Date.prototype.getTime)){Date.now = function now(){return new Date().getTime();};}if(!(global.performance && global.performance.now)){var startTime=Date.now();if(!global.performance){global.performance = {};}global.performance.now = function(){return Date.now() - startTime;};}var lastTime=Date.now();var vendors=["ms", "moz", "webkit", "o"];for(var x=0; x < vendors.length && !global.requestAnimationFrame; ++x) {global.requestAnimationFrame = global[vendors[x] + "RequestAnimationFrame"];global.cancelAnimationFrame = global[vendors[x] + "CancelAnimationFrame"] || global[vendors[x] + "CancelRequestAnimationFrame"];}if(!global.requestAnimationFrame){global.requestAnimationFrame = function(callback){if(typeof callback !== "function"){throw new TypeError(callback + "is not a function");}var currentTime=Date.now(), delay=16 + lastTime - currentTime;if(delay < 0){delay = 0;}lastTime = currentTime;return setTimeout(function(){lastTime = Date.now();callback(performance.now());}, delay);};}if(!global.cancelAnimationFrame){global.cancelAnimationFrame = function(id){clearTimeout(id);};}}).call(this, typeof global !== "undefined"?global:typeof self !== "undefined"?self:typeof window !== "undefined"?window:{});}, {}]}, {}, [115])(115);});;require.register("deps/phoenix/web/static/js/phoenix", function(exports, require, module) {
"use strict";

var _toConsumableArray = function (arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) arr2[i] = arr[i]; return arr2; } else { return Array.from(arr); } };

var _createClass = (function () { function defineProperties(target, props) { for (var key in props) { var prop = props[key]; prop.configurable = true; if (prop.value) prop.writable = true; } Object.defineProperties(target, props); } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; })();

var _classCallCheck = function (instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } };

// Phoenix Channels JavaScript client
//
// ## Socket Connection
//
// A single connection is established to the server and
// channels are multiplexed over the connection.
// Connect to the server using the `Socket` class:
//
//     let socket = new Socket("/ws", {params: {userToken: "123"}})
//     socket.connect()
//
// The `Socket` constructor takes the mount point of the socket,
// the authentication params, as well as options that can be found in
// the Socket docs, such as configuring the `LongPoll` transport, and
// heartbeat.
//
// ## Channels
//
// Channels are isolated, concurrent processes on the server that
// subscribe to topics and broker events between the client and server.
// To join a channel, you must provide the topic, and channel params for
// authorization. Here's an example chat room example where `"new_msg"`
// events are listened for, messages are pushed to the server, and
// the channel is joined with ok/error/timeout matches:
//
//     let channel = socket.channel("rooms:123", {token: roomToken})
//     channel.on("new_msg", msg => console.log("Got message", msg) )
//     $input.onEnter( e => {
//       channel.push("new_msg", {body: e.target.val}, 10000)
//        .receive("ok", (msg) => console.log("created message", msg) )
//        .receive("error", (reasons) => console.log("create failed", reasons) )
//        .receive("timeout", () => console.log("Networking issue...") )
//     })
//     channel.join()
//       .receive("ok", ({messages}) => console.log("catching up", messages) )
//       .receive("error", ({reason}) => console.log("failed join", reason) )
//       .receive("timeout", () => console.log("Networking issue. Still waiting...") )
//
//
// ## Joining
//
// Creating a channel with `socket.channel(topic, params)`, binds the params to
// `channel.params`, which are sent up on `channel.join()`.
// Subsequent rejoins will send up the modified params for
// updating authorization params, or passing up last_message_id information.
// Successful joins receive an "ok" status, while unsuccessful joins
// receive "error".
//
// ## Duplicate Join Subscriptions
//
// While the client may join any number of topics on any number of channels,
// the client may only hold a single subscription for each unique topic at any
// given time. When attempting to create a duplicate subscription,
// the server will close the existing channel, log a warning, and
// spawn a new channel for the topic. The client will have their
// `channel.onClose` callbacks fired for the existing channel, and the new
// channel join will have its receive hooks processed as normal.
//
// ## Pushing Messages
//
// From the previous example, we can see that pushing messages to the server
// can be done with `channel.push(eventName, payload)` and we can optionally
// receive responses from the push. Additionally, we can use
// `receive("timeout", callback)` to abort waiting for our other `receive` hooks
//  and take action after some period of waiting. The default timeout is 5000ms.
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
//     channel.onError( () => console.log("there was an error!") )
//     channel.onClose( () => console.log("the channel has gone away gracefully") )
//
// ### onError hooks
//
// `onError` hooks are invoked if the socket connection drops, or the channel
// crashes on the server. In either case, a channel rejoin is attempted
// automatically in an exponential backoff manner.
//
// ### onClose hooks
//
// `onClose` hooks are invoked only in two cases. 1) the channel explicitly
// closed on the server, or 2). The client explicitly closed, by calling
// `channel.leave()`
//
//
// ## Presence
//
// The `Presence` object provides features for syncing presence information
// from the server with the client and handling presences joining and leaving.
//
// ### Syncing initial state from the server
//
// `Presence.syncState` is used to sync the list of presences on the server
// with the client's state. An optional `onJoin` and `onLeave` callback can
// be provided to react to changes in the client's local presences across
// disconnects and reconnects with the server.
//
// `Presence.syncDiff` is used to sync a diff of presence join and leave
// events from the server, as they happen. Like `syncState`, `syncDiff`
// accepts optional `onJoin` and `onLeave` callbacks to react to a user
// joining or leaving from a device.
//
// ### Listing Presences
//
// `Presence.list` is used to return a list of presence information
// based on the local state of metadata. By default, all presence
// metadata is returned, but a `listBy` function can be supplied to
// allow the client to select which metadata to use for a given presence.
// For example, you may have a user online from different devices with a
// a metadata status of "online", but they have set themselves to "away"
// on another device. In this case, they app may choose to use the "away"
// status for what appears on the UI. The example below defines a `listBy`
// function which prioritizes the first metadata which was registered for
// each user. This could be the first tab they opened, or the first device
// they came online from:
//
//     let state = {}
//     Presence.syncState(state, stateFromServer)
//     let listBy = (id, {metas: [first, ...rest]}) => {
//       first.count = rest.length + 1 // count of this user's presences
//       first.id = id
//       return first
//     }
//     let onlineUsers = Presence.list(state, listBy)
//
//
// ### Example Usage
//
//     // detect if user has joined for the 1st time or from another tab/device
//     let onJoin = (id, current, newPres) => {
//       if(!current){
//         console.log("user has entered for the first time", newPres)
//       } else {
//         console.log("user additional presence", newPres)
//       }
//     }
//     // detect if user has left from all tabs/devices, or is still present
//     let onLeave = (id, current, leftPres) => {
//       if(current.metas.length === 0){
//         console.log("user has left from all devices", leftPres)
//       } else {
//         console.log("user left from a device", leftPres)
//       }
//     }
//     let presences = {} // client's initial empty presence state
//     // receive initial presence data from server, sent after join
//     myChannel.on("presences", state => {
//       Presence.syncState(presences, state, onJoin, onLeave)
//       displayUsers(Presence.list(presences))
//     })
//     // receive "presence_diff" from server, containing join/leave events
//     myChannel.on("presence_diff", diff => {
//       Presence.syncDiff(presences, diff, onJoin, onLeave)
//       this.setState({users: Presence.list(room.presences, listBy)})
//     })
//
var VSN = "1.0.0";
var SOCKET_STATES = { connecting: 0, open: 1, closing: 2, closed: 3 };
var DEFAULT_TIMEOUT = 10000;
var CHANNEL_STATES = {
  closed: "closed",
  errored: "errored",
  joined: "joined",
  joining: "joining",
  leaving: "leaving" };
var CHANNEL_EVENTS = {
  close: "phx_close",
  error: "phx_error",
  join: "phx_join",
  reply: "phx_reply",
  leave: "phx_leave"
};
var TRANSPORTS = {
  longpoll: "longpoll",
  websocket: "websocket"
};

var Push = (function () {

  // Initializes the Push
  //
  // channel - The Channel
  // event - The event, for example `"phx_join"`
  // payload - The payload, for example `{user_id: 123}`
  // timeout - The push timeout in milliseconds
  //

  function Push(channel, event, payload, timeout) {
    _classCallCheck(this, Push);

    this.channel = channel;
    this.event = event;
    this.payload = payload || {};
    this.receivedResp = null;
    this.timeout = timeout;
    this.timeoutTimer = null;
    this.recHooks = [];
    this.sent = false;
  }

  _createClass(Push, {
    resend: {
      value: function resend(timeout) {
        this.timeout = timeout;
        this.cancelRefEvent();
        this.ref = null;
        this.refEvent = null;
        this.receivedResp = null;
        this.sent = false;
        this.send();
      }
    },
    send: {
      value: function send() {
        if (this.hasReceived("timeout")) {
          return;
        }
        this.startTimeout();
        this.sent = true;
        this.channel.socket.push({
          topic: this.channel.topic,
          event: this.event,
          payload: this.payload,
          ref: this.ref
        });
      }
    },
    receive: {
      value: function receive(status, callback) {
        if (this.hasReceived(status)) {
          callback(this.receivedResp.response);
        }

        this.recHooks.push({ status: status, callback: callback });
        return this;
      }
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
      }
    },
    cancelRefEvent: {
      value: function cancelRefEvent() {
        if (!this.refEvent) {
          return;
        }
        this.channel.off(this.refEvent);
      }
    },
    cancelTimeout: {
      value: function cancelTimeout() {
        clearTimeout(this.timeoutTimer);
        this.timeoutTimer = null;
      }
    },
    startTimeout: {
      value: function startTimeout() {
        var _this = this;

        if (this.timeoutTimer) {
          return;
        }
        this.ref = this.channel.socket.makeRef();
        this.refEvent = this.channel.replyEventName(this.ref);

        this.channel.on(this.refEvent, function (payload) {
          _this.cancelRefEvent();
          _this.cancelTimeout();
          _this.receivedResp = payload;
          _this.matchReceive(payload);
        });

        this.timeoutTimer = setTimeout(function () {
          _this.trigger("timeout", {});
        }, this.timeout);
      }
    },
    hasReceived: {
      value: function hasReceived(status) {
        return this.receivedResp && this.receivedResp.status === status;
      }
    },
    trigger: {
      value: function trigger(status, response) {
        this.channel.trigger(this.refEvent, { status: status, response: response });
      }
    }
  });

  return Push;
})();

var Channel = exports.Channel = (function () {
  function Channel(topic, params, socket) {
    var _this = this;

    _classCallCheck(this, Channel);

    this.state = CHANNEL_STATES.closed;
    this.topic = topic;
    this.params = params || {};
    this.socket = socket;
    this.bindings = [];
    this.timeout = this.socket.timeout;
    this.joinedOnce = false;
    this.joinPush = new Push(this, CHANNEL_EVENTS.join, this.params, this.timeout);
    this.pushBuffer = [];
    this.rejoinTimer = new Timer(function () {
      return _this.rejoinUntilConnected();
    }, this.socket.reconnectAfterMs);
    this.joinPush.receive("ok", function () {
      _this.state = CHANNEL_STATES.joined;
      _this.rejoinTimer.reset();
      _this.pushBuffer.forEach(function (pushEvent) {
        return pushEvent.send();
      });
      _this.pushBuffer = [];
    });
    this.onClose(function () {
      _this.socket.log("channel", "close " + _this.topic + " " + _this.joinRef());
      _this.state = CHANNEL_STATES.closed;
      _this.socket.remove(_this);
    });
    this.onError(function (reason) {
      _this.socket.log("channel", "error " + _this.topic, reason);
      _this.state = CHANNEL_STATES.errored;
      _this.rejoinTimer.scheduleTimeout();
    });
    this.joinPush.receive("timeout", function () {
      if (_this.state !== CHANNEL_STATES.joining) {
        return;
      }

      _this.socket.log("channel", "timeout " + _this.topic, _this.joinPush.timeout);
      _this.state = CHANNEL_STATES.errored;
      _this.rejoinTimer.scheduleTimeout();
    });
    this.on(CHANNEL_EVENTS.reply, function (payload, ref) {
      _this.trigger(_this.replyEventName(ref), payload);
    });
  }

  _createClass(Channel, {
    rejoinUntilConnected: {
      value: function rejoinUntilConnected() {
        this.rejoinTimer.scheduleTimeout();
        if (this.socket.isConnected()) {
          this.rejoin();
        }
      }
    },
    join: {
      value: function join() {
        var timeout = arguments[0] === undefined ? this.timeout : arguments[0];

        if (this.joinedOnce) {
          throw "tried to join multiple times. 'join' can only be called a single time per channel instance";
        } else {
          this.joinedOnce = true;
          this.rejoin(timeout);
          return this.joinPush;
        }
      }
    },
    onClose: {
      value: function onClose(callback) {
        this.on(CHANNEL_EVENTS.close, callback);
      }
    },
    onError: {
      value: function onError(callback) {
        this.on(CHANNEL_EVENTS.error, function (reason) {
          return callback(reason);
        });
      }
    },
    on: {
      value: function on(event, callback) {
        this.bindings.push({ event: event, callback: callback });
      }
    },
    off: {
      value: function off(event) {
        this.bindings = this.bindings.filter(function (bind) {
          return bind.event !== event;
        });
      }
    },
    canPush: {
      value: function canPush() {
        return this.socket.isConnected() && this.state === CHANNEL_STATES.joined;
      }
    },
    push: {
      value: function push(event, payload) {
        var timeout = arguments[2] === undefined ? this.timeout : arguments[2];

        if (!this.joinedOnce) {
          throw "tried to push '" + event + "' to '" + this.topic + "' before joining. Use channel.join() before pushing events";
        }
        var pushEvent = new Push(this, event, payload, timeout);
        if (this.canPush()) {
          pushEvent.send();
        } else {
          pushEvent.startTimeout();
          this.pushBuffer.push(pushEvent);
        }

        return pushEvent;
      }
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
      //     channel.leave().receive("ok", () => alert("left!") )
      //

      value: function leave() {
        var _this = this;

        var timeout = arguments[0] === undefined ? this.timeout : arguments[0];

        this.state = CHANNEL_STATES.leaving;
        var onClose = function () {
          _this.socket.log("channel", "leave " + _this.topic);
          _this.trigger(CHANNEL_EVENTS.close, "leave", _this.joinRef());
        };
        var leavePush = new Push(this, CHANNEL_EVENTS.leave, {}, timeout);
        leavePush.receive("ok", function () {
          return onClose();
        }).receive("timeout", function () {
          return onClose();
        });
        leavePush.send();
        if (!this.canPush()) {
          leavePush.trigger("ok", {});
        }

        return leavePush;
      }
    },
    onMessage: {

      // Overridable message hook
      //
      // Receives all events for specialized message handling

      value: function onMessage(event, payload, ref) {}
    },
    isMember: {

      // private

      value: function isMember(topic) {
        return this.topic === topic;
      }
    },
    joinRef: {
      value: function joinRef() {
        return this.joinPush.ref;
      }
    },
    sendJoin: {
      value: function sendJoin(timeout) {
        this.state = CHANNEL_STATES.joining;
        this.joinPush.resend(timeout);
      }
    },
    rejoin: {
      value: function rejoin() {
        var timeout = arguments[0] === undefined ? this.timeout : arguments[0];
        if (this.state === CHANNEL_STATES.leaving) {
          return;
        }
        this.sendJoin(timeout);
      }
    },
    trigger: {
      value: function trigger(event, payload, ref) {
        var close = CHANNEL_EVENTS.close;
        var error = CHANNEL_EVENTS.error;
        var leave = CHANNEL_EVENTS.leave;
        var join = CHANNEL_EVENTS.join;

        if (ref && [close, error, leave, join].indexOf(event) >= 0 && ref !== this.joinRef()) {
          return;
        }
        this.onMessage(event, payload, ref);
        this.bindings.filter(function (bind) {
          return bind.event === event;
        }).map(function (bind) {
          return bind.callback(payload, ref);
        });
      }
    },
    replyEventName: {
      value: function replyEventName(ref) {
        return "chan_reply_" + ref;
      }
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
  //   transport - The Websocket Transport, for example WebSocket or Phoenix.LongPoll.
  //               Defaults to WebSocket with automatic LongPoll fallback.
  //   timeout - The default timeout in milliseconds to trigger push timeouts.
  //             Defaults `DEFAULT_TIMEOUT`
  //   heartbeatIntervalMs - The millisec interval to send a heartbeat message
  //   reconnectAfterMs - The optional function that returns the millsec
  //                      reconnect interval. Defaults to stepped backoff of:
  //
  //     function(tries){
  //       return [1000, 5000, 10000][tries - 1] || 10000
  //     }
  //
  //   logger - The optional function for specialized logging, ie:
  //     `logger: (kind, msg, data) => { console.log(`${kind}: ${msg}`, data) }
  //
  //   longpollerTimeout - The maximum timeout of a long poll AJAX request.
  //                        Defaults to 20s (double the server long poll timer).
  //
  //   params - The optional params to pass when connecting
  //
  // For IE8 support use an ES5-shim (https://github.com/es-shims/es5-shim)
  //

  function Socket(endPoint) {
    var _this = this;

    var opts = arguments[1] === undefined ? {} : arguments[1];

    _classCallCheck(this, Socket);

    this.stateChangeCallbacks = { open: [], close: [], error: [], message: [] };
    this.channels = [];
    this.sendBuffer = [];
    this.ref = 0;
    this.timeout = opts.timeout || DEFAULT_TIMEOUT;
    this.transport = opts.transport || window.WebSocket || LongPoll;
    this.heartbeatIntervalMs = opts.heartbeatIntervalMs || 30000;
    this.reconnectAfterMs = opts.reconnectAfterMs || function (tries) {
      return [1000, 2000, 5000, 10000][tries - 1] || 10000;
    };
    this.logger = opts.logger || function () {}; // noop
    this.longpollerTimeout = opts.longpollerTimeout || 20000;
    this.params = opts.params || {};
    this.endPoint = "" + endPoint + "/" + TRANSPORTS.websocket;
    this.reconnectTimer = new Timer(function () {
      _this.disconnect(function () {
        return _this.connect();
      });
    }, this.reconnectAfterMs);
  }

  _createClass(Socket, {
    protocol: {
      value: function protocol() {
        return location.protocol.match(/^https/) ? "wss" : "ws";
      }
    },
    endPointURL: {
      value: function endPointURL() {
        var uri = Ajax.appendParams(Ajax.appendParams(this.endPoint, this.params), { vsn: VSN });
        if (uri.charAt(0) !== "/") {
          return uri;
        }
        if (uri.charAt(1) === "/") {
          return "" + this.protocol() + ":" + uri;
        }

        return "" + this.protocol() + "://" + location.host + "" + uri;
      }
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
      }
    },
    connect: {

      // params - The params to send when connecting, for example `{user_id: userToken}`

      value: function connect(params) {
        var _this = this;

        if (params) {
          console && console.log("passing params to connect is deprecated. Instead pass :params to the Socket constructor");
          this.params = params;
        }
        if (this.conn) {
          return;
        }

        this.conn = new this.transport(this.endPointURL());
        this.conn.timeout = this.longpollerTimeout;
        this.conn.onopen = function () {
          return _this.onConnOpen();
        };
        this.conn.onerror = function (error) {
          return _this.onConnError(error);
        };
        this.conn.onmessage = function (event) {
          return _this.onConnMessage(event);
        };
        this.conn.onclose = function (event) {
          return _this.onConnClose(event);
        };
      }
    },
    log: {

      // Logs the message. Override `this.logger` for specialized logging. noops by default

      value: function log(kind, msg, data) {
        this.logger(kind, msg, data);
      }
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
      }
    },
    onClose: {
      value: function onClose(callback) {
        this.stateChangeCallbacks.close.push(callback);
      }
    },
    onError: {
      value: function onError(callback) {
        this.stateChangeCallbacks.error.push(callback);
      }
    },
    onMessage: {
      value: function onMessage(callback) {
        this.stateChangeCallbacks.message.push(callback);
      }
    },
    onConnOpen: {
      value: function onConnOpen() {
        var _this = this;

        this.log("transport", "connected to " + this.endPointURL(), this.transport.prototype);
        this.flushSendBuffer();
        this.reconnectTimer.reset();
        if (!this.conn.skipHeartbeat) {
          clearInterval(this.heartbeatTimer);
          this.heartbeatTimer = setInterval(function () {
            return _this.sendHeartbeat();
          }, this.heartbeatIntervalMs);
        }
        this.stateChangeCallbacks.open.forEach(function (callback) {
          return callback();
        });
      }
    },
    onConnClose: {
      value: function onConnClose(event) {
        this.log("transport", "close", event);
        this.triggerChanError();
        clearInterval(this.heartbeatTimer);
        this.reconnectTimer.scheduleTimeout();
        this.stateChangeCallbacks.close.forEach(function (callback) {
          return callback(event);
        });
      }
    },
    onConnError: {
      value: function onConnError(error) {
        this.log("transport", error);
        this.triggerChanError();
        this.stateChangeCallbacks.error.forEach(function (callback) {
          return callback(error);
        });
      }
    },
    triggerChanError: {
      value: function triggerChanError() {
        this.channels.forEach(function (channel) {
          return channel.trigger(CHANNEL_EVENTS.error);
        });
      }
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
      }
    },
    isConnected: {
      value: function isConnected() {
        return this.connectionState() === "open";
      }
    },
    remove: {
      value: function remove(channel) {
        this.channels = this.channels.filter(function (c) {
          return c.joinRef() !== channel.joinRef();
        });
      }
    },
    channel: {
      value: function channel(topic) {
        var chanParams = arguments[1] === undefined ? {} : arguments[1];

        var chan = new Channel(topic, chanParams, this);
        this.channels.push(chan);
        return chan;
      }
    },
    push: {
      value: function push(data) {
        var _this = this;

        var topic = data.topic;
        var event = data.event;
        var payload = data.payload;
        var ref = data.ref;

        var callback = function () {
          return _this.conn.send(JSON.stringify(data));
        };
        this.log("push", "" + topic + " " + event + " (" + ref + ")", payload);
        if (this.isConnected()) {
          callback();
        } else {
          this.sendBuffer.push(callback);
        }
      }
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
      }
    },
    sendHeartbeat: {
      value: function sendHeartbeat() {
        if (!this.isConnected()) {
          return;
        }
        this.push({ topic: "phoenix", event: "heartbeat", payload: {}, ref: this.makeRef() });
      }
    },
    flushSendBuffer: {
      value: function flushSendBuffer() {
        if (this.isConnected() && this.sendBuffer.length > 0) {
          this.sendBuffer.forEach(function (callback) {
            return callback();
          });
          this.sendBuffer = [];
        }
      }
    },
    onConnMessage: {
      value: function onConnMessage(rawMessage) {
        var msg = JSON.parse(rawMessage.data);
        var topic = msg.topic;
        var event = msg.event;
        var payload = msg.payload;
        var ref = msg.ref;

        this.log("receive", "" + (payload.status || "") + " " + topic + " " + event + " " + (ref && "(" + ref + ")" || ""), payload);
        this.channels.filter(function (channel) {
          return channel.isMember(topic);
        }).forEach(function (channel) {
          return channel.trigger(event, payload, ref);
        });
        this.stateChangeCallbacks.message.forEach(function (callback) {
          return callback(msg);
        });
      }
    }
  });

  return Socket;
})();

var LongPoll = exports.LongPoll = (function () {
  function LongPoll(endPoint) {
    _classCallCheck(this, LongPoll);

    this.endPoint = null;
    this.token = null;
    this.skipHeartbeat = true;
    this.onopen = function () {}; // noop
    this.onerror = function () {}; // noop
    this.onmessage = function () {}; // noop
    this.onclose = function () {}; // noop
    this.pollEndpoint = this.normalizeEndpoint(endPoint);
    this.readyState = SOCKET_STATES.connecting;

    this.poll();
  }

  _createClass(LongPoll, {
    normalizeEndpoint: {
      value: function normalizeEndpoint(endPoint) {
        return endPoint.replace("ws://", "http://").replace("wss://", "https://").replace(new RegExp("(.*)/" + TRANSPORTS.websocket), "$1/" + TRANSPORTS.longpoll);
      }
    },
    endpointURL: {
      value: function endpointURL() {
        return Ajax.appendParams(this.pollEndpoint, { token: this.token });
      }
    },
    closeAndRetry: {
      value: function closeAndRetry() {
        this.close();
        this.readyState = SOCKET_STATES.connecting;
      }
    },
    ontimeout: {
      value: function ontimeout() {
        this.onerror("timeout");
        this.closeAndRetry();
      }
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
            var messages = resp.messages;

            _this.token = token;
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
      }
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
      }
    },
    close: {
      value: function close(code, reason) {
        this.readyState = SOCKET_STATES.closed;
        this.onclose();
      }
    }
  });

  return LongPoll;
})();

var Ajax = exports.Ajax = (function () {
  function Ajax() {
    _classCallCheck(this, Ajax);
  }

  _createClass(Ajax, null, {
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
      }
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
      }
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
      }
    },
    parseJSON: {
      value: function parseJSON(resp) {
        return resp && resp !== "" ? JSON.parse(resp) : null;
      }
    },
    serialize: {
      value: function serialize(obj, parentKey) {
        var queryStr = [];
        for (var key in obj) {
          if (!obj.hasOwnProperty(key)) {
            continue;
          }
          var paramKey = parentKey ? "" + parentKey + "[" + key + "]" : key;
          var paramVal = obj[key];
          if (typeof paramVal === "object") {
            queryStr.push(this.serialize(paramVal, paramKey));
          } else {
            queryStr.push(encodeURIComponent(paramKey) + "=" + encodeURIComponent(paramVal));
          }
        }
        return queryStr.join("&");
      }
    },
    appendParams: {
      value: function appendParams(url, params) {
        if (Object.keys(params).length === 0) {
          return url;
        }

        var prefix = url.match(/\?/) ? "&" : "?";
        return "" + url + "" + prefix + "" + this.serialize(params);
      }
    }
  });

  return Ajax;
})();

Ajax.states = { complete: 4 };

var Presence = {

  syncState: function syncState(state, newState, onJoin, onLeave) {
    var _this = this;

    var joins = {};
    var leaves = {};

    this.map(state, function (key, presence) {
      if (!newState[key]) {
        leaves[key] = _this.clone(presence);
      }
    });
    this.map(newState, function (key, newPresence) {
      var currentPresence = state[key];
      if (currentPresence) {
        (function () {
          var newRefs = newPresence.metas.map(function (m) {
            return m.phx_ref;
          });
          var curRefs = currentPresence.metas.map(function (m) {
            return m.phx_ref;
          });
          var joinedMetas = newPresence.metas.filter(function (m) {
            return curRefs.indexOf(m.phx_ref) < 0;
          });
          var leftMetas = currentPresence.metas.filter(function (m) {
            return newRefs.indexOf(m.phx_ref) < 0;
          });
          if (joinedMetas.length > 0) {
            joins[key] = newPresence;
            joins[key].metas = joinedMetas;
          }
          if (leftMetas.length > 0) {
            leaves[key] = _this.clone(currentPresence);
            leaves[key].metas = leftMetas;
          }
        })();
      } else {
        joins[key] = newPresence;
      }
    });
    this.syncDiff(state, { joins: joins, leaves: leaves }, onJoin, onLeave);
  },

  syncDiff: function syncDiff(state, _ref, onJoin, onLeave) {
    var joins = _ref.joins;
    var leaves = _ref.leaves;

    if (!onJoin) {
      onJoin = function () {};
    }
    if (!onLeave) {
      onLeave = function () {};
    }

    this.map(joins, function (key, newPresence) {
      var currentPresence = state[key];
      state[key] = newPresence;
      if (currentPresence) {
        var _state$key$metas;

        (_state$key$metas = state[key].metas).unshift.apply(_state$key$metas, _toConsumableArray(currentPresence.metas));
      }
      onJoin(key, currentPresence, newPresence);
    });
    this.map(leaves, function (key, leftPresence) {
      var currentPresence = state[key];
      if (!currentPresence) {
        return;
      }
      var refsToRemove = leftPresence.metas.map(function (m) {
        return m.phx_ref;
      });
      currentPresence.metas = currentPresence.metas.filter(function (p) {
        return refsToRemove.indexOf(p.phx_ref) < 0;
      });
      onLeave(key, currentPresence, leftPresence);
      if (currentPresence.metas.length === 0) {
        delete state[key];
      }
    });
  },

  list: function list(presences, chooser) {
    if (!chooser) {
      chooser = function (key, pres) {
        return pres;
      };
    }

    return this.map(presences, function (key, presence) {
      return chooser(key, presence);
    });
  },

  // private

  map: function map(obj, func) {
    return Object.getOwnPropertyNames(obj).map(function (key) {
      return func(key, obj[key]);
    });
  },

  clone: function clone(obj) {
    return JSON.parse(JSON.stringify(obj));
  }
};

exports.Presence = Presence;
// Creates a timer that accepts a `timerCalc` function to perform
// calculated timeout retries, such as exponential backoff.
//
// ## Examples
//
//    let reconnectTimer = new Timer(() => this.connect(), function(tries){
//      return [1000, 5000, 10000][tries - 1] || 10000
//    })
//    reconnectTimer.scheduleTimeout() // fires after 1000
//    reconnectTimer.scheduleTimeout() // fires after 5000
//    reconnectTimer.reset()
//    reconnectTimer.scheduleTimeout() // fires after 1000
//

var Timer = (function () {
  function Timer(callback, timerCalc) {
    _classCallCheck(this, Timer);

    this.callback = callback;
    this.timerCalc = timerCalc;
    this.timer = null;
    this.tries = 0;
  }

  _createClass(Timer, {
    reset: {
      value: function reset() {
        this.tries = 0;
        clearTimeout(this.timer);
      }
    },
    scheduleTimeout: {

      // Cancels any previous scheduleTimeout and schedules callback

      value: function scheduleTimeout() {
        var _this = this;

        clearTimeout(this.timer);

        this.timer = setTimeout(function () {
          _this.tries = _this.tries + 1;
          _this.callback();
        }, this.timerCalc(this.tries + 1));
      }
    }
  });

  return Timer;
})();

Object.defineProperty(exports, "__esModule", {
  value: true
});});

;require.register("deps/phoenix_html/web/static/js/phoenix_html", function(exports, require, module) {
"use strict";

// Although ^=parent is not technically correct,
// we need to use it in order to get IE8 support.
var elements = document.querySelectorAll("[data-submit^=parent]");
var len = elements.length;

for (var i = 0; i < len; ++i) {
  elements[i].addEventListener("click", function (event) {
    var message = this.getAttribute("data-confirm");
    if (message === null || confirm(message)) {
      this.parentNode.submit();
    };
    event.preventDefault();
    return false;
  }, false);
}});

;require.register("web/static/js/app", function(exports, require, module) {
"use strict";

require("deps/phoenix_html/web/static/js/phoenix_html");

var Socket = require("deps/phoenix/web/static/js/phoenix").Socket;

var pruneBackscroll, addToScroll, adjustScrollTop, clearScroll, command_history, disableField, focus, history_marker, setFocus, updateRoom, socket, push;
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

var socket = new Socket("" + window.location.origin.replace("http", "ws") + "/ws");
socket.connect();
var chan = socket.channel("mud:play", { spirit: spiritID });

chan.join().receive("error", function (_ref) {
  var reason = _ref.reason;
  return window.location = "" + window.location.origin;
});

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
  $("#prompt").html(message.html);
});

chan.on("update room essence", function (message) {
  console.log("updating room essence");
  $(".room-" + message.room_id + "-default").text(message["default"]);
  $(".room-" + message.room_id + "-good").text(message.good);
  $(".room-" + message.room_id + "-evil").text(message.evil);
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

pruneBackscroll = function () {
  var backscroll_size = 5000;

  if ($("#scroll").children().length > backscroll_size) {
    $("#scroll").children().first().remove();
    $("#scroll").children().first().remove();
  };
};

addToScroll = function (elem, text) {
  $(elem).append(text);
  pruneBackscroll();
  return adjustScrollTop();
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
    clearScroll();
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
    setFocus("#command").select();
    addToScroll($("#scroll"), "<p>" + $("#prompt").html() + " <span class='dark-yellow'>" + command + "</span></p>");
    return push(event.target.id, command);
  } else if (event.which === 38) {
    return command_history("up");
  } else if (event.which === 40) {
    return command_history("down");
  }
});});

require.register("web/static/js/index", function(exports, require, module) {
"use strict";

var Socket = require("deps/phoenix/web/static/js/phoenix").Socket;

var update_war_status = function update_war_status(war_status) {
  $("#war-status").html(war_status.stats.join("\n    "));
};

var socket = new Socket("" + window.location.origin.replace("http", "ws") + "/ws");
socket.connect();
var chan = socket.channel("index", {});

chan.join().receive("ok", function (message) {
  update_war_status(message);
});

chan.on("war-status", function (message) {
  update_war_status(message);
});});

require.register("web/static/js/jquery-linedtextarea", function(exports, require, module) {
"use strict";

/**
 * jQuery Lined Textarea Plugin 
 *   http://alan.blog-city.com/jquerylinedtextarea.htm
 *
 * Copyright (c) 2010 Alan Williamson
 * 
 * Contributions done by Ryan Zielke (NeoAlchemy@gmail.com)
 *
 * Version: 
 *    $Id: jquery-linedtextarea.js 464 2010-01-08 10:36:33Z alan $
 *
 * Released under the MIT License:
 *    http://www.opensource.org/licenses/mit-license.php
 * 
 * Usage:
 *   Displays a line number count column to the left of the textarea
 *   
 *   Class up your textarea with a given class, or target it directly
 *   with JQuery Selectors
 *   
 *   $(".lined").linedtextarea({
 *   	selectedLine: 10,
 *    selectedClass: 'lineselect'
 *   });
 *
 * History:
 *   - 2011.12.08: Changes to allow resizing and not affect styling of the outer div
 *   - 2010.01.08: Fixed a Google Chrome layout problem
 *   - 2010.01.07: Refactored code for speed/readability; Fixed horizontal sizing
 *   - 2010.01.06: Initial Release
 *
 */
(function ($) {

	$.fn.linedtextarea = function (options) {

		// Get the Options
		var opts = $.extend({}, $.fn.linedtextarea.defaults, options);

		/*
   * Helper function to make sure the line numbers are always
   * kept up to the current system
   */
		var fillOutLines = function fillOutLines(codeLines, h, lineNo) {
			while (codeLines.height() - h <= 0) {
				if (lineNo == opts.selectedLine) codeLines.append("<div class='lineno lineselect'>" + lineNo + "</div>");else codeLines.append("<div class='lineno'>" + lineNo + "</div>");

				lineNo++;
			}
			return lineNo;
		};

		/*
   * Iterate through each of the elements are to be applied to
   */
		return this.each(function () {
			var lineNo = 1;
			var textarea = $(this);

			/* Turn off the wrapping of as we don't want to screw up the line numbers */
			textarea.attr("wrap", "off");
			textarea.css({ resize: "both" });
			var originalTextAreaWidth = textarea.outerWidth();

			/* Wrap the text area in the elements we need */
			var linedTextAreaDiv = textarea.wrap("<div class='linedwrap'></div>");
			var linedWrapDiv = linedTextAreaDiv.parent();

			linedWrapDiv.prepend("<div class='lines' style='width:50px'></div>");

			var linesDiv = linedWrapDiv.find(".lines");

			/* Draw the number bar; filling it out where necessary */
			linesDiv.append("<div class='codelines'></div>");
			var codeLinesDiv = linesDiv.find(".codelines");
			lineNo = fillOutLines(codeLinesDiv, linesDiv.height(), 1);

			/* Move the textarea to the selected line */
			if (opts.selectedLine != -1 && !isNaN(opts.selectedLine)) {
				var fontSize = parseInt(textarea.height() / (lineNo - 2));
				var position = parseInt(fontSize * opts.selectedLine) - textarea.height() / 2;
				textarea[0].scrollTop = position;
			}

			/* Set the width */
			var sidebarWidth = linesDiv.outerWidth(true);
			var paddingHorizontal = parseInt(linedWrapDiv.css("border-left-width")) + parseInt(linedWrapDiv.css("border-right-width")) + parseInt(linedWrapDiv.css("padding-left")) + parseInt(linedWrapDiv.css("padding-right"));
			var linedWrapDivNewWidth = originalTextAreaWidth - paddingHorizontal;
			var textareaNewWidth = originalTextAreaWidth - sidebarWidth - paddingHorizontal;

			textarea.width(textareaNewWidth);
			textarea.css({ maxWidth: textareaNewWidth - 6 }); //TODO make this calculated

			/* React to the scroll event */
			textarea.scroll(function (tn) {
				var domTextArea = $(this)[0];
				var scrollTop = domTextArea.scrollTop;
				var clientHeight = domTextArea.clientHeight;
				codeLinesDiv.css({ "margin-top": -1 * scrollTop + "px" });
				lineNo = fillOutLines(codeLinesDiv, scrollTop + clientHeight, lineNo);
			});

			/* Should the textarea get resized outside of our control */
			textarea.resize(function (tn) {
				var domTextArea = $(this)[0];
				linesDiv.height(domTextArea.clientHeight + 6);
			});

			window.setInterval(function (tn) {
				linesDiv.height(textarea.height());
				var scrollTop = textarea[0].scrollTop;
				var clientHeight = textarea[0].clientHeight;
				codeLinesDiv.css({ "margin-top": -1 * scrollTop + "px" });
				lineNo = fillOutLines(codeLinesDiv, scrollTop + clientHeight, lineNo);
			}, 10);
		});
	};

	// default options
	$.fn.linedtextarea.defaults = {
		selectedLine: -1,
		selectedClass: "lineselect"
	};
})(jQuery);});

require.register("web/static/js/json-edit", function(exports, require, module) {
"use strict";

var formatJson = require("web/static/js/json-format").formatJson;

$(".json").linedtextarea();

$("textarea.json").each(function (index, textarea) {

  reformat(textarea);
});

$("textarea.json").on("change", function () {
  validate($(this).context);
});

function reformat(field) {
  field.value = formatJson(field.value);
}

function validate(textarea) {
  try {
    var result = parser.parse(textarea.value);
    if (result) {
      reformat(textarea);
    }
  } catch (e) {
    alert("Invalid JSON in: " + textarea.name + ":\n\n" + e);
  }
}

$(document).on("keyup", "#search", function (event) {
  if (event.which === 13) {
    var query = $("#search").val();
    window.location = "" + window.location.origin + window.location.pathname + "?q=" + query;
  }
});});

require.register("web/static/js/json-format", function(exports, require, module) {
"use strict";

exports.formatJson = formatJson;
/**
 * jsl.format - Provide json reformatting in a character-by-character approach, so that even invalid JSON may be reformatted (to the best of its ability).
 *
**/
function repeat(s, count) {
    return new Array(count + 1).join(s);
}

function formatJson(json) {
    var i = 0,
        il = 0,
        tab = "  ",
        newJson = "",
        indentLevel = 0,
        inString = false,
        currentChar = null;

    for (i = 0, il = json.length; i < il; i += 1) {
        currentChar = json.charAt(i);

        switch (currentChar) {
            case "{":
            case "[":
                if (!inString) {
                    newJson += currentChar + "\n" + repeat(tab, indentLevel + 1);
                    indentLevel += 1;
                } else {
                    newJson += currentChar;
                }
                break;
            case "}":
            case "]":
                if (!inString) {
                    indentLevel -= 1;
                    newJson += "\n" + repeat(tab, indentLevel) + currentChar;
                } else {
                    newJson += currentChar;
                }
                break;
            case ",":
                if (!inString) {
                    newJson += ",\n" + repeat(tab, indentLevel);
                } else {
                    newJson += currentChar;
                }
                break;
            case ":":
                if (!inString) {
                    newJson += ": ";
                } else {
                    newJson += currentChar;
                }
                break;
            case " ":
            case "\n":
            case "\t":
                if (inString) {
                    newJson += currentChar;
                }
                break;
            case "\"":
                if (i > 0 && json.charAt(i - 1) !== "\\") {
                    inString = !inString;
                }
                newJson += currentChar;
                break;
            default:
                newJson += currentChar;
                break;
        }
    }

    return newJson;
}

Object.defineProperty(exports, "__esModule", {
    value: true
});});

;require.register("web/static/js/json-parser", function(exports, require, module) {
"use strict";

window.parser = (function () {
    var a = !0,
        b = !1,
        c = {},
        d = (function () {
        var a = {
            trace: function trace() {},
            yy: {},
            symbols_: {
                error: 2,
                JSONString: 3,
                STRING: 4,
                JSONNumber: 5,
                NUMBER: 6,
                JSONNullLiteral: 7,
                NULL: 8,
                JSONBooleanLiteral: 9,
                TRUE: 10,
                FALSE: 11,
                JSONText: 12,
                JSONObject: 13,
                EOF: 14,
                JSONArray: 15,
                JSONValue: 16,
                "{": 17,
                "}": 18,
                JSONMemberList: 19,
                JSONMember: 20,
                ":": 21,
                ",": 22,
                "[": 23,
                "]": 24,
                JSONElementList: 25,
                $accept: 0,
                $end: 1
            },
            terminals_: {
                2: "error",
                4: "STRING",
                6: "NUMBER",
                8: "NULL",
                10: "TRUE",
                11: "FALSE",
                14: "EOF",
                17: "{",
                18: "}",
                21: ":",
                22: ",",
                23: "[",
                24: "]"
            },
            productions_: [0, [3, 1], [5, 1], [7, 1], [9, 1], [9, 1], [12, 2], [12, 2], [16, 1], [16, 1], [16, 1], [16, 1], [16, 1], [16, 1], [13, 2], [13, 3], [20, 3], [19, 1], [19, 3], [15, 2], [15, 3], [25, 1], [25, 3]],
            performAction: function performAction(a, b, c, d, e, f, g) {
                var h = f.length - 1;
                switch (e) {
                    case 1:
                        this.$ = a;
                        break;
                    case 2:
                        this.$ = Number(a);
                        break;
                    case 3:
                        this.$ = null;
                        break;
                    case 4:
                        this.$ = !0;
                        break;
                    case 5:
                        this.$ = !1;
                        break;
                    case 6:
                        return this.$ = f[h - 1];
                    case 7:
                        return this.$ = f[h - 1];
                    case 8:
                        this.$ = f[h];
                        break;
                    case 9:
                        this.$ = f[h];
                        break;
                    case 10:
                        this.$ = f[h];
                        break;
                    case 11:
                        this.$ = f[h];
                        break;
                    case 12:
                        this.$ = f[h];
                        break;
                    case 13:
                        this.$ = f[h];
                        break;
                    case 14:
                        this.$ = {};
                        break;
                    case 15:
                        this.$ = f[h - 1];
                        break;
                    case 16:
                        this.$ = [f[h - 2], f[h]];
                        break;
                    case 17:
                        this.$ = {}, this.$[f[h][0]] = f[h][1];
                        break;
                    case 18:
                        this.$ = f[h - 2], f[h - 2][f[h][0]] = f[h][1];
                        break;
                    case 19:
                        this.$ = [];
                        break;
                    case 20:
                        this.$ = f[h - 1];
                        break;
                    case 21:
                        this.$ = [f[h]];
                        break;
                    case 22:
                        this.$ = f[h - 2], f[h - 2].push(f[h]);
                }
            },
            table: [{
                12: 1,
                13: 2,
                15: 3,
                17: [1, 4],
                23: [1, 5]
            }, {
                1: [3]
            }, {
                14: [1, 6]
            }, {
                14: [1, 7]
            }, {
                3: 11,
                4: [1, 12],
                18: [1, 8],
                19: 9,
                20: 10
            }, {
                3: 18,
                4: [1, 12],
                5: 19,
                6: [1, 25],
                7: 16,
                8: [1, 22],
                9: 17,
                10: [1, 23],
                11: [1, 24],
                13: 20,
                15: 21,
                16: 15,
                17: [1, 4],
                23: [1, 5],
                24: [1, 13],
                25: 14
            }, {
                1: [2, 6]
            }, {
                1: [2, 7]
            }, {
                14: [2, 14],
                18: [2, 14],
                22: [2, 14],
                24: [2, 14]
            }, {
                18: [1, 26],
                22: [1, 27]
            }, {
                18: [2, 17],
                22: [2, 17]
            }, {
                21: [1, 28]
            }, {
                18: [2, 1],
                21: [2, 1],
                22: [2, 1],
                24: [2, 1]
            }, {
                14: [2, 19],
                18: [2, 19],
                22: [2, 19],
                24: [2, 19]
            }, {
                22: [1, 30],
                24: [1, 29]
            }, {
                22: [2, 21],
                24: [2, 21]
            }, {
                18: [2, 8],
                22: [2, 8],
                24: [2, 8]
            }, {
                18: [2, 9],
                22: [2, 9],
                24: [2, 9]
            }, {
                18: [2, 10],
                22: [2, 10],
                24: [2, 10]
            }, {
                18: [2, 11],
                22: [2, 11],
                24: [2, 11]
            }, {
                18: [2, 12],
                22: [2, 12],
                24: [2, 12]
            }, {
                18: [2, 13],
                22: [2, 13],
                24: [2, 13]
            }, {
                18: [2, 3],
                22: [2, 3],
                24: [2, 3]
            }, {
                18: [2, 4],
                22: [2, 4],
                24: [2, 4]
            }, {
                18: [2, 5],
                22: [2, 5],
                24: [2, 5]
            }, {
                18: [2, 2],
                22: [2, 2],
                24: [2, 2]
            }, {
                14: [2, 15],
                18: [2, 15],
                22: [2, 15],
                24: [2, 15]
            }, {
                3: 11,
                4: [1, 12],
                20: 31
            }, {
                3: 18,
                4: [1, 12],
                5: 19,
                6: [1, 25],
                7: 16,
                8: [1, 22],
                9: 17,
                10: [1, 23],
                11: [1, 24],
                13: 20,
                15: 21,
                16: 32,
                17: [1, 4],
                23: [1, 5]
            }, {
                14: [2, 20],
                18: [2, 20],
                22: [2, 20],
                24: [2, 20]
            }, {
                3: 18,
                4: [1, 12],
                5: 19,
                6: [1, 25],
                7: 16,
                8: [1, 22],
                9: 17,
                10: [1, 23],
                11: [1, 24],
                13: 20,
                15: 21,
                16: 33,
                17: [1, 4],
                23: [1, 5]
            }, {
                18: [2, 18],
                22: [2, 18]
            }, {
                18: [2, 16],
                22: [2, 16]
            }, {
                22: [2, 22],
                24: [2, 22]
            }],
            defaultActions: {
                6: [2, 6],
                7: [2, 7]
            },
            parseError: function parseError(a, b) {
                throw new Error(a);
            },
            parse: function parse(a) {
                function o() {
                    var a;
                    a = b.lexer.lex() || 1, typeof a != "number" && (a = b.symbols_[a] || a);
                    return a;
                }

                function n(a) {
                    c.length = c.length - 2 * a, d.length = d.length - a, e.length = e.length - a;
                }
                var b = this,
                    c = [0],
                    d = [null],
                    e = [],
                    f = this.table,
                    g = "",
                    h = 0,
                    i = 0,
                    j = 0,
                    k = 2,
                    l = 1;
                this.lexer.setInput(a), this.lexer.yy = this.yy, this.yy.lexer = this.lexer, typeof this.lexer.yylloc == "undefined" && (this.lexer.yylloc = {});
                var m = this.lexer.yylloc;
                e.push(m), typeof this.yy.parseError == "function" && (this.parseError = this.yy.parseError);
                var p,
                    q,
                    r,
                    s,
                    t,
                    u,
                    v = {},
                    w,
                    x,
                    y,
                    z;
                for (;;) {
                    r = c[c.length - 1], this.defaultActions[r] ? s = this.defaultActions[r] : (p == null && (p = o()), s = f[r] && f[r][p]);
                    if (typeof s == "undefined" || !s.length || !s[0]) {
                        if (!j) {
                            z = [];
                            for (w in f[r]) this.terminals_[w] && w > 2 && z.push("'" + this.terminals_[w] + "'");
                            var A = "";
                            this.lexer.showPosition ? A = "Parse error on line " + (h + 1) + ":\n" + this.lexer.showPosition() + "\nExpecting " + z.join(", ") : A = "Parse error on line " + (h + 1) + ": Unexpected " + (p == 1 ? "end of input" : "'" + (this.terminals_[p] || p) + "'"), this.parseError(A, {
                                text: this.lexer.match,
                                token: this.terminals_[p] || p,
                                line: this.lexer.yylineno,
                                loc: m,
                                expected: z
                            });
                        }
                        if (j == 3) {
                            if (p == l) throw new Error(A || "Parsing halted.");
                            i = this.lexer.yyleng, g = this.lexer.yytext, h = this.lexer.yylineno, m = this.lexer.yylloc, p = o();
                        }
                        for (;;) {
                            if (k.toString() in f[r]) break;
                            if (r == 0) throw new Error(A || "Parsing halted.");
                            n(1), r = c[c.length - 1];
                        }
                        q = p, p = k, r = c[c.length - 1], s = f[r] && f[r][k], j = 3;
                    }
                    if (s[0] instanceof Array && s.length > 1) throw new Error("Parse Error: multiple actions possible at state: " + r + ", token: " + p);
                    switch (s[0]) {
                        case 1:
                            c.push(p), d.push(this.lexer.yytext), e.push(this.lexer.yylloc), c.push(s[1]), p = null, q ? (p = q, q = null) : (i = this.lexer.yyleng, g = this.lexer.yytext, h = this.lexer.yylineno, m = this.lexer.yylloc, j > 0 && j--);
                            break;
                        case 2:
                            x = this.productions_[s[1]][1], v.$ = d[d.length - x], v._$ = {
                                first_line: e[e.length - (x || 1)].first_line,
                                last_line: e[e.length - 1].last_line,
                                first_column: e[e.length - (x || 1)].first_column,
                                last_column: e[e.length - 1].last_column
                            }, u = this.performAction.call(v, g, i, h, this.yy, s[1], d, e);
                            if (typeof u != "undefined") {
                                return u;
                            }x && (c = c.slice(0, -1 * x * 2), d = d.slice(0, -1 * x), e = e.slice(0, -1 * x)), c.push(this.productions_[s[1]][0]), d.push(v.$), e.push(v._$), y = f[c[c.length - 2]][c[c.length - 1]], c.push(y);
                            break;
                        case 3:
                            return !0;
                    }
                }
                return !0;
            }
        },
            f = (function () {
            var a = {
                EOF: 1,
                parseError: function parseError(a, b) {
                    if (this.yy.parseError) this.yy.parseError(a, b);else throw new Error(a);
                },
                setInput: function setInput(a) {
                    this._input = a, this._more = this._less = this.done = !1, this.yylineno = this.yyleng = 0, this.yytext = this.matched = this.match = "", this.conditionStack = ["INITIAL"], this.yylloc = {
                        first_line: 1,
                        first_column: 0,
                        last_line: 1,
                        last_column: 0
                    };
                    return this;
                },
                input: function input() {
                    var a = this._input[0];
                    this.yytext += a, this.yyleng++, this.match += a, this.matched += a;
                    var b = a.match(/\n/);
                    b && this.yylineno++, this._input = this._input.slice(1);
                    return a;
                },
                unput: function unput(a) {
                    this._input = a + this._input;
                    return this;
                },
                more: function more() {
                    this._more = !0;
                    return this;
                },
                pastInput: function pastInput() {
                    var a = this.matched.substr(0, this.matched.length - this.match.length);
                    return (a.length > 20 ? "..." : "") + a.substr(-20).replace(/\n/g, "");
                },
                upcomingInput: function upcomingInput() {
                    var a = this.match;
                    a.length < 20 && (a += this._input.substr(0, 20 - a.length));
                    return (a.substr(0, 20) + (a.length > 20 ? "..." : "")).replace(/\n/g, "");
                },
                showPosition: function showPosition() {
                    var a = this.pastInput(),
                        b = Array(a.length + 1).join("-");
                    return a + this.upcomingInput() + "\n" + b + "^";
                },
                next: function next() {
                    if (this.done) {
                        return this.EOF;
                    }this._input || (this.done = !0);
                    var a, b, c, d;
                    this._more || (this.yytext = "", this.match = "");
                    var e = this._currentRules();
                    for (var f = 0; f < e.length; f++) {
                        b = this._input.match(this.rules[e[f]]);
                        if (b) {
                            d = b[0].match(/\n.*/g), d && (this.yylineno += d.length), this.yylloc = {
                                first_line: this.yylloc.last_line,
                                last_line: this.yylineno + 1,
                                first_column: this.yylloc.last_column,
                                last_column: d ? d[d.length - 1].length - 1 : this.yylloc.last_column + b[0].length
                            }, this.yytext += b[0], this.match += b[0], this.matches = b, this.yyleng = this.yytext.length, this._more = !1, this._input = this._input.slice(b[0].length), this.matched += b[0], a = this.performAction.call(this, this.yy, this, e[f], this.conditionStack[this.conditionStack.length - 1]);
                            if (a) {
                                return a;
                            }return;
                        }
                    }
                    if (this._input === "") {
                        return this.EOF;
                    }this.parseError("Lexical error on line " + (this.yylineno + 1) + ". Unrecognized text.\n" + this.showPosition(), {
                        text: "",
                        token: null,
                        line: this.yylineno
                    });
                },
                lex: function lex() {
                    var a = this.next();
                    return typeof a != "undefined" ? a : this.lex();
                },
                begin: function begin(a) {
                    this.conditionStack.push(a);
                },
                popState: function popState() {
                    return this.conditionStack.pop();
                },
                _currentRules: function _currentRules() {
                    return this.conditions[this.conditionStack[this.conditionStack.length - 1]].rules;
                }
            };
            a.performAction = function (a, b, c, d) {
                var e = d;
                switch (c) {
                    case 0:
                        break;
                    case 1:
                        return 6;
                    case 2:
                        b.yytext = b.yytext.substr(1, b.yyleng - 2);
                        return 4;
                    case 3:
                        return 17;
                    case 4:
                        return 18;
                    case 5:
                        return 23;
                    case 6:
                        return 24;
                    case 7:
                        return 22;
                    case 8:
                        return 21;
                    case 9:
                        return 10;
                    case 10:
                        return 11;
                    case 11:
                        return 8;
                    case 12:
                        return 14;
                    case 13:
                        return "INVALID";
                }
            }, a.rules = [/^\s+/, /^-?([0-9]|[1-9][0-9]+)(\.[0-9]+)?([eE][-+]?[0-9]+)?\b/, /^"(\\["bfnrt/\\]|\\u[a-fA-F0-9]{4}|[^\0-\x09\x0a-\x1f"\\])*"/, /^\{/, /^\}/, /^\[/, /^\]/, /^,/, /^:/, /^true\b/, /^false\b/, /^null\b/, /^$/, /^./], a.conditions = {
                INITIAL: {
                    rules: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13],
                    inclusive: !0
                }
            };
            return a;
        })();
        a.lexer = f;
        return a;
    })();
    typeof a != "undefined" && typeof c != "undefined" && (c.parser = d, c.parse = function () {
        return d.parse.apply(d, arguments);
    }, c.main = function (b) {
        if (!b[1]) throw new Error("Usage: " + b[0] + " FILE");
        if (typeof process != "undefined") var d = a("fs").readFileSync(a("path").join(process.cwd(), b[1]), "utf8");else var e = a("file").path(a("file").cwd()),
            d = e.join(b[1]).read({
            charset: "utf-8"
        });
        return c.parser.parse(d);
    }, typeof b != "undefined" && a.main === b && c.main(typeof process != "undefined" ? process.argv.slice(1) : a("system").args));
    return c;
})();});

;require.register("web/static/js/map", function(exports, require, module) {
"use strict";

var Socket = require("deps/phoenix/web/static/js/phoenix").Socket;

$(document).ready(function () {

  // Autodetect, create and append the renderer to the body element
  var renderer = PIXI.autoDetectRenderer(1200, 720, { backgroundColor: 0, antialias: true });
  document.body.appendChild(renderer.view);

  // Create the main stage for your display objects
  var stage = new PIXI.Container();

  var zoom = 1;

  // Initialize the pixi Graphics class
  var graphics = new PIXI.Graphics();

  // Start animating
  animate();
  function animate() {
    //Render the stage
    renderer.render(stage);
    requestAnimationFrame(animate);
  }

  var socket = new Socket("" + window.location.origin.replace("http", "ws") + "/ws");
  socket.connect();
  var chan = socket.channel("map", {});

  chan.join();

  var draw_room = function draw_room(room) {
    if (room.coords) {
      // Set a new fill color
      graphics.beginFill(0); // Blue

      graphics.lineStyle(2, 16777215, 1);

      // Draw a rectangle
      // drawRect(x, y, width, height)
      var x = room.coords.x * 32 + 500;
      var y = room.coords.y * 32 + 3000;

      var start_x;
      var start_y;
      var end_x;
      var end_y;

      graphics.drawRect(x, y, 16, 16);

      room.directions.forEach(function (direction) {
        switch (direction) {
          case "north":
            start_x = x + 8;
            start_y = y;
            end_x = x + 8;
            end_y = y + 8 - 16;
            break;
          case "northeast":
            start_x = x + 16;
            start_y = y;
            end_x = x + 8 + 16;
            end_y = y + 8 - 16;
            break;
          case "east":
            start_x = x + 16;
            start_y = y + 8;
            end_x = x + 8 + 16;
            end_y = y + 8;
            break;
          case "southeast":
            start_x = x + 16;
            start_y = y + 16;
            end_x = x + 8 + 16;
            end_y = y + 8 + 16;
            break;
          case "south":
            start_x = x + 8;
            start_y = y + 16;
            end_x = x + 8;
            end_y = y + 8 + 16;
            break;
          case "southwest":
            start_x = x;
            start_y = y + 16;
            end_x = x + 8 - 16;
            end_y = y + 8 + 16;
            break;
          case "west":
            start_x = x;
            start_y = y + 8;
            end_x = x + 8 - 16;
            end_y = y + 8;
            break;
          case "northwest":
            start_x = x;
            start_y = y;
            end_x = x + 8 - 16;
            end_y = y + 8 - 16;
            break;
        }

        graphics.moveTo(start_x, start_y);
        graphics.lineTo(end_x, end_y);
      });

      graphics.endFill();

      // Add the graphics to the stage
      stage.addChild(graphics);

      stage.scale.x = zoom;
      stage.scale.y = zoom;
    }
  };

  chan.on("update_room", function (room) {
    console.log(room);
    draw_room(room);
  });

  chan.on("presence_diff", function (message) {
    for (var key in message.joins) {
      message.joins[key].metas.forEach(function (room) {
        draw_room(room);
      });
    }
  });

  chan.on("full_map", function (world) {
    for (var room_id in world) {
      draw_room(world[room_id]);
    }
  });

  $(document).on("keyup", function (event) {
    event.preventDefault();
    if (event.which === 187) {
      zoom = zoom + 0.1;
      stage.scale.x = zoom;
      stage.scale.y = zoom;
    } else if (event.which === 189) {
      zoom = zoom - 0.1;
      stage.scale.x = zoom;
      stage.scale.y = zoom;
    } else if (event.which === 38) {
      stage.y = stage.y + 100;
    } else if (event.which === 39) {
      stage.x = stage.x - 100;
    } else if (event.which === 40) {
      stage.y = stage.y - 100;
    } else if (event.which === 37) {
      stage.x = stage.x + 100;
    }
  });
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
    $("nav a[href^=\"#\"]").on("click", smoothScroll);
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