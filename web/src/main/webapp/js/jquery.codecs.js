/*
 * ELW : e-learning workspace
 * Copyright (C) 2010  Anton Kraievoy
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 *	URL encode / decode (adopted as a jQuery plugin)
 *	http://www.webtoolkit.info/
 **/
(function() {
  function _utf8_encode(string) {
    string = string.replace(/\r\n/g, "\n");
    var utftext = "";

    for (var n = 0; n < string.length; n++) {

      var c = string.charCodeAt(n);

      if (c < 128) {
        utftext += String.fromCharCode(c);
      }
      else if ((c > 127) && (c < 2048)) {
        utftext += String.fromCharCode((c >> 6) | 192);
        utftext += String.fromCharCode((c & 63) | 128);
      }
      else {
        utftext += String.fromCharCode((c >> 12) | 224);
        utftext += String.fromCharCode(((c >> 6) & 63) | 128);
        utftext += String.fromCharCode((c & 63) | 128);
      }
    }

    return utftext;
  }

  function _utf8_decode(utftext) {
    var string = "";
    var i = 0;
    var c, c1, c2;
    c = c1 = c2 = 0;

    while (i < utftext.length) {

      c = utftext.charCodeAt(i);

      if (c < 128) {
        string += String.fromCharCode(c);
        i++;
      }
      else {
        if ((c > 191) && (c < 224)) {
          c2 = utftext.charCodeAt(i + 1);
          string += String.fromCharCode(((c & 31) << 6) | (c2 & 63));
          i += 2;
        }
        else {
          c2 = utftext.charCodeAt(i + 1);
          c3 = utftext.charCodeAt(i + 2);
          string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
          i += 3;
        }
      }
    }

    return string;
  }

  var _keyStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_=";

  $.extend({
    encode64 : function (input) {
      var output = "";
      var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
      var i = 0;

      input = _utf8_encode(input);

      while (i < input.length) {

        chr1 = input.charCodeAt(i++);
        chr2 = input.charCodeAt(i++);
        chr3 = input.charCodeAt(i++);

        enc1 = chr1 >> 2;
        enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
        enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
        enc4 = chr3 & 63;

        if (isNaN(chr2)) {
          enc3 = enc4 = 64;
        } else {
          if (isNaN(chr3)) {
            enc4 = 64;
          }
        }

        output = output +
            _keyStr.charAt(enc1) + _keyStr.charAt(enc2) +
            _keyStr.charAt(enc3) + _keyStr.charAt(enc4);

      }

      return output;
    },

    // public method for decoding
    decode64 : function (input) {
      var output = "";
      var chr1, chr2, chr3;
      var enc1, enc2, enc3, enc4;
      var i = 0;

      input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");

      while (i < input.length) {

        enc1 = _keyStr.indexOf(input.charAt(i++));
        enc2 = _keyStr.indexOf(input.charAt(i++));
        enc3 = _keyStr.indexOf(input.charAt(i++));
        enc4 = _keyStr.indexOf(input.charAt(i++));

        chr1 = (enc1 << 2) | (enc2 >> 4);
        chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
        chr3 = ((enc3 & 3) << 6) | enc4;

        output = output + String.fromCharCode(chr1);

        if (enc3 != 64) {
          output = output + String.fromCharCode(chr2);
        }
        if (enc4 != 64) {
          output = output + String.fromCharCode(chr3);
        }

      }

      output = _utf8_decode(output);

      return output;
    },
    URLEncode:function(string) {
      if (typeof encodeURIComponent == 'function') {
        return encodeURIComponent(string);
      } else {
        return escape(_utf8_encode(string));	// pre-1,5ish way
      }
    },
    URLDecode:function(string) {
      if (typeof decodeURIComponent == 'function') {
        return decodeURIComponent(string);
      } else {
        return _utf8_decode(unescape(string));
      }
    }
  });
})();

/*
 * jQuery JSON Plugin
 * version: 2.1 (2009-08-14) (akraievoy: actually that was a 2.2 version)
 *
 * This document is licensed as free software under the terms of the
 * MIT License: http://www.opensource.org/licenses/mit-license.php
 *
 * Brantley Harris wrote this plugin. It is based somewhat on the JSON.org
 * website's http://www.json.org/json2.js, which proclaims:
 * "NO WARRANTY EXPRESSED OR IMPLIED. USE AT YOUR OWN RISK.", a sentiment that
 * I uphold.
 *
 * It is also influenced heavily by MochiKit's serializeJSON, which is
 * copyrighted 2005 by Bob Ippolito.
 */
(function($) {
  /** jQuery.toJSON( json-serializble )
   Converts the given argument into a JSON respresentation.

   If an object has a "toJSON" function, that will be used to get the representation.
   Non-integer/string keys are skipped in the object, as are keys that point to a function.

   json-serializble:
   The *thing* to be converted.
   **/
  $.toJSON = function(o) {
    if (typeof(JSON) == 'object' && JSON.stringify) {
      return JSON.stringify(o);
    }

    var type = typeof(o);
    if (o === null) {
      return "null";
    }

    if (type == "undefined") {
      return undefined;
    }

    if (type == "number" || type == "boolean") {
      return o + "";
    }

    if (type == "string") {
      return $.quoteString(o);
    }

    if (type == 'object') {
      if (typeof o.toJSON == "function") {
        return $.toJSON(o.toJSON());
      }

      if (o.constructor === Date) {
        var month = o.getUTCMonth() + 1;
        if (month < 10) {
          month = '0' + month;
        }

        var day = o.getUTCDate();
        if (day < 10) {
          day = '0' + day;
        }

        var year = o.getUTCFullYear();

        var hours = o.getUTCHours();
        if (hours < 10) {
          hours = '0' + hours;
        }

        var minutes = o.getUTCMinutes();
        if (minutes < 10) {
          minutes = '0' + minutes;
        }

        var seconds = o.getUTCSeconds();
        if (seconds < 10) {
          seconds = '0' + seconds;
        }

        var milli = o.getUTCMilliseconds();
        if (milli < 100) {
          milli = '0' + milli;
        }
        if (milli < 10) {
          milli = '0' + milli;
        }

        return '"' + year + '-' + month + '-' + day + 'T' +
            hours + ':' + minutes + ':' + seconds +
            '.' + milli + 'Z"';
      }

      if (o.constructor === Array) {
        var ret = [];
        for (var i = 0; i < o.length; i++) {
          ret.push($.toJSON(o[i]) || "null");
        }

        return "[" + ret.join(",") + "]";
      }

      var pairs = [];
      for (var k in o) {
        var name;
        var type = typeof k;

        if (type == "number") {
          name = '"' + k + '"';
        } else {
          if (type == "string") {
            name = $.quoteString(k);
          } else {
            continue;
          }
        }  //skip non-string or number keys

        if (typeof o[k] == "function") {
          continue;
        }  //skip pairs where the value is a function.

        var val = $.toJSON(o[k]);

        pairs.push(name + ":" + val);
      }

      return "{" + pairs.join(", ") + "}";
    }
  };

  /** jQuery.evalJSON(src)
   Evaluates a given piece of json source.
   **/
  $.evalJSON = function(src) {
    if (typeof(JSON) == 'object' && JSON.parse) {
      return JSON.parse(src);
    }
    return eval("(" + src + ")");
  };

  /** jQuery.secureEvalJSON(src)
   Evals JSON in a way that is *more* secure.
   **/
  $.secureEvalJSON = function(src) {
    if (typeof(JSON) == 'object' && JSON.parse) {
      return JSON.parse(src);
    }

    var filtered = src;
    filtered = filtered.replace(/\\["\\\/bfnrtu]/g, '@');
    filtered = filtered.replace(/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g, ']');
    filtered = filtered.replace(/(?:^|:|,)(?:\s*\[)+/g, '');

    if (/^[\],:{}\s]*$/.test(filtered)) {
      return eval("(" + src + ")");
    } else {
      throw new SyntaxError("Error parsing JSON, source is not valid.");
    }
  };

  /** jQuery.quoteString(string)
   Returns a string-repr of a string, escaping quotes intelligently.
   Mostly a support function for toJSON.

   Examples:
   >>> jQuery.quoteString("apple")
   "apple"

   >>> jQuery.quoteString('"Where are we going?", she asked.')
   "\"Where are we going?\", she asked."
   **/
  $.quoteString = function(string) {
    if (string.match(_escapeable)) {
      return '"' + string.replace(_escapeable, function (a) {
        var c = _meta[a];
        if (typeof c === 'string') {
          return c;
        }
        c = a.charCodeAt();
        return '\\u00' + Math.floor(c / 16).toString(16) + (c % 16).toString(16);
      }) + '"';
    }
    return '"' + string + '"';
  };

  var _escapeable = /["\\\x00-\x1f\x7f-\x9f]/g;

  var _meta = {
    '\b': '\\b',
    '\t': '\\t',
    '\n': '\\n',
    '\f': '\\f',
    '\r': '\\r',
    '"' : '\\"',
    '\\': '\\\\'
  };
})(jQuery);