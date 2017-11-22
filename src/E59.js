"use strict"

exports.member = function(k) {
    return function(arr) {
        return arr.includes(k)
    }
}

exports.partition = function(arr) {
    return function(bs) {
        var parts = [];
        for (var i=0; i<bs; i++) {
            parts.push([])
        }
        for (var i=0; i<arr.length; i++) {
            parts[i%bs].push(arr[i])
        }
        return parts;
    }
}