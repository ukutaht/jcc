#!/usr/bin/env node
var esprima = require('esprima')
var fs = require('fs')

let inputfile = process.argv[2]

if (inputfile === undefined)  {
  console.log("missing input file")
  process.exit(1)
}

var data = fs.readFileSync(inputfile, { encoding: 'utf8' })

var program = esprima.parse(data, {loc: true, comment: true})

console.log(JSON.stringify(program, null, 2))
