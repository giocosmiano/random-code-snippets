var expect = require('chai').expect;
require('chai').use(require('chai-as-promised'));
var countPrimesAsync = require('../src/primes');

describe('primes test to learn testing promises', function() {
  it('returning a promise that verifies', function() {
    var verifyCount = function(count) {
      expect(count).to.be.eql(25);
    };
    
    return countPrimesAsync(100)
      .then(verifyCount);
  });

  it('using eventually', function() {
    return expect(countPrimesAsync(100)).to.eventually.eql(25);
  });

  it('expect rejected', function() {
    return expect(countPrimesAsync(-1)).to.rejected;
  });

  it('expect rejected', function() {
    return expect(countPrimesAsync(-1)).to.be.rejectedWith('invalid number');
  });
});