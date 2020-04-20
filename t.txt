class A {
  var x = 6;
  var y = 7;

  function prod() {
    return this.x * this.y;
  }

  function set2(a, b) {
    x = a;
    y = b;
  }
}

class B extends A {
  function set1(a) {
    set2(a, a);
  }

  static function main () {
    var b = new B();
    b.set1(10);
    return b.prod();
  }
}