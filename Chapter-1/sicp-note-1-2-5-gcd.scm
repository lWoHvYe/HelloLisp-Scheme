; 如果欧几里得算法需要k步计算出一对整数的GCD，那么这对数中较小的那个数必然大于或等于第k个斐波那契数
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(display (gcd 20 5))