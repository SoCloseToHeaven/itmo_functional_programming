function isPrime(n) {
    if (n < 2) {
        return false;
    }
    for (let i = 2; i <= Math.sqrt(n); i++) {
        if (n % i === 0) {
            return false;
        }
    }
    return true;
}

function countPrimes(a, b) {
    let n = 0;
    while (isPrime(n * n + a * n + b)) {
        n++;
    }
    return n;
}

function findMaxCoeffs() {
    let maxCount = 0;
    let maxCoeffs = [0, 0];

    for (let a = -999; a < 1000; a++) {
        for (let b = -1000; b <= 1000; b++) {
            let count = countPrimes(a, b);
            if (count > maxCount) {
                maxCount = count;
                maxCoeffs = [a, b];
            }
        }
    }

    return maxCoeffs
}

// Example usage:
const [a, b] = findMaxCoeffs();
console.log(`Max coefficients: a=${a}, b=${b}`);
console.log(`Coefficients product: ${a * b}`)