function isPalindrome(num) {
    const strNum = num.toString();
    return strNum === strNum.split('').reverse().join('');
}

(function findLargestPalindrome() {
    let maxPal = 0;
    for (let i = 999; i > 99; i--) {
        for (let j = 999; j > 99; j--) {
            const product = i * j;
            if (product > maxPal && isPalindrome(product)) {
                maxPal = product
            }
        }
    }

    console.log(maxPal);
})();