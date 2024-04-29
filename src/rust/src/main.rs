fn main() {
    let args: Vec<String> = std::env::args().collect();
    let max: usize = match args.len() {
        2 => args[1].parse::<usize>().unwrap(),
        1 => 1 << 16,
        _ => panic!("Invalid number of CLI arguments"),
    };

    let primes: Vec<u32> = generate_primes(max);
    for (idx, prime) in primes.into_iter().enumerate() {
        println!("{} {}", idx, prime);
    }
}

fn generate_primes(max: usize) -> Vec<u32> {
    let mut p = Vec::<u32>::with_capacity(max);
    p.push(2);
    p.push(3);
    for i in 2..max {
        let last_p = p[i - 1];
        let mut next_p = last_p + 2;
        let mut k = 1;
        let mut s = xqrt(next_p);
        while (k < i) && (p[k] <= s) {
            if (next_p % p[k]) == 0 {
                next_p += 2;
                s = xqrt(next_p);
                k = 1;
            } else {
                k += 1;
            }
        }
        p.push(next_p)
    }
    p
}

fn xqrt(x: u32) -> u32 {
    if x <= 0 {
        return 0;
    }
    let mut i = x / 2;
    let mut l;
    loop {
        l = i;
        i = (l + x / l) / 2;
        if ((l as i32) - (i as i32)).abs() <= 1 {
            return (i + x / 2) / 2;
        }
    }
}
