def kenken_products(target, nbox, hist=[]):
    if nbox == 0:
        if target == 1:
            print(" ".join(str(e) for e in hist))
        return
    for a in nums:
        if len(hist) and hist[-1] < a:
            continue
        if target % a == 0:
            ppp(target // a, nbox - 1, hist + [a])

def kenken_sums(target, nbox, hist=[]):
    if nbox == 0:
        if target == 0:
            print(" ".join(str(e) for e in hist))
        return
    for a in nums:
        if len(hist) and hist[-1] < a:
            continue
        if target >= a:
            sss(target - a, nbox - 1, hist + [a])
