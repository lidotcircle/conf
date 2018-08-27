// misc function

transform scaleat(real xs, real ys=xs, pair at=(0,0))
{
    return shift(at)*scale(xs, ys)*shift(-at);
}
