// Asymptote library for mathematic using.
import math;

guide func_path(real func(real), pair value_domain, real step) //{
{
    int i; // counter
    guide ret_path; // return path
    if(step > (value_domain.y - value_domain.x) / 1000)
        step = (value_domain.y - value_domain.x) / 1000;
    real point_value = value_domain.x;
    while(point_value <= value_domain.y){
        ret_path = ret_path -- (point_value, func(point_value));
        point_value += step;
    }
    if(point_value != value_domain.y)
        ret_path = ret_path -- (value_domain.y, func(value_domain.y));
    return ret_path;
}//}

guide polar_func_path(real polar_func(real), pair domain, real step)//{
{
    guide ret_guide;
    real loop_val = domain.x;
    real rho;
    if((domain.y - domain.x) * step <= 0)
        return ret_guide;
    if((domain.y - domain.x)/step < 1000)
        step = (domain.y - domain.x)/1000;
    while(loop_val < domain.y){
        real rho = polar_func(loop_val);
        ret_guide = 
            ret_guide -- (cos(loop_val)*rho, sin(loop_val)*rho);
        loop_val += step;
    }
    if(domain.y != loop_val){
        real rho = polar_func(loop_val);
        ret_guide = 
            ret_guide -- (cos(domain.x)*rho, sin(domain.x)*rho);
    }
    return ret_guide;
}//}

picture coordinate(int XL, int YL)//{
{
    picture ret_pic;
    XL = abs(XL);
    YL = abs(YL);
real offset_axal = (XL + YL) / 30;
    draw(ret_pic, (-XL - offset_axal, 0) -- (XL + offset_axal, 0), linewidth(.8), arrow = Arrow(TeXHead));
    draw(ret_pic, (0, -YL - offset_axal) -- (0, YL + offset_axal), linewidth(.8), arrow = Arrow(TeXHead));
    int i, j; pen grid_pen = gray(0.5) + linetype(new real[] {2, 2});
    label(ret_pic, "$O$", (0, 0), SW);
    for(i = -XL;i <= XL; ++i){
        if(i == 0)
            continue;
        draw(ret_pic, (i, -YL - offset_axal / 2) -- (i, YL + offset_axal / 2), grid_pen);
        dot((i, 0));
        label(ret_pic, format("$%d$", i), (i, 0), SW);
    }
    for(j = -YL;j <= YL; ++j){
        if(j == 0)
            continue;
        draw(ret_pic, (-XL - offset_axal / 2, j) -- (XL + offset_axal / 2, j), grid_pen);
        dot((0, j));
        label(ret_pic, format("$%d$", j), (0, j), SW);
    }
    return ret_pic;
}//}

guide drawFunc(real func(real) = new real(real x){return x;}, 
               pair valueDomain = (-1, 1), real step = 0.1pt)//{
{
    return func_path(func, valueDomain, step);
}
//}

guide drawPolarFunc(real polar_func(real) = new real(real x){return x;}, 
                    pair valueDomain = (-1, 1), real step = 0.1pt)//{
{
    return polar_func_path(polar_func, valueDomain, step);
}
//}
