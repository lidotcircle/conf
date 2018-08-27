// something to draw the illustration in mechanics of structure.

import math;
import graph;

// the __dimen_unit__, main unit
real __dimen_unit__ = 10;
// the number of short line below the support
int __under_part_count = 12; real __short_line_length__ = __dimen_unit__/3.5; real __short_line_dir__=-30;
// different bold pens
real boldTime = 0.8, bboldTime = 1.5;
real mboldTime = 1.1, sboldTime = 0.5;
// circle parameter
real __circle_bold__ = 0.8; real __circle_radius__ = __dimen_unit__/7;

// lever
picture lever(pair PFrom, pair PTo)//{
{
    picture ret;
    path level_path = PFrom -- PTo;
    pen bboldpen = currentpen + bboldTime*linewidth(currentpen);
    draw(pic=ret, g=level_path, p=bboldpen+.8blue);
    return ret;
}//}

// hinge point
picture hingePoint(pair point, real scale=1)//{
{
    picture ret;
    pen circle_boldpen = currentpen + __circle_bold__*linewidth(currentpen);
    pair origin=(0,0);
    fill(ret, circle(origin, __circle_radius__*scale), white);
    draw(ret, circle(origin, __circle_radius__*scale), p=circle_boldpen);
    return shift(point)*ret;
}//}

// hinge support
picture hingeSupport(pair on, real dir = 0, pair scale=(1,1))//{
{
    pair origin = (0, 0);
    pen boldpen = currentpen + boldTime*linewidth(currentpen);
    pen bboldpen = currentpen + bboldTime*linewidth(currentpen);
    pen sboldpen = currentpen + sboldTime*linewidth(currentpen);
    pen circle_boldpen = currentpen + __circle_bold__*linewidth(currentpen);
    pair underPo, underPt;
    underPo = (xpart(origin) - __dimen_unit__, ypart(origin) - __dimen_unit__*sqrt(3));
    underPt = (xpart(origin) + __dimen_unit__, ypart(origin) - __dimen_unit__*sqrt(3));
    picture ret;
    guide triangle = origin -- underPo -- underPt -- cycle;
    underPo = (xpart(underPo) - __dimen_unit__/4, ypart(underPo));
    underPt = (xpart(underPt) + __dimen_unit__/4, ypart(underPt));
    guide underPart = underPo -- underPt;
    draw(pic=ret, triangle, p = sboldpen);
    draw(pic=ret, g = underPart, p = bboldpen);
    path underShort = underPt -- (xpart(underPt), ypart(underPt) - __short_line_length__);
    underShort = shift(-__dimen_unit__/8,0)*rotate(__short_line_dir__, underPt)*underShort;
    int i = 0;
    real dist = ((xpart(underPt) - xpart(underPo)) - __dimen_unit__/4)/(__under_part_count-1);
    path tempPath;
    for(i = 0; i < __under_part_count;++i){
        tempPath = shift(-dist*i, 0)*underShort;
        draw(pic=ret, g = tempPath, p = boldpen);
    }
    ret.add(hingePoint(origin));
    ret = scale(xpart(scale), ypart(scale))*ret;
    // rotate the returned picture, and move the picture to <on> point.
    ret = shift(on)*rotate(dir*180/pi, origin)*ret;
    return ret;
}//}

// fixed support
picture fixedSupport(pair on, real dir = 0, pair scale=(1,1))//{
{
    picture ret;
    pen boldpen = currentpen + boldTime*linewidth(currentpen);
    pen bboldpen = currentpen + bboldTime*linewidth(currentpen);
    pair origin = (0, 0);
    pair p_one, p_two;
    p_one = (-__dimen_unit__ - __dimen_unit__/4, 0); p_two = (__dimen_unit__+__dimen_unit__/4, 0);
    path mainLine = p_one -- p_two;
    draw(pic=ret, g=mainLine, p=bboldpen);
    path shortPart = p_two -- (xpart(p_two), -__short_line_length__);
    shortPart = shift(-__dimen_unit__/8,0)*rotate(__short_line_dir__, p_two)*shortPart;
    int i = 0;
    real dist = ((xpart(p_two) - xpart(p_one)) - __dimen_unit__/4)/(__under_part_count-1);
    path tempPath;
    for(i = 0; i < __under_part_count;++i){
        tempPath = shift(-dist*i, 0)*shortPart;
        draw(pic=ret, g = tempPath, p = boldpen);
    }
    fill(ret, circle(origin, __circle_radius__));
    ret = scale(xpart(scale), ypart(scale))*ret;
    // rotate the returned picture, and move the picture to <on> point.
    ret = shift(on)*rotate(dir*180/pi, origin)*ret;
    return ret;
}//}

// roller support
picture rollerSupport(pair on, real dir = 0, pair scale=(1,1))//{
{
    picture ret;
    pen boldpen = currentpen + boldTime*linewidth(currentpen);
    pen bboldpen = currentpen + bboldTime*linewidth(currentpen);
    pen sboldpen = currentpen + sboldTime*linewidth(currentpen);
    pen mboldpen = currentpen + mboldTime*linewidth(currentpen);
    pen circle_boldpen = currentpen + __circle_bold__*linewidth(currentpen);
    pair origin = (0, 0);
    pair p_one, p_two;
    p_one = (-__dimen_unit__-__dimen_unit__/4, -__dimen_unit__); p_two = (__dimen_unit__ + __short_line_length__, -__dimen_unit__);
    path mainLine = p_one -- p_two;
    draw(ret, g=mainLine, p=bboldpen);
    draw(ret, circle((0, -__dimen_unit__ + __dimen_unit__/20), __circle_radius__), p=circle_boldpen);
    draw(ret, (0, 0) -- (0, -__dimen_unit__), p = mboldpen);
    path shortPart = p_two -- (xpart(p_two), ypart(p_two)-__short_line_length__);
    shortPart = shift(-__dimen_unit__/8,0)*rotate(__short_line_dir__, p_two)*shortPart;
    int i = 0;
    real dist = ((xpart(p_two) - xpart(p_one)) - __dimen_unit__/4)/(__under_part_count-1);
    path tempPath;
    for(i = 0; i < __under_part_count;++i){
        tempPath = shift(-dist*i, 0)*shortPart;
        draw(pic=ret, g = tempPath, p = boldpen);
    }
    draw(ret, circle(origin, __circle_radius__), p=circle_boldpen);
    ret = scale(xpart(scale), ypart(scale))*ret;
    // rotate the returned picture, and move the picture to <on> point.
    ret = shift(on)*rotate(dir*180/pi, origin)*ret;
    return ret;
}//}

// sliding support
picture slidingSupport(pair on, real dir = 0, pair scale=(1,1))//{
{
    picture ret;
    pen boldpen = currentpen + boldTime*linewidth(currentpen);
    pen bboldpen = currentpen + bboldTime*linewidth(currentpen);
    pen sboldpen = currentpen + sboldTime*linewidth(currentpen);
    pen mboldpen = currentpen + mboldTime*linewidth(currentpen);
    pen circle_boldpen = currentpen + __circle_bold__*linewidth(currentpen);
    pair origin = (0, 0);
    real magnified = 1.3;
    pair p_one, p_two;
    p_one = (-__dimen_unit__-__dimen_unit__/4, -__dimen_unit__*magnified); p_two = (__dimen_unit__ + __short_line_length__, -__dimen_unit__*magnified);
    path mainLine = p_one -- p_two;
    draw(ret, g=mainLine, p=bboldpen);
    path shortPart = p_two -- (xpart(p_two), ypart(p_two)-__short_line_length__);
    shortPart = shift(-__dimen_unit__/8,0)*rotate(__short_line_dir__, p_two)*shortPart;
    int i = 0;
    real dist = ((xpart(p_two) - xpart(p_one)) - __dimen_unit__/4)/(__under_part_count-1);
    path tempPath;
    for(i = 0; i < __under_part_count;++i){
        tempPath = shift(-dist*i, 0)*shortPart;
        draw(pic=ret, g = tempPath, p = boldpen);
    }
    path otherLine = (-__dimen_unit__, 0) -- (__dimen_unit__, 0);
    draw(ret, otherLine, p=mboldpen);
    pair cir_p_one, cir_p_two, cir_p_thr, cir_p_for;
    cir_p_one = (-__dimen_unit__/1.5, -__circle_radius__); cir_p_two = (__dimen_unit__/1.5, -__circle_radius__);
    cir_p_thr = (-__dimen_unit__/1.5, -__dimen_unit__*magnified + __circle_radius__); cir_p_for = (__dimen_unit__/1.5, -__dimen_unit__*magnified + __circle_radius__);
    draw(ret, cir_p_one -- cir_p_thr, p=boldpen);
    draw(ret, cir_p_two -- cir_p_for, p=boldpen);
    ret.add(hingePoint(cir_p_one));
    ret.add(hingePoint(cir_p_two));
    ret.add(hingePoint(cir_p_thr));
    ret.add(hingePoint(cir_p_for));
    fill(ret, circle(origin, __circle_radius__));
    ret = scale(xpart(scale), ypart(scale))*ret;
    // rotate the returned picture, and move the picture to <on> point.
    ret = shift(on)*rotate(dir*180/pi, origin)*ret;
    return ret;
}//}

// load_unit
real __dimen_load_unit__ = __dimen_unit__ / 2;
// point load
picture pointLoad(pair on, real dir = 0, string what="", pair offset = (0,0), real scale = 1)//{
{
    picture ret;
    pen bboldpen = currentpen + bboldTime*linewidth(currentpen);
    pair origin = (0, 0);
    real arrow_length = __dimen_load_unit__ * 3;
    path arrow_line = (0, arrow_length) -- origin;
    draw(ret, g = arrow_line, arrow = Arrow(), p = bboldpen);
    real __fontsize = arrow_length/4 * scale;
    ret = shift(0, arrow_length/10)*ret;
    ret = rotate(dir*180/pi) * ret;
    string what_out;
    if(dir != 0)
        what_out = "\hbox{\vrule\vbox{\hrule\vskip.2ex\relax\hbox{" + what + "\lower.1ex\hbox{\,$\angle" + 
            string(round(dir*180/pi)) + "\kern-.1em^\circ$}" + "}\vskip.2ex\hrule}\vrule}";
    else
        what_out = what;
    label(ret, what_out, (arrow_length*cos(dir + pi/2), arrow_length*sin(dir + pi/2)) +
            scale(scale, scale)*rotate(dir*180/pi)*(0, arrow_length/7) + offset, 
            (0, 1), p=currentpen + fontsize(__fontsize));
    ret = shift(on)*scale(scale, scale)*ret;
    return ret;
}//}

// distributed load
picture distributedLoad(pair From, pair To, real dir = 0, string what="", pair offset = (0, 0), 
        real destiny = 0, real scale = 1)//{
{
    picture ret;
    pen boldpen = currentpen + boldTime*linewidth(currentpen);
    real total_length = length(From - To);
    real __dir_ft = angle(To - From);
    pair origin = (0, 0), final = (total_length, 0);
    real arrow_length = __dimen_load_unit__ * 3 * scale;
// MARK
    path arrow_line = (0, arrow_length) -- origin;
    arrow_line = rotate(dir*180/pi)*shift(0, arrow_length/5)*arrow_line;
    int counter;
    if(destiny < 20*linewidth(currentpen))
        counter = floor(total_length / (linewidth(currentpen) * 24)) + 1;
    else
        counter = floor(total_length / destiny) + 1;
    if (counter<2)
        counter=2;
    real dist_per = total_length / (counter-1);
    int i;
    for(i=0;i<counter;++i)
        draw(ret, shift(i*dist_per, 0)*arrow_line, arrow = Arrow(), p = boldpen);
    draw(ret, shift(rotate(dir*180/pi)*(0, arrow_length/5  + arrow_length))*
            (origin -- (total_length, 0)),
            p=boldpen);
    string what_out;
    if(dir != 0)
        what_out = "\hbox{\vrule\vbox{\hrule\vskip.2ex\relax\hbox{" + what + "\lower.1ex\hbox{\,$\angle" + 
            string(round(dir*180/pi)) + "\kern-.1em^\circ$}" + "}\vskip.2ex\hrule}\vrule}";
    else
        what_out = what;
    real __fontsize = arrow_length/4 * scale;
    label(ret, what_out, shift(rotate(dir*180/pi)*(0, arrow_length/5  + arrow_length))*
            ((origin + (total_length, 0))/2) + offset, align=(0,1),
            p=currentpen+fontsize(__fontsize));
    ret = shift(From)*rotate(__dir_ft*180/pi)*ret;
    return ret;
}//}

// moment
real radius_of_moment = __dimen_load_unit__ * 1.5;
picture momentLoad(pair on, bool clock = true, string what = "", 
        real arc = 270, pair offset = (0, 0), real scale = 1)//{
{
    picture ret;
    if(clock)
        arc = - arc;
    pen bboldpen = currentpen + bboldTime*linewidth(currentpen);
    draw(ret, Arc(on, radius_of_moment, -(arc-180)/2, (arc+180)/2), arrow=Arrow(), p = bboldpen);
    pair max_p = max(ret), min_p = min(ret);
    real __fontsize = radius_of_moment/2 * scale;
    ret = shift(on)*scale(scale, scale)*shift(-on)*ret;
    label(ret, what, ((xpart(min_p)+xpart(max_p))/2, ypart(max_p)) + offset, 
            p = currentpen + fontsize(__fontsize));
    return ret;
}//}

// combine variable picture args
void combinePic(picture keyword dest = currentpicture ... picture[] picSet)//{
{
    int i;
    for(i=0;i<picSet.length;++i)
        dest.add(picSet[i]);
    return;
}//}
