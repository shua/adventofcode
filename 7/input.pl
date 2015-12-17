:- include("circuit.pl").
bit(ls, N, B) :- and(lf, lq, N, B).
bit(jn, N, B) :- shft(iu, 1, N, B).
bit(bv, N, B) :- or(bo, bu, N, B).
bit(hc, N, B) :- shft(gj, 1, N, B).
bit(eu, N, B) :- shft(et, 2, N, B).
bit(by, N, B) :- and(bv, bx, N, B).
bit(iu, N, B) :- or(is, it, N, B).
bit(o, N, B) :- or(b, n, N, B).
bit(gg, N, B) :- or(gf, ge, N, B).
bit(ku, N, B) :- not(kt, N, B).
bit(ed, N, B) :- and(ea, eb, N, B).
bit(ks, N, B) :- or(kl, kr, N, B).
bit(hl, N, B) :- and(hi, hk, N, B).
bit(ax, N, B) :- and(au, av, N, B).
bit(lg, N, B) :- shft(lf, 2, N, B).
bit(df, N, B) :- shft(dd, 3, N, B).
bit(fc, N, B) :- and(eu, fa, N, B).
bit(di, N, B) :- and(df, dg, N, B).
bit(it, N, B) :- shft(ip, -15, N, B).
bit(em, N, B) :- not(el, N, B).
bit(ff, N, B) :- or(et, fe, N, B).
bit(fn, N, B) :- shft(fj, -15, N, B).
bit(u, N, B) :- or(t, s, N, B).
bit(ma, N, B) :- or(ly, lz, N, B).
bit(kr, N, B) :- and(ko, kq, N, B).
bit(fy, N, B) :- not(fx, N, B).
bit(fm, N, B) :- shft(et, 1, N, B).
bit(fb, N, B) :- or(eu, fa, N, B).
bit(de, N, B) :- shft(dd, 2, N, B).
bit(gp, N, B) :- not(go, N, B).
bit(ke, N, B) :- and(kb, kd, N, B).
bit(hi, N, B) :- or(hg, hh, N, B).
bit(kg, N, B) :- shft(jm, -1, N, B).
bit(co, N, B) :- not(cn, N, B).
bit(jq, N, B) :- shft(jp, 2, N, B).
bit(js, N, B) :- shft(jp, 5, N, B).
bit(ip, N, B) :- and(1, io, N, B).
bit(es, N, B) :- shft(eo, -15, N, B).
bit(jk, N, B) :- and(1, jj, N, B).
bit(j, N, B) :- and(g, i, N, B).
bit(ck, N, B) :- shft(ci, 3, N, B).
bit(gq, N, B) :- and(gn, gp, N, B).
bit(fv, N, B) :- and(fs, fu, N, B).
bit(lm, N, B) :- and(lj, ll, N, B).
bit(jo, N, B) :- shft(jk, -15, N, B).
bit(iw, N, B) :- shft(iu, 3, N, B).
bit(ij, N, B) :- not(ii, N, B).
bit(cd, N, B) :- and(1, cc, N, B).
bit(bp, N, B) :- shft(bn, 3, N, B).
bit(gx, N, B) :- not(gw, N, B).
bit(fu, N, B) :- not(ft, N, B).
bit(jp, N, B) :- or(jn, jo, N, B).
bit(jc, N, B) :- or(iv, jb, N, B).
bit(hw, N, B) :- or(hv, hu, N, B).
bit(b, N, B) :- eql(19138, N, B).
bit(gm, N, B) :- shft(gj, 5, N, B).
bit(ht, N, B) :- and(hq, hs, N, B).
bit(er, N, B) :- shft(dy, 1, N, B).
bit(ap, N, B) :- or(ao, an, N, B).
bit(lf, N, B) :- or(ld, le, N, B).
bit(ce, N, B) :- shft(bk, -1, N, B).
bit(cc, N, B) :- and(bz, cb, N, B).
bit(bm, N, B) :- shft(bi, -15, N, B).
bit(io, N, B) :- and(il, in, N, B).
bit(ai, N, B) :- and(af, ah, N, B).
bit(bl, N, B) :- shft(as, 1, N, B).
bit(lh, N, B) :- shft(lf, 3, N, B).
bit(et, N, B) :- or(er, es, N, B).
bit(ay, N, B) :- not(ax, N, B).
bit(db, N, B) :- shft(ci, 1, N, B).
bit(fg, N, B) :- and(et, fe, N, B).
bit(ln, N, B) :- or(lg, lm, N, B).
bit(n, N, B) :- and(k, m, N, B).
bit(ia, N, B) :- shft(hz, 2, N, B).
bit(lb, N, B) :- shft(kh, -1, N, B).
bit(ez, N, B) :- not(ey, N, B).
bit(dj, N, B) :- not(di, N, B).
bit(eg, N, B) :- or(dz, ef, N, B).
bit(a, N, B) :- eql(lx, N, B).
bit(ja, N, B) :- not(iz, N, B).
bit(hd, N, B) :- shft(gz, -15, N, B).
bit(cf, N, B) :- or(ce, cd, N, B).
bit(ft, N, B) :- and(fq, fr, N, B).
bit(bb, N, B) :- and(at, az, N, B).
bit(hb, N, B) :- or(ha, gz, N, B).
bit(fx, N, B) :- and(fp, fv, N, B).
bit(gc, N, B) :- not(gb, N, B).
bit(ii, N, B) :- and(ia, ig, N, B).
bit(gn, N, B) :- or(gl, gm, N, B).
bit(c, N, B) :- eql(0, N, B).
bit(cb, N, B) :- not(ca, N, B).
bit(cg, N, B) :- shft(bn, 1, N, B).
bit(t, N, B) :- shft(c, -1, N, B).
bit(iy, N, B) :- or(iw, ix, N, B).
bit(kh, N, B) :- or(kg, kf, N, B).
bit(ek, N, B) :- or(dy, ej, N, B).
bit(kp, N, B) :- and(km, kn, N, B).
bit(fd, N, B) :- not(fc, N, B).
bit(ib, N, B) :- shft(hz, 3, N, B).
bit(dr, N, B) :- not(dq, N, B).
bit(fh, N, B) :- not(fg, N, B).
bit(dz, N, B) :- shft(dy, 2, N, B).
bit(kl, N, B) :- shft(kk, 2, N, B).
bit(fj, N, B) :- and(1, fi, N, B).
bit(hs, N, B) :- not(hr, N, B).
bit(ki, N, B) :- shft(jp, 1, N, B).
bit(bn, N, B) :- or(bl, bm, N, B).
bit(gz, N, B) :- and(1, gy, N, B).
bit(gu, N, B) :- and(gr, gt, N, B).
bit(dd, N, B) :- or(db, dc, N, B).
bit(dl, N, B) :- or(de, dk, N, B).
bit(av, N, B) :- shft(as, 5, N, B).
bit(li, N, B) :- shft(lf, 5, N, B).
bit(hp, N, B) :- and(hm, ho, N, B).
bit(ci, N, B) :- or(cg, ch, N, B).
bit(gw, N, B) :- and(gj, gu, N, B).
bit(gi, N, B) :- shft(ge, -15, N, B).
bit(g, N, B) :- or(e, f, N, B).
bit(fw, N, B) :- or(fp, fv, N, B).
bit(fe, N, B) :- and(fb, fd, N, B).
bit(ch, N, B) :- shft(cd, -15, N, B).
bit(v, N, B) :- shft(b, 1, N, B).
bit(ba, N, B) :- or(at, az, N, B).
bit(bo, N, B) :- shft(bn, 2, N, B).
bit(lk, N, B) :- and(lh, li, N, B).
bit(do, N, B) :- and(dl, dn, N, B).
bit(ej, N, B) :- and(eg, ei, N, B).
bit(fa, N, B) :- and(ex, ez, N, B).
bit(kq, N, B) :- not(kp, N, B).
bit(ll, N, B) :- not(lk, N, B).
bit(ak, N, B) :- and(x, ai, N, B).
bit(kb, N, B) :- or(jp, ka, N, B).
bit(je, N, B) :- not(jd, N, B).
bit(jb, N, B) :- and(iy, ja, N, B).
bit(jr, N, B) :- shft(jp, 3, N, B).
bit(ga, N, B) :- or(fo, fz, N, B).
bit(dh, N, B) :- or(df, dg, N, B).
bit(gk, N, B) :- shft(gj, 2, N, B).
bit(gv, N, B) :- or(gj, gu, N, B).
bit(ji, N, B) :- not(jh, N, B).
bit(bj, N, B) :- shft(ap, -1, N, B).
bit(lt, N, B) :- not(ls, N, B).
bit(jl, N, B) :- shft(ir, -1, N, B).
bit(ca, N, B) :- and(bn, by, N, B).
bit(lz, N, B) :- shft(lv, -15, N, B).
bit(bd, N, B) :- and(ba, bc, N, B).
bit(dc, N, B) :- shft(cy, -15, N, B).
bit(lq, N, B) :- and(ln, lp, N, B).
bit(aq, N, B) :- shft(x, 1, N, B).
bit(gr, N, B) :- or(gk, gq, N, B).
bit(ky, N, B) :- not(kx, N, B).
bit(jj, N, B) :- and(jg, ji, N, B).
bit(bz, N, B) :- or(bn, by, N, B).
bit(gf, N, B) :- shft(fl, -1, N, B).
bit(br, N, B) :- or(bp, bq, N, B).
bit(hq, N, B) :- or(he, hp, N, B).
bit(ew, N, B) :- shft(et, 5, N, B).
bit(iv, N, B) :- shft(iu, 2, N, B).
bit(go, N, B) :- and(gl, gm, N, B).
bit(aj, N, B) :- or(x, ai, N, B).
bit(he, N, B) :- or(hc, hd, N, B).
bit(lo, N, B) :- and(lg, lm, N, B).
bit(lj, N, B) :- or(lh, li, N, B).
bit(du, N, B) :- shft(da, -1, N, B).
bit(fp, N, B) :- shft(fo, 2, N, B).
bit(gs, N, B) :- and(gk, gq, N, B).
bit(bk, N, B) :- or(bj, bi, N, B).
bit(lr, N, B) :- or(lf, lq, N, B).
bit(cr, N, B) :- and(cj, cp, N, B).
bit(hy, N, B) :- shft(hu, -15, N, B).
bit(bi, N, B) :- and(1, bh, N, B).
bit(fq, N, B) :- shft(fo, 3, N, B).
bit(lp, N, B) :- not(lo, N, B).
bit(iq, N, B) :- shft(hw, -1, N, B).
bit(dw, N, B) :- shft(dd, 1, N, B).
bit(dx, N, B) :- shft(dt, -15, N, B).
bit(el, N, B) :- and(dy, ej, N, B).
bit(ar, N, B) :- shft(an, -15, N, B).
bit(as, N, B) :- or(aq, ar, N, B).
bit(s, N, B) :- and(1, r, N, B).
bit(fz, N, B) :- and(fw, fy, N, B).
bit(in, N, B) :- not(im, N, B).
bit(ev, N, B) :- shft(et, 3, N, B).
bit(dt, N, B) :- and(1, ds, N, B).
bit(ef, N, B) :- and(ec, ee, N, B).
bit(al, N, B) :- not(ak, N, B).
bit(jm, N, B) :- or(jl, jk, N, B).
bit(eo, N, B) :- and(1, en, N, B).
bit(lc, N, B) :- or(lb, la, N, B).
bit(jh, N, B) :- and(iu, jf, N, B).
bit(ix, N, B) :- shft(iu, 5, N, B).
bit(bw, N, B) :- and(bo, bu, N, B).
bit(da, N, B) :- or(cz, cy, N, B).
bit(jd, N, B) :- and(iv, jb, N, B).
bit(iz, N, B) :- and(iw, ix, N, B).
bit(ly, N, B) :- shft(lf, 1, N, B).
bit(jg, N, B) :- or(iu, jf, N, B).
bit(dn, N, B) :- not(dm, N, B).
bit(lx, N, B) :- or(lw, lv, N, B).
bit(ha, N, B) :- shft(gg, -1, N, B).
bit(lu, N, B) :- and(lr, lt, N, B).
bit(fo, N, B) :- or(fm, fn, N, B).
bit(hg, N, B) :- shft(he, 3, N, B).
bit(am, N, B) :- and(aj, al, N, B).
bit(la, N, B) :- and(1, kz, N, B).
bit(eb, N, B) :- shft(dy, 5, N, B).
bit(jf, N, B) :- and(jc, je, N, B).
bit(cp, N, B) :- and(cm, co, N, B).
bit(gy, N, B) :- and(gv, gx, N, B).
bit(ex, N, B) :- or(ev, ew, N, B).
bit(kc, N, B) :- and(jp, ka, N, B).
bit(fl, N, B) :- or(fk, fj, N, B).
bit(ea, N, B) :- shft(dy, 3, N, B).
bit(bt, N, B) :- not(bs, N, B).
bit(ah, N, B) :- not(ag, N, B).
bit(eh, N, B) :- and(dz, ef, N, B).
bit(cz, N, B) :- shft(cf, -1, N, B).
bit(cw, N, B) :- not(cv, N, B).
bit(cy, N, B) :- and(1, cx, N, B).
bit(dm, N, B) :- and(de, dk, N, B).
bit(cn, N, B) :- and(ck, cl, N, B).
bit(aa, N, B) :- shft(x, 5, N, B).
bit(ep, N, B) :- shft(dv, -1, N, B).
bit(hf, N, B) :- shft(he, 2, N, B).
bit(bx, N, B) :- not(bw, N, B).
bit(cm, N, B) :- or(ck, cl, N, B).
bit(bs, N, B) :- and(bp, bq, N, B).
bit(be, N, B) :- or(as, bd, N, B).
bit(hr, N, B) :- and(he, hp, N, B).
bit(ey, N, B) :- and(ev, ew, N, B).
bit(lv, N, B) :- and(1, lu, N, B).
bit(km, N, B) :- shft(kk, 3, N, B).
bit(p, N, B) :- and(b, n, N, B).
bit(kd, N, B) :- not(kc, N, B).
bit(lw, N, B) :- shft(lc, -1, N, B).
bit(ko, N, B) :- or(km, kn, N, B).
bit(ig, N, B) :- and(id, if, N, B).
bit(ik, N, B) :- and(ih, ij, N, B).
bit(ju, N, B) :- and(jr, js, N, B).
bit(cl, N, B) :- shft(ci, 5, N, B).
bit(is, N, B) :- shft(hz, 1, N, B).
bit(kf, N, B) :- and(1, ke, N, B).
bit(gt, N, B) :- not(gs, N, B).
bit(az, N, B) :- and(aw, ay, N, B).
bit(y, N, B) :- shft(x, 2, N, B).
bit(ae, N, B) :- and(ab, ad, N, B).
bit(fi, N, B) :- and(ff, fh, N, B).
bit(cv, N, B) :- and(ci, ct, N, B).
bit(fk, N, B) :- shft(eq, -1, N, B).
bit(gl, N, B) :- shft(gj, 3, N, B).
bit(ao, N, B) :- shft(u, -1, N, B).
bit(bc, N, B) :- not(bb, N, B).
bit(hk, N, B) :- not(hj, N, B).
bit(kz, N, B) :- and(kw, ky, N, B).
bit(bf, N, B) :- and(as, bd, N, B).
bit(dy, N, B) :- or(dw, dx, N, B).
bit(bu, N, B) :- and(br, bt, N, B).
bit(kx, N, B) :- and(kk, kv, N, B).
bit(eq, N, B) :- or(ep, eo, N, B).
bit(hx, N, B) :- shft(he, 1, N, B).
bit(kk, N, B) :- or(ki, kj, N, B).
bit(jv, N, B) :- not(ju, N, B).
bit(en, N, B) :- and(ek, em, N, B).
bit(kn, N, B) :- shft(kk, 5, N, B).
bit(ei, N, B) :- not(eh, N, B).
bit(hz, N, B) :- or(hx, hy, N, B).
bit(ec, N, B) :- or(ea, eb, N, B).
bit(w, N, B) :- shft(s, -15, N, B).
bit(gh, N, B) :- shft(fo, 1, N, B).
bit(kw, N, B) :- or(kk, kv, N, B).
bit(bq, N, B) :- shft(bn, 5, N, B).
bit(ee, N, B) :- not(ed, N, B).
bit(hu, N, B) :- and(1, ht, N, B).
bit(cx, N, B) :- and(cu, cw, N, B).
bit(f, N, B) :- shft(b, 5, N, B).
bit(kt, N, B) :- and(kl, kr, N, B).
bit(ir, N, B) :- or(iq, ip, N, B).
bit(cj, N, B) :- shft(ci, 2, N, B).
bit(cq, N, B) :- or(cj, cp, N, B).
bit(r, N, B) :- and(o, q, N, B).
bit(dg, N, B) :- shft(dd, 5, N, B).
bit(d, N, B) :- shft(b, 2, N, B).
bit(kv, N, B) :- and(ks, ku, N, B).
bit(e, N, B) :- shft(b, 3, N, B).
bit(k, N, B) :- or(d, j, N, B).
bit(q, N, B) :- not(p, N, B).
bit(cs, N, B) :- not(cr, N, B).
bit(dv, N, B) :- or(du, dt, N, B).
bit(kj, N, B) :- shft(kf, -15, N, B).
bit(ad, N, B) :- not(ac, N, B).
bit(fr, N, B) :- shft(fo, 5, N, B).
bit(il, N, B) :- or(hz, ik, N, B).
bit(ka, N, B) :- and(jx, jz, N, B).
bit(gj, N, B) :- or(gh, gi, N, B).
bit(ld, N, B) :- shft(kk, 1, N, B).
bit(ic, N, B) :- shft(hz, 5, N, B).
bit(at, N, B) :- shft(as, 2, N, B).
bit(jz, N, B) :- not(jy, N, B).
bit(an, N, B) :- and(1, am, N, B).
bit(cu, N, B) :- or(ci, ct, N, B).
bit(hj, N, B) :- and(hg, hh, N, B).
bit(jx, N, B) :- or(jq, jw, N, B).
bit(x, N, B) :- or(v, w, N, B).
bit(le, N, B) :- shft(la, -15, N, B).
bit(dk, N, B) :- and(dh, dj, N, B).
bit(ds, N, B) :- and(dp, dr, N, B).
bit(jy, N, B) :- and(jq, jw, N, B).
bit(aw, N, B) :- or(au, av, N, B).
bit(bg, N, B) :- not(bf, N, B).
bit(ab, N, B) :- or(z, aa, N, B).
bit(gd, N, B) :- and(ga, gc, N, B).
bit(im, N, B) :- and(hz, ik, N, B).
bit(jw, N, B) :- and(jt, jv, N, B).
bit(ac, N, B) :- and(z, aa, N, B).
bit(jt, N, B) :- or(jr, js, N, B).
bit(hv, N, B) :- shft(hb, -1, N, B).
bit(hm, N, B) :- or(hf, hl, N, B).
bit(id, N, B) :- or(ib, ic, N, B).
bit(fs, N, B) :- or(fq, fr, N, B).
bit(ct, N, B) :- and(cq, cs, N, B).
bit(ih, N, B) :- or(ia, ig, N, B).
bit(dp, N, B) :- or(dd, do, N, B).
bit(l, N, B) :- and(d, j, N, B).
bit(ie, N, B) :- and(ib, ic, N, B).
bit(au, N, B) :- shft(as, 3, N, B).
bit(bh, N, B) :- and(be, bg, N, B).
bit(dq, N, B) :- and(dd, do, N, B).
bit(m, N, B) :- not(l, N, B).
bit(ge, N, B) :- and(1, gd, N, B).
bit(ag, N, B) :- and(y, ae, N, B).
bit(gb, N, B) :- and(fo, fz, N, B).
bit(if, N, B) :- not(ie, N, B).
bit(h, N, B) :- and(e, f, N, B).
bit(z, N, B) :- shft(x, 3, N, B).
bit(af, N, B) :- or(y, ae, N, B).
bit(hn, N, B) :- and(hf, hl, N, B).
bit(i, N, B) :- not(h, N, B).
bit(ho, N, B) :- not(hn, N, B).
bit(hh, N, B) :- shft(he, 5, N, B).
bit(d, N, B) :- eql(72, N, B).
bit(e, N, B) :- eql(507, N, B).
bit(f, N, B) :- eql(492, N, B).
bit(g, N, B) :- eql(114, N, B).
bit(h, N, B) :- eql(65412, N, B).
bit(i, N, B) :- eql(65079, N, B).
bit(x, N, B) :- eql(123, N, B).
bit(y, N, B) :- eql(456, N, B).
