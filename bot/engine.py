import json,os,sys,time,urllib.request,urllib.error
E=os.environ.get("MAKEFUN_ENV_URL") or sys.exit("no env")
V=[0,100,320,330,500,900,0]
KN=[-33,-31,-18,-14,14,18,31,33]
KI=[-17,-16,-15,-1,1,15,16,17]
BD=[-17,-15,15,17]; RD=[-16,-1,1,16]
MATE=100000

def _pst():
    P=[None]*8; A=[0,0,4,8,14,24,40,0]; C=[0,0,0,10,15,10,5,0]
    for t in range(8):
        P[t]=[0]*128
        for r in range(8):
            for f in range(8):
                c=min(f,7-f)+min(r,7-r); sq=r*16+f
                if t==1: v=A[r]+(C[r] if f in(3,4) else 0)
                elif t==2: v=6*c-20
                elif t==3: v=4*c-8
                elif t==4: v=(15 if r==6 else 0)+(5 if r==0 and f in(3,4) else 0)
                elif t==5: v=2*c-6
                elif t==6: v=[20,25,10,0,0,10,25,20][f] if r==0 else max(-50,-15*r)
                elif t==7: v=8*c-24
                else: v=0
                P[t][sq]=v
    return P
P=_pst()
def sqn(s): return "abcdefgh"[s&7]+str((s>>4)+1)
def sqi(n): return (ord(n[1])-49)*16+ord(n[0])-97
def uci(m):
    p=(m>>14)&7
    return sqn(m&127)+sqn((m>>7)&127)+("nbrq"[p-2] if p else "")
def parse(fen):
    global B,side,cast,ep,half,KS
    p=fen.split(); B=[0]*128; KS=[0,0]
    for i,row in enumerate(p[0].split("/")):
        f=0
        for c in row:
            if c.isdigit(): f+=int(c)
            else:
                v="pnbrqk".index(c.lower())+1
                if not c.isupper(): v=-v
                sq=(7-i)*16+f; B[sq]=v
                if abs(v)==6: KS[0 if v>0 else 1]=sq
                f+=1
    side=1 if p[1]=="w" else 0
    cast=0
    for c in p[2]:
        cast|={"K":1,"Q":2,"k":4,"q":8}.get(c,0)
    ep=sqi(p[3]) if p[3]!="-" else -1
    half=int(p[4]) if len(p)>4 else 0
def on(s): return 0<=s<128 and not (s&0x88)
def attacked(s,by):

    d=16 if by else -16
    for t in (s-d-1,s-d+1):
        if 0<=t<128 and not t&0x88 and B[t]==(1 if by else -1): return 1
    for t in KN:
        q=s+t
        if 0<=q<128 and not q&0x88 and abs(B[q])==2 and (B[q]>0)==bool(by): return 1
    for t in KI:
        q=s+t
        if 0<=q<128 and not q&0x88 and abs(B[q])==6 and (B[q]>0)==bool(by): return 1
    for dirs,ty in ((BD,(3,5)),(RD,(4,5))):
        for t in dirs:
            q=s+t
            while 0<=q<128 and not q&0x88:
                pc=B[q]
                if pc:
                    if (pc>0)==bool(by) and abs(pc) in ty: return 1
                    break
                q+=t
    return 0

CK=[15]*128

for _s,_m in ((0,13),(7,14),(4,12),(112,7),(119,11),(116,3)): CK[_s]=_m
S=[]
def gen(cap=0):
    ms=[]; d=16 if side else -16
    for f in range(128):
        if f&0x88: continue
        pc=B[f]
        if not pc or (pc>0)!=bool(side): continue
        t=abs(pc)
        if t==1:
            r=f>>4
            to=f+d
            if 0<=to<128 and not to&0x88 and not B[to]:
                if (r==6 and side) or (r==1 and not side):
                    for pr in (5,4,3,2): ms.append(f|to<<7|pr<<14)
                elif not cap:
                    ms.append(f|to<<7)
                    if (r==1 and side) or (r==6 and not side):
                        to2=f+2*d
                        if not B[to2]: ms.append(f|to2<<7)
            for dd in (d-1,d+1):
                to=f+dd
                if not (0<=to<128 and not to&0x88): continue
                if B[to] and (B[to]>0)!=bool(side):
                    if (r==6 and side) or (r==1 and not side):
                        for pr in (5,4,3,2): ms.append(f|to<<7|pr<<14)
                    else: ms.append(f|to<<7)
                elif to==ep:
                    ms.append(f|to<<7)
        elif t in (2,6):
            for dd in (KN if t==2 else KI):
                to=f+dd
                if 0<=to<128 and not to&0x88 and (not B[to] or (B[to]>0)!=bool(side)) and (B[to] or not cap): ms.append(f|to<<7)
        else:
            for dd in (BD if t==3 else RD if t==4 else BD+RD):
                to=f+dd
                while 0<=to<128 and not to&0x88:
                    pc2=B[to]
                    if pc2:
                        if (pc2>0)!=bool(side): ms.append(f|to<<7)
                        break
                    if not cap: ms.append(f|to<<7)
                    to+=dd
    if cap: return ms

    if side and cast&1 and B[4]==6 and B[7]==4 and not B[5] and not B[6] and not attacked(4,0) and not attacked(5,0) and not attacked(6,0): ms.append(4|6<<7)
    if side and cast&2 and B[4]==6 and B[0]==4 and not B[1] and not B[2] and not B[3] and not attacked(4,0) and not attacked(3,0) and not attacked(2,0): ms.append(4|2<<7)
    if not side and cast&4 and B[116]==-6 and B[119]==-4 and not B[117] and not B[118] and not attacked(116,1) and not attacked(117,1) and not attacked(118,1): ms.append(116|118<<7)
    if not side and cast&8 and B[116]==-6 and B[112]==-4 and not B[113] and not B[114] and not B[115] and not attacked(116,1) and not attacked(115,1) and not attacked(114,1): ms.append(116|114<<7)
    return ms
def make(m):
    global side,cast,ep,half
    f=m&127; to=(m>>7)&127; pr=(m>>14)&7
    pc=B[f]; d=16 if side else -16
    cap=B[to]; csq=to
    if abs(pc)==1 and to==ep and not cap:
        csq=to-d; cap=B[csq]; B[csq]=0
    S.append((m,cap,csq,cast,ep,half,KS[0],KS[1],pc))
    B[to]= (pr if pr else abs(pc))*(1 if side else -1); B[f]=0
    if abs(pc)==6:
        KS[1-side]=to
        if to-f==2: B[to-1]=B[to+1]; B[to+1]=0
        elif f-to==2: B[to+1]=B[to-2]; B[to-2]=0
    cast&=CK[f]&CK[to]
    ep=f+d if abs(pc)==1 and abs(to-f)==32 else -1
    half=0 if abs(pc)==1 or cap else half+1
    side^=1
def unmake():
    global side,cast,ep,half
    m,cap,csq,cast,ep,half,kw,kb,pc=S.pop()
    f=m&127; to=(m>>7)&127; pr=(m>>14)&7
    side^=1
    B[f]=pc
    if abs(pc)==6:
        if to-f==2: B[to+1]=B[to-1]; B[to-1]=0
        elif f-to==2: B[to-2]=B[to+1]; B[to+1]=0
    B[to]=0
    if cap: B[csq]=cap
    KS[0]=kw; KS[1]=kb
def legal():
    out=[]
    for m in gen():
        make(m)
        if not attacked(KS[side],side): out.append(m)
        unmake()
    return out

def ev():
    s=0; np=0; bi=[0,0]; kw=0; kb=0
    for q in range(128):
        if q&0x88: continue
        pc=B[q]
        if not pc: continue
        t=abs(pc)
        if t==6:
            if pc>0: kw=q
            else: kb=q
            continue
        r=q>>4; f=q&7
        if pc>0:
            s+=V[t]+P[t][q]
            if t==3: bi[0]+=1
            if t==1 and r>=5 and (not on(q+16) or B[q+16]!=-1) and (not on(q+15) or B[q+15]!=-1) and (not on(q+17) or B[q+17]!=-1): s+=(r-3)*25
        else:
            s-=V[t]+P[t][q^0x70]
            if t==3: bi[1]+=1
            if t==1 and r<=2 and (not on(q-16) or B[q-16]!=1) and (not on(q-15) or B[q-15]!=1) and (not on(q-17) or B[q-17]!=1): s-=(4-r)*25
        if t!=1: np+=V[t]
    NP[0]=np
    ph=min(np,6200)

    s+= (P[6][kw]*ph+P[7][kw]*(6200-ph))//6200
    s-= (P[6][kb^0x70]*ph+P[7][kb^0x70]*(6200-ph))//6200
    if bi[0]>=2: s+=30
    if bi[1]>=2: s-=30
    return s if side else -s
N=[0]; AB=[0]; TT={}; KL={}; HI={}; D=[0]; NP=[0]; T=[0]
def order(ms,ply,ttm):
    def sc(m):
        f=m&127; to=(m>>7)&127; pr=(m>>14)&7
        v=0
        if m==ttm: v=1000000
        elif B[to] or (abs(B[f])==1 and to==ep):
            vic=abs(B[to]) if B[to] else 1
            v=100000+10*V[vic]-V[abs(B[f])]//10
        if pr: v+=200000+V[pr]
        elif KL.get((ply,0))==m: v=50000
        elif KL.get((ply,1))==m: v=40000
        elif not B[to]: v+=HI.get(m,0)
        return v
    ms.sort(key=sc,reverse=True)
def quies(alpha,beta,ply):
    N[0]+=1
    if AB[0]or time.time()>T[0]:AB[0]=1;return 0
    chk=attacked(KS[1-side],1-side)
    best=-MATE-1
    if not chk:
        st=ev()
        if st>=beta: return st
        best=st
        if st>alpha: alpha=st
    ms=[]
    for m in gen(0 if chk else 1):
        f=m&127; to=(m>>7)&127; pr=(m>>14)&7
        if chk or B[to] or pr or (abs(B[f])==1 and to==ep): ms.append(m)
    order(ms,ply,0)
    legal=0
    for m in ms:
        make(m)
        if attacked(KS[side],side):
            unmake(); continue
        legal+=1
        sc=-quies(-beta,-alpha,ply+1)
        unmake()
        if AB[0]: return 0
        if sc>best: best=sc
        if sc>=beta: return sc
        if sc>alpha: alpha=sc
    if chk and not legal: return -MATE+ply
    return best
def search(depth,alpha,beta,ply):
    global side,cast,ep,half
    if ply and half>=100: return 0
    if depth<=0: return quies(alpha,beta,ply)
    N[0]+=1
    if AB[0]or time.time()>T[0]:AB[0]=1;return 0
    key=(tuple(B),side,cast,ep)
    e=TT.get(key)
    ttm=0
    if e and e[0]>=depth:
        ttm=e[3]
        s=e[1]
        if e[2]==2: return s
        if e[2]==1 and s>=beta: return s
        if e[2]==0 and s<=alpha: return s

    if depth>=3 and not attacked(KS[1-side],1-side) and ev()>=beta and NP[0]:
        c0,e0,h0=cast,ep,half; side^=1; ep=-1; half+=1
        sc=-search(depth-3,-beta,-beta+1,ply+1)
        side^=1; cast,ep,half=c0,e0,h0
        if AB[0]: return 0
        if sc>=beta: return beta
    a0=alpha; ms=gen(); order(ms,ply,ttm)
    best=-MATE-1; bm=0; legal=0
    for i,m in enumerate(ms):
        make(m)
        if attacked(KS[side],side):
            unmake(); continue
        legal+=1
        if i==0:
            sc=-search(depth-1,-beta,-alpha,ply+1)
        else:
            sc=-search(depth-1,-alpha-1,-alpha,ply+1)
            if not AB[0] and alpha<sc<beta:
                sc=-search(depth-1,-beta,-alpha,ply+1)
        unmake()
        if AB[0]: return 0
        if sc>best: best=sc; bm=m
        if sc>alpha: alpha=sc
        if alpha>=beta:
            if not B[(m>>7)&127] and not ((m>>14)&7):
                if KL.get((ply,0))!=m: KL[(ply,1)]=KL.get((ply,0),0); KL[(ply,0)]=m
                HI[m]=HI.get(m,0)+depth*depth
            break
    if not legal: return -MATE+ply if attacked(KS[1-side],1-side) else 0
    fl=0 if best<=a0 else 1 if best>=beta else 2

    TT[key]=(depth,best,fl,bm)
    return best

def from_uci(u):
    pr=("nbrq".index(u[4])+2) if len(u)==5 else 0
    return sqi(u[:2])|sqi(u[2:4])<<7|pr<<14
def choose(fen,envlegal):
    parse(fen)
    root=[from_uci(u) for u in envlegal]
    if len(root)==1: return root[0]
    best=root[0]; scores={m:0 for m in root}; TT.clear(); KL.clear(); HI.clear(); T[0]=time.time()+6
    for depth in range(1,8):
        N[0]=0; AB[0]=0
        root.sort(key=lambda m:-scores.get(m,0))
        cur=best; cur_sc=-MATE-1; done=True
        for j,m in enumerate(root):
            make(m)
            if attacked(KS[side],side):
                unmake(); scores[m]=-MATE; continue
            if j==0:
                sc=-search(depth-1,-MATE-1,MATE+1,1)
            else:
                sc=-search(depth-1,-cur_sc-1,-cur_sc,1)
                if not AB[0] and sc>cur_sc:
                    sc=-search(depth-1,-MATE-1,MATE+1,1)
            unmake()
            if AB[0]: done=False; break
            scores[m]=sc
            if sc>cur_sc: cur_sc=sc; cur=m
        if not done: break
        best=cur; D[0]=depth
        if cur_sc>=MATE-100: break
    return best
def post(p,body):
    req=urllib.request.Request(E+p,data=json.dumps(body).encode(),headers={"content-type":"application/json"})
    try:
        with urllib.request.urlopen(req) as r: return json.load(r)
    except urllib.error.HTTPError as e:
        sys.exit(f"{p}: HTTP {e.code} {e.read().decode(errors='replace')}")
while True:
    g=post("/game",{})
    if g["status"]=="done": break
    while g["status"]=="your_move":
        m=choose(g["fen"],g["legal_moves"])
        g=post("/move",{"game":g["game"],"move":uci(m)})
    print(f"game {g['game']}: {g['outcome']} by {g['reason']}",file=sys.stderr)
print(f"done: {g.get('wins')} won, {g.get('draws')} drawn, {g.get('losses')} lost",file=sys.stderr)
