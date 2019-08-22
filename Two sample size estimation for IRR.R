
#alfa error
alfa = .05
#beta error
beta = .2
#número de eventos en no expuestos
a0 = 2380
#número de eventos en expuestos
a1 = 14
#tiempo/persona no expuestos
t0 = 10^5
# tiempo persona expuestos
t1 = 10^5
#average number of person-years in group 1
t1.1 = .5
#average number of person-years in group 2
t2.2 = .5
#ratio of sample sizes in the two samples (razón de reclutamiento)
k = .62


#incidence density in group 1
id1 = a0/t0
#incidence density in group 2
id2 = a1/t1
#tiempo sobre la suma del tiempo expuestos y no expuestos
p.0 = t0/(t0+t1)
q.0 = 1 - p.0
irr = (id1)/(id2)
#tiempo 1 por la razón de incidencia sobre tiempo 1 por la razón de incidencia más el tiempo 2
#el tiempo debe encontrarse en años-persona
p.1 = (t0*irr)/((t0*irr)+t1)
q.1 = 1 - p.1

#z critical value of alfa
z1alfa = qnorm(1 - alfa/2)
#z critical value of beta
z1beta = pnorm(1 - alfa/2)

#m expected number of events in the two groups combined (m1 + m2)
m = (((sqrt(p.0*q.0)*z1alfa)+(sqrt(p.1*q.1)*z1beta))^2/(abs(p.0 - p.1)^2))
m

n = ( m /((k+ 1)- exp(-id1*t1.1) - k*exp(-id2*t2.2)))
n

n2 = n*k
n2

n + n2


#Fórmula con años-persona modificados

#alfa error
alfa = .05
#beta error
beta = .2
#número de eventos en no expuestos
a0 = 240
#número de eventos en expuestos
a1 = 111
#tiempo/persona no expuestos
t0 = 201
# tiempo persona expuestos
t1 = 134
#average number of person-years in group 1
t1.1 = .5
#average number of person-years in group 2
t2.2 = .5
#ratio of sample sizes in the two samples (razón de reclutamiento)
k = .62


#incidence density in group 1
id1 = a0/t0
#incidence density in group 2
id2 = a1/t1
#tiempo sobre la suma del tiempo expuestos y no expuestos
p.0 = t0/(t0+t1)
q.0 = 1 - p.0
irr = (id1)/(id2)
#tiempo 1 por la razón de incidencia sobre tiempo 1 por la razón de incidencia más el tiempo 2
#el tiempo debe encontrarse en años-persona
p.1 = (t0*irr)/((t0*irr)+t1)
q.1 = 1 - p.1

#z critical value of alfa
z1alfa = qnorm(1 - alfa/2)
#z critical value of beta
z1beta = qnorm(1 - beta)

#m expected number of events in the two groups combined (m1 + m2)
m = (((sqrt(p.0*q.0)*z1alfa)+(sqrt(p.1*q.1)*z1beta))^2/(abs(p.0 - p.1)^2))
m
n = ( m /((k+ 1)- exp(-id1*t1.1) - k*exp(-id2*t2.2)))
n

n2 = n*k
n2

n + n2



#Ensayo 1

#alfa error
alfa = .05
#beta error
beta = .2
#número de eventos en no expuestos
a0 = 297
#número de eventos en expuestos
a1 = 104
#tiempo/persona no expuestos
t0 = 221
# tiempo persona expuestos
t1 = 130
#average number of person-years in group 1
t1.1 = .5
#average number of person-years in group 2
t2.2 = .5
#ratio of sample sizes in the two samples (razón de reclutamiento)
k = .62

#incidence density in group 1
id1 = a0/t0
#incidence density in group 2
id2 = a1/t1
#tiempo sobre la suma del tiempo expuestos y no expuestos
p.0 = 0.492
q.0 = 1 - p.0
irr = (id1)/(id2)
#tiempo 1 por la razón de incidencia sobre tiempo 1 por la razón de incidencia más el tiempo 2
#el tiempo debe encontrarse en años-persona
p.1 = 0.369
q.1 = 1 - p.1

#z critical value of alfa
z1alfa = qnorm(1 - alfa/2)
#z critical value of beta
z1beta = qnorm(1 - beta)

#m expected number of events in the two groups combined (m1 + m2)
m = (((sqrt(p.0*q.0)*z1alfa)+(sqrt(p.1*q.1)*z1beta))^2/(abs(p.0 - p.1)^2))
m
n = ( m /((k+ 1)- exp(-id1*t1.1) - k*exp(-id2*t2.2)))
n

n2 = n*k
n2

n + n2


-------------------------------------------------------------------------------------
							#Summary
#alfa error
alfa
#beta error
beta
#número de eventos en no expuestos
a0
#número de eventos en expuestos
a1
#tiempo/persona no expuestos
t0
# tiempo persona expuestos
t1
#average number of person-years in group 1
t1.1
#average number of person-years in group 2
t2.2
#ratio of sample sizes in the two samples (razón de reclutamiento)
k

#incidence density in group 1
id1
#incidence density in group 2
id2
#tiempo sobre la suma del tiempo expuestos y no expuestos
p.0
q.0
irr
#tiempo 1 por la razón de incidencia sobre tiempo 1 por la razón de incidencia más el tiempo 2
#el tiempo debe encontrarse en años-persona
p.1
q.1

#Eventos en cada grupo
m

#Grupo 1
n

#Grupo 2
n2

#muestra
n + n2
