
#ifndef KOKONOEPLAYERLITE_H
#define KOKONOEPLAYERLITE_H

/* Refer to the documentation for information on how to use these functions. */

void knl_reset();
void knl_init(unsigned,unsigned);
void knl_stop(unsigned);
void knl_pause(unsigned);
void knl_resume(unsigned);

int knl_get_song(unsigned);
unsigned knl_get_loops(unsigned);

#endif
