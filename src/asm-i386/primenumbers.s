/* $Id: primenumbers.s,v 1.2 2003/09/23 21:50:24 ndim Exp $
 *
 * assembler version of prime number tests
 * Copyright (C) 2002 Hans Ulrich Niedermann <primes@n-dimensional.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* #include "primenumbers_asm.h" */
	
/**********************************************************************/
.data
starting:
	.ascii "Starting...\n"
finished:
	.ascii "Finished...\n"
msgend:
	len_starting = finished - starting
	len_finished = msgend - finished
/**********************************************************************/
	.global primetable
	.comm	primetable, 4*(1<<16)
/**********************************************************************/
.text
	.local	initprimes
initprimes:
	/* init table with 2 and 3 */
	movl	$2, primetable	
	movl	$3, primetable+4
	movl	$2, %edi		/* "new" index */

__mainloop:
	/* p[new] = p[old] + 2 ; i = 2 */
	movl	primetable-4(,%edi,4), %eax
	movl	$1, %esi
	addl	$2, %eax
	movl	%eax,primetable(,%edi,4)

sqrt:	movl	primetable(,%edi,4), %ecx	/* %ecx = i */
	shrl	$1, %ecx	/* i initial value x/2 */

sqrtloop:
	movl	primetable(,%edi,4), %eax	/* edx:eax = x */
	clrl	%edx
	movl	%ecx, %ebx			/* ebx = l */
	divl	%ebx
	addl	%ebx, %eax
	shrl	%eax		/* new i value in %eax */
	movl	%eax, %ecx	/* %ecx = new i value */
	subl	%eax, %ebx	/* difference l-i */
	incl	%ebx		/* {-1,0,1} -> {0,1,2} */
	cmpl	$2, %ebx
	ja	sqrtloop	/* nice way to determine abs val */
	movl	primetable(,%edi,4), %eax
	clrl	%edx
	divl	%ecx
	addl	%eax, %ecx
	shrl	%ecx		/* ecx contains the sqrt now */

__innerloop:
	/* while (k<i) ... */
	cmpl	%edi, %esi
	jnb	__innerfinished
	/* ... && p[k] <= sqrt(p[i]) */
	cmpl	%ecx, primetable(,%esi,4)
	ja	__innerfinished
	/* if ((p[i] % p[k]) == 0) */
	clrl	%edx
	movl	primetable(,%edi,4), %eax
	movl	primetable(,%esi,4), %ebx
	divl	%ebx
	incl	%esi	/* assume else branch */
	cmpl	$0, %edx
	jne	__innerloop	/* else assumption was correct */
	addl	$2, primetable(,%edi,4)	/* p[i] = p[i] + 2 ; */
	movl    $1,%esi		/* revert else branch */
	jmp	sqrt	/* end while loop */
	
__innerfinished:
	
	addl	$1, %edi
	cmpl	$(1<<16), %edi
	jb	__mainloop
	
	/* end of for loop */
	ret
	
.global	mainp
mainp:
/*
	movl	$len_starting, %edx
	movl	$starting, %ecx
	movl	$1, %ebx
	movl	$0x4,%eax
	int	$0x80
*/
	pushal
	call	initprimes
	popal

/*
	movl	$len_starting, %edx
	movl	$finished, %ecx
	movl	$1, %ebx
	movl	$0x4,%eax
	int	$0x80
*/
	ret

/*
	xorl	%ebx, %ebx
	movl	$0x1, %eax
	int	$0x80
*/

/*
 * Local Variables:
 * mode: asm
 * indent-tabs-mode: t
 * End:
 */
