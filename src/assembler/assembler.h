/*
 * assembler.h
 *
 *  Created on: May 6, 2009
 *      Author: cmrudd
 */

#ifndef ASSEMBLER_H_
#define ASSEMBLER_H_

struct NodeList;

NodeList* parse();
int* compile( NodeList* nodes );

#endif /* ASSEMBLER_H_ */
