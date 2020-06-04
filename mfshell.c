#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

typedef struct node
{
	char * elem;
	struct node* next;	
	int type;
}node;

typedef struct tree 
{
	node * key;
	struct tree * right;
	struct tree * left;
	int rd;
	int wr;
}tree;

int eoflag = 0;
int Q1f = 0, Q2f=0, Q3f=0;
int Nf = 0;
int Lspf=0;
int dupf=0;
int BMf=0;
int fortype=0;
const char speccheck[5]="&|<>\0";
const char nspeccheck[4]=";()\0";
node * deleted=NULL;
node * processed=NULL;

char * readW();
void CheckAmp(node*);
void MasDel(char **);
void fquotes(char** , int*, int*, int, int);
node * parselist(node *, int *, int *);
void listPrint(node *);
void delprint(node *);
node * insert(node *, char *);
node * CreateNode (char*);
node * listDel(node *);
void errQuot(node *);
char** ListToMas (node*);
int  work (node*, int, int, int *);
int checkcd(char **);
tree * maketree(node *, int, int, int);
int worktree(tree *, int);
node * Del(node *, int, node**);
void sig_child (int);
void killer ();
node * popend (node *);
node * popstart (node *);
void Printtree(tree*);
void DelTree (tree*);

int main()
{
	node * l = NULL;
	char * s;
	tree * root;
	int check=0;
    printf("write command>");
	signal(SIGCHLD, sig_child);
	signal(SIGINT, SIG_IGN);
	while(!eoflag)
	{
		root=NULL;
		l=NULL;
		while (!Nf && !eoflag)
		{
			check=0;
			s=readW();
			if (s) 
			{
				l=insert(l,s);
				free(s);
			}
		}

		if (Q1f || Q2f || Q3f)	
		{
			errQuot(l);
			check=1;
			Q1f=0;
			Q2f=0;
			Q3f=0;
			continue;
		}
		if (!eoflag && !check && l && BMf!=-1)
		{
			CheckAmp(l);
			root=maketree(l, -1, -1, 1);
			worktree(root, 0);
			DelTree(root);
		}
		delprint(deleted);
		listDel(deleted);
		deleted=NULL;
		BMf=0;
		Nf=0;
		if (!eoflag) printf("write command>");			
	}
	listDel(l);
	printf("\n");
	killer();
	printf("\nGOODBUY!\n");
	return 0;
}

void fquotes(char**ps, int*pi, int * pqf, int c, int endsymb)
{
	if (c==endsymb)
	{
		*pqf=0;
	}
	else
	{
		*ps=(char*)realloc(*ps, (*pi+2)*sizeof(char));
		(*ps)[*pi]=c;
		(*pi)++;
	}
}

char * readW()
{
	Lspf=1;
	fortype=0;
	int c=0;
	int i=0;
	char *str;
	str=(char *) malloc(sizeof(char));
	str[i]=0;
	while ((c=getchar()) !=EOF) 
	{
		if (Q1f)
		{
			fquotes(&str, &i, &Q1f, c, '\'');
		}
		else if (Q2f)
		{
			fquotes(&str, &i, &Q2f, c, '"');
		}
		else if (Q3f)
		{
			fquotes(&str, &i, &Q3f, c, '`');
		}
		else if (dupf)
		{
			dupf=0;
			if(c==str[i-1])
			{
				str=(char*)realloc(str, (i+2)*sizeof(char));
				str[i]=c;
				i++;
				str[i]=0;
				return str;
			}
			else 
			{
				ungetc(c,stdin);
				str[i]=0;
				return str;
			}	
		}
		else if (isspace(c))
		{
			if (!Lspf)
			{
				if (c=='\n')
				{
					Nf=1;
				}
				str[i]=0;
				return str;
			}
			else 
			{
				if (c == '\n') 
				{
					Nf = 1;
					free(str);
					return NULL;
				}
				continue;
			}
		} 
		else if (c=='\'')
		{
			Lspf=0;
			Q1f=1;
		}
		else if (c=='"')
		{
			Lspf=0;
			Q2f=1;
		}
		else if (c=='`')
		{
			Lspf=0;
			Q3f=1;
		} 
		
		else if (strchr(speccheck, c))
		{
			if(!Lspf)
			{
				ungetc(c,stdin);
				str[i]=0;
				return str;
			}
			else
			{
				dupf=1;
				fortype=1;
				str=(char*)realloc(str, (i+2)*sizeof(char));
				str[i]=c;
				i++;
			}
		}
		else if (strchr(nspeccheck,c))
		{
			if(!Lspf)
			{
				ungetc(c,stdin);
				str[i]=0;
				return str;
			}
			else
			{
				str=(char*)realloc(str, (i+2)*sizeof(char));
				str[i]=c;
				i++;
				fortype=1;
				str[i]=0;
				return str;
			}
		}
		else
		{
			Lspf=0;
			str=(char*)realloc(str, (i+2)*sizeof(char));
			str[i]=c;
			i++;
		}
	}
	eoflag=1;
	if (!strlen(str)) 
	{
		free(str);
		return NULL;
	}
	else
	{
		str[i]=0;
		return str;
	}
}

void listPrint(node * l)
{
	if(l)
	{
		printf("%s ",l->elem);
		listPrint(l->next);
	}
	return;
}

void delprint(node * l)
{
	if(l)
	{
		printf("Process %s is dead (pid:%d)\n",l->elem, l->type);
		delprint(l->next);
	}	
	return;
}

node * CreateNode (char*s)
{
	node * tmp = (node*)malloc(sizeof(node));
	tmp->elem = (char*)malloc((strlen(s)+1)*sizeof(char));
	tmp->type=fortype;
	strcpy (tmp->elem, s);
	tmp->next = NULL;
	return tmp;
}

node * insert(node * l, char * s)
{
	if (!l) return CreateNode(s);
	l->next=insert(l->next,s);
	return l;
}

node * listDel(node * l)
{
	if(l)
	{
		listDel(l->next);
		free(l->elem);
		free(l);
	}
	return NULL;
}

void errQuot(node * l)
{
	l=listDel(l);
	perror("QUOTES ERROR!");
}

char** ListToMas (node* list)
{
	char ** res=NULL;
	int i=0;
	while (list)
	{
		res=(char**)realloc(res, sizeof(char*)*(i+2));
		res[i]=(char*)malloc(sizeof(char)*(strlen(list->elem)+1));
		strcpy(res[i], list->elem);
		i++;
		list=list->next;
	}
	res[i]=NULL;
	return res;
}

int work(node * l, int r, int w, int * exitcode)
{
	char ** argv;
	argv=ListToMas(l);
	if (checkcd(argv))
	{
		return 1;
	}
	int pid;
	if (BMf) printf("Background mode\n");
	if((pid=fork())==-1)
	{
		perror("FORK ERROR!");
        if (r != -1) 
		{
            close(r);
        }
        if (w != -1) 
		{
            close(w);
        }
        return -1;
	}
	if (!pid)
	{
		if (r != -1) 
		{
			dup2(r, 0);
			close(r);
		}
		if (w != -1) 
		{
			dup2(w, 1);
			close(w);
		}
		if (BMf)
		{
			if (r != -1) 
			{
				r=open("/dev/null", O_RDONLY);
				dup2(r,0);
			}
			if (w != -1) 
			{
				w=open("/dev/null", O_WRONLY);
				dup2(w,1);
			}
		}
		execvp(*argv,argv);
		perror("INCORRECT COMMAND!");
		exit(2);
	}
	else 
	{
		if (!BMf) 
		{
			int status;
			wait (&status);
			*exitcode=WEXITSTATUS(status);
			if (r != -1) 
			{
				close(r);
			}
			if (w != -1)
			{
				close(w);
			}
		}
		else
		{
			fortype=pid;
			processed=insert(processed, argv[0]);
			if (r != -1) 
			{
				close(r);
			}
			if (w != -1) 
			{
				close(w);
			}
		}
	}
	MasDel(argv);
	return pid;
}


int checkcd(char **cmd)
{  
	char * c="cd\0";
	if (strcmp(*cmd,c)==0)
	{ 
		if (cmd[1]==NULL) 
		{
			chdir(getenv("HOME"));
		}
		else
		{
			chdir(cmd[1]); 
		}
		return 1;
	}
	else  return 0;    
}

void CheckAmp (node* l)
{
	node *tmp;
    tmp=l;
    while (tmp && strcmp((tmp->elem),"&"))
	{
        l=tmp;
        tmp=tmp->next;
    }
    if (tmp)
	{
		if (tmp->next) 
		{
			perror("& ERROR!");
			BMf=-1;
		}
		else
		{
			BMf=1;
			free(tmp);
			l->next=NULL;
		}
	}
    return;
}

node * parselist(node * p, int * r, int * w) 
{
	int sk = 0;
	node* res = NULL;
	while (p) 
	{
		int fl = 1;
		if (!strcmp(p->elem, "(")) 
		{
			fl = 0;
			++sk;
		} 
		else if (!strcmp(p->elem, ")")) 
		{
			fl = 0;
			--sk;
		}
		if (p->type == 1 && !sk && fl)   // type(')') != 1, now type(')') == 1.
		{
			if (!strcmp(p->elem, ">>")) 
			{
				if (*w != -1) 
				{
					close(*w);
				}
				*w = open(p->next->elem, O_WRONLY | O_APPEND | O_CREAT, S_IWRITE | S_IREAD);
				p=p->next;
			}
			else if (!strcmp(p->elem, ">")) 
			{
				if (*w != -1) 
				{
					close(*w);
				}
				*w = open(p->next->elem, O_WRONLY | O_CREAT | O_TRUNC, S_IWRITE | S_IREAD);
				p=p->next;
			} 
			else if (!strcmp(p->elem, "<")) 
			{
				if (*r != -1) 
				{
					close(*r);
				}
				*r = open(p->next->elem, O_RDONLY);
				p = p->next;
			}
		}
		else 
		{
			fortype=p->type;
			res = insert(res, p->elem); 
		}
		p=p->next;
	}
	return res;
}


node* Del(node * l, int pid, node** buf)
{
	if (!l) return NULL;
	else if (l->type==pid)
	{
		*buf=l;
		node*ret=l;
		ret=l->next;
		return ret;
	}
	else
	{
		node*ret=l;
		node*pr=l;
		l=l->next;
		while (l)
		{
			if (l->type==pid)
			{
				pr->next=l->next;
				*buf=l;
			}
			l=l->next;
			pr=pr->next;
		}
		return ret;
	}
}

void sig_child (int sig)
{
	int status;
	node* buf=NULL;
	int pid = waitpid(-1, &status, WNOHANG);
	processed=Del(processed, pid, &buf);
	if (buf)
	{
		fortype=buf->type;
		deleted=insert(deleted, buf->elem);
		free(buf->elem);
		free(buf);
	}
	return;
}

void killer()
{
	node * tmp = processed;
	int status;
	int pid;
	while (tmp)
	{
		pid=tmp->type;
		printf("Killing process: %s \t [pid:%d]\n", tmp->elem, tmp->type);
		kill(pid, 9);
		waitpid(pid, &status, 0);
		tmp=tmp->next;
	}
	listDel(processed);
	return;
}

void MasDel (char ** argv)
{
	int i=0;
	while (argv[i])
	{
		free(argv[i]);
		i++;
	}
	free(argv[i]);
	free(argv);
}

tree * maketree (node * l, int r, int w, int fl)
{
	int sk=0;
	int nr;
	int nw;
	tree * res=NULL;
	node * fend=NULL;
	node * fstart=l;
	node * sstart=NULL;
	node * parse;
	node * tmp=l;
	if (!l) 
	{
		return NULL;
	}
	while(tmp->next)
	{
		if (!strcmp(tmp->elem, "("))
		{
			sk++;
		}
		else if (!strcmp(tmp->elem, ")"))
		{
			sk--;
		}
		if (!sk) 
		{
			
			if (!(strcmp(tmp->next->elem, ";"))) 
			{
				fend=tmp;
			}
			else if (!(strcmp(tmp->next->elem, "||")) || !(strcmp(tmp->next->elem, "&&")))
			{
				if (!fend || strcmp(fend->next->elem, ";"))
				{
					fend=tmp;
				}
			}
			else if (!(strcmp(tmp->next->elem, "|")))
			{
				if (!fend || !strcmp(fend->next->elem, "|"))
				{
					fend=tmp;
				}
			}
		}
		tmp=tmp->next;
	}
	if (fend)
	{
		res=(tree*)malloc(sizeof(tree));
		res->key=fend->next;
		fend->next=NULL;
		sstart=res->key->next;
		res->key->next=NULL;
		res->rd=r;
		res->wr=w;
		res->left=maketree(fstart, r, w, 0);
		res->right=maketree(sstart, r, w, 0);
		if (r!=-1)
		{
			close(r);
		}
		if(w!=-1)
		{
			close(w);
		}
		return res;
	}
	if (!fl)
	{
		if (r!=-1)
		{
			nr = dup(r);
		}
		else 
		{
			nr=-1;
		}
		if (w!=-1)
		{
			nw = dup(w);
		}
		else 
		{
			nw=-1;
		}
		parse=parselist(l, &nr, &nw);
	}
	else 
	{
		parse=parselist(l, &r, &w);
		nr=r;
		nw=w;
	}
	listDel(l);
	if (!strcmp(parse->elem, "("))
	{
		parse=popend(parse);
		parse=popstart(parse);
		return maketree(parse, nr, nw, 1);
	}
	res=(tree*)malloc(sizeof(tree));
	res->key=parse;
	res->rd=nr;
	res->wr=nw;
	res->left=NULL;
	res->right=NULL;
	return res;
}

node * popend (node * l)
{
	node * ret=l;
	node * tmp;
	if (!l) 
	{
		return NULL;
	}
	if (!(l->next))
	{
		return NULL;
	}
	do
	{
		tmp=l;
		l=l->next;
	}
	while (l->next);
	free (l->elem);
	free(l);
	tmp->next=NULL;
	return ret;
}

node * popstart (node * l)
{
	node* tmp=l;
	l=l->next;
	free(tmp->elem);
	free(tmp);
	return l;
}

void Printtree(tree* t)
{
	if (!t) 
	{
		return;
	}
	listPrint(t->key);
	printf("%d %d", t->rd, t->wr);
	if (t->left)
	{
		printf("\n->");
		listPrint(t->left->key);
		
	}
	if (t->right)
	{
		printf("\n->");
		listPrint(t->right->key);
	}
	printf("\n\n");
	Printtree(t->left);
	Printtree(t->right);
	return;
}

int worktree (tree * t, int p)
{
	if (!t)
	{
		return 1;
	}
	int r=0, l=0;
	if (!strcmp(t->key->elem, "||"))
	{
		l = worktree (t->left, p);
		if (l==-1)
		{
			r = worktree (t->right, p);
		}
		if (r==-1)
		{
			return -1;
		}
		else 
		{
			return 1;
		}
	}
	else if (!strcmp(t->key->elem, "&&"))
	{
		l = worktree (t->left, p);
		if (l!=-1)
		{
			r = worktree (t->right, p);
		}
		else
		{
			return -1;
		}
		if (r==-1)
		{
			return -1;
		}
		else 
		{
			return 1;
		}
	}
	else if (!strcmp(t->key->elem, ";"))
	{
		worktree (t->left, p);
		r = worktree (t->right, p);
		if (r==-1)
		{
			return -1;
		}
		else 
		{
			return 1;
		}
	}
	else if (!strcmp(t->key->elem, "|"))
	{
		int fd[2];
		pipe(fd);
		pipe(fd);
		if (t->left->wr!=-1)
		{
			close(t->left->wr);
			t->left->wr=-1;
		}
		if (t->right->rd!=-1)
		{
			close(t->right->rd);
			t->right->rd=-1;
		}
		if (!p)
		{
			t->left->wr=fd[1];
			t->right->rd=fd[0];
			worktree (t->left, 1);
			worktree (t->right, 0);
		}
		else 
		{
			t->left->wr=fd[1];
			t->right->rd=fd[0];
			t->right->wr=t->wr;
			worktree (t->left, 1);
			worktree (t->right, 0);
		}
		return 1;
	}
	else 
	{
		if (!strcmp(t->key->elem, "exit"))
		{
			delprint(deleted);
			listDel(deleted);
			killer();
			DelTree(t);
			printf("\nGOODBYE!");
			exit(0);
		} 
		int e;
		work (t->key, t->rd, t->wr, &e);
		if (e==2) return -1;
	}
	return 1;
}

void DelTree (tree * t)
{
	if (!t) 
	{
		return;
	}
	DelTree(t->left);
	DelTree(t->right);
	listDel(t->key);
	free(t);
	return;
}