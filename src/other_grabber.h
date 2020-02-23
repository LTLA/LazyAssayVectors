#ifndef OTHER_GRABBER_H
#define OTHER_GRABBER_H

#include <cstdio>
#include <chrono>
#include <thread>
#include <string>

#include "Rcpp.h"
#include "base_grabber.h"

template<bool GETCOL, typename T, unsigned int RTYPE>
struct other_grabber : public base_grabber<GETCOL> {
    typedef T value_type;
    static const unsigned int sexp_type=RTYPE;

    other_grabber(SEXP data1) : base_grabber<GETCOL>(data1) {
        Rcpp::List details(this->raw_mat);
        auto prefix=Rcpp::as<std::string>(details[0]);
        input=prefix + ".in";
        output=prefix + ".out";
        timeout=Rcpp::as<int>(details[1]);
        return;
    }

    value_type operator()(int i) {
        if (GETCOL) {
            if (!dump_indices(i, this->idx)) {
                warn_timeout();
                return 0;
            }
        } else {
            if (!dump_indices(this->idx, i)) {
                warn_timeout();
                return 0;
            }
        }

        value_type target=0;
        if (!read_results(&target, 1)) {
            warn_timeout();
        }
        return target;            
    }

    void fill(value_type* out) {
        if (GETCOL) {
            if (!dump_indices(-1, this->idx)) {
                warn_timeout();
                return;
            }
            if (!read_results(out, this->dim[0])) {
                warn_timeout();
                return;
            }
        } else {
            if (!dump_indices(this->idx, -1)) {
                warn_timeout();
                return;
            }
            if (!read_results(out, this->dim[1])) {
                warn_timeout();
                return;
            }
        }
        return;
    }
private:
    std::string input, output;
    int timeout;
    static const int delay=20;

    bool dump_indices(int i, int j) {
        int counter=0;
        while (1) {
            // Neither of these files should exist when we want to write the input.
            FILE* fpout = fopen(output.c_str(), "rb");
            if (fpout==NULL) {
                FILE* fpin = fopen(input.c_str(), "rb");
                if (fpin==NULL) {
                    break;
                } else {
                    fclose(fpin);
                }
            } else {
                fclose(fpout);
            }

            // Using a poor man's timer, instead of actually timing things.
            // Whatever, it's close enough.
            if (counter * delay >= timeout * 1000) {
                return false;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(delay));
            ++counter;
        }

        FILE * fpin=fopen(input.c_str(), "wb");
        if (fpin!=NULL) {
            fwrite(&i, sizeof(i), 1, fpin); 
            fwrite(&j, sizeof(j), 1, fpin); 
            fclose(fpin);
            return true;
        } else {
            return false;
        }
    }

    bool read_results(value_type* target, int n) {
        if (n==0) { return true; } // don't even bother.

        int counter=0;
        FILE * fpout;
        while (1) {
            // Output should exist, obviously; but input should not.
            fpout = fopen(output.c_str(), "rb");
            if (fpout!=NULL) {
                FILE* fpin = fopen(input.c_str(), "rb");
                if (fpin==NULL) {
                    break;
                } else {
                    fclose(fpin);
                    fclose(fpout);
                }
            }

            // Again, using our poor man's timer.
            if (counter * delay >= timeout * 1000) {
                return false;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(delay));
            ++counter;
        }

        if (fpout!=NULL) {
            auto ret=fread(target, sizeof(value_type), n, fpout);
            fclose(fpout);
            return ret!=0;
        } else {
            return false;
        }
    }

    void warn_timeout() {
        Rcpp::warning("ALTREP extraction from watcher timed out, see ?restartWatcher");
        return;
    }
};

#endif
