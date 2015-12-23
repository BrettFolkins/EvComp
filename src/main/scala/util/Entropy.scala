package com.util

import java.util.concurrent.ThreadLocalRandom

package object Entropy {
    def rand = ThreadLocalRandom.current()
}
