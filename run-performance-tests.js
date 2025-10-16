#!/usr/bin/env node

/**
 * Automated Performance Test Runner for CollabCanvas
 * Uses Puppeteer to run browser-based performance tests
 */

const puppeteer = require('puppeteer');

const SERVER_URL = 'http://localhost:6465'; // Vite dev server (proxies to backend)
const TEST_EMAIL = 'perftest@example.com';
const TEST_USERNAME = 'perftest';
const TEST_PASSWORD = 'perftest123';

async function runPerformanceTests() {
  console.log('🚀 Starting CollabCanvas Performance Tests\n');

  const browser = await puppeteer.launch({
    headless: false, // Show browser for visibility
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();

  // Listen to console logs from the page
  page.on('console', msg => {
    const text = msg.text();
    if (text.includes('===') || text.includes('FPS:') || text.includes('Requirements Check')) {
      console.log(`  ${text}`);
    }
  });

  try {
    console.log('📡 Connecting to server...');
    await page.goto(SERVER_URL, { waitUntil: 'networkidle2' });

    console.log('🔐 Authenticating...');

    // Wait for auth modal
    await page.waitForSelector('#auth-modal', { timeout: 5000 });

    // Try to register (or login if already registered)
    await page.click('.tab-btn[data-tab="register"]');
    await page.type('#register-username', TEST_USERNAME);
    await page.type('#register-email', TEST_EMAIL);
    await page.type('#register-password', TEST_PASSWORD);
    await page.click('#register-form .submit-btn');

    // Wait a moment, then try login if registration failed
    await new Promise(resolve => setTimeout(resolve, 1000));

    const modalVisible = await page.evaluate(() => {
      const modal = document.getElementById('auth-modal');
      return !modal.classList.contains('hidden');
    });

    if (modalVisible) {
      console.log('   (User exists, logging in instead)');
      await page.click('.tab-btn[data-tab="login"]');
      await page.type('#login-email', TEST_EMAIL);
      await page.type('#login-password', TEST_PASSWORD);
      await page.click('#login-form .submit-btn');
    }

    // Wait for canvas to load
    await new Promise(resolve => setTimeout(resolve, 2000));

    console.log('✅ Authenticated and canvas loaded\n');

    // Run performance tests
    console.log('🧪 Running comprehensive performance tests...\n');

    const testResults = await page.evaluate(async () => {
      // Check if performance test is available
      if (!window.collabCanvas || !window.collabCanvas.runPerformanceTest) {
        throw new Error('Performance test not available');
      }

      // Run the test
      const results = await window.collabCanvas.runPerformanceTest();

      return {
        success: true,
        results: results
      };
    });

    // Wait for tests to complete (they take about 15 seconds)
    await new Promise(resolve => setTimeout(resolve, 20000));

    // Get final results
    const finalResults = await page.evaluate(() => {
      const tester = window.collabCanvas.performanceTester;
      if (!tester || !tester.testResults) {
        return null;
      }

      return {
        testResults: tester.testResults,
        passed: tester.checkRequirements()
      };
    });

    console.log('\n📊 Test Results Summary:');

    if (finalResults) {
      const { testResults, passed } = finalResults;

      const staticTest = testResults.find(r => r.test === 'static');
      const panTest = testResults.find(r => r.test === 'pan');
      const zoomTest = testResults.find(r => r.test === 'zoom');
      const cullingTest = testResults.find(r => r.test === 'culling');

      if (staticTest) {
        console.log(`   Static FPS: Avg ${staticTest.avgFPS}, Min ${staticTest.minFPS}, Max ${staticTest.maxFPS}`);
        console.log(`   Visible objects: ${staticTest.visibleObjects}/${staticTest.totalObjects}`);
      }

      if (panTest) {
        console.log(`   Pan FPS: Avg ${panTest.avgFPS}, Min ${panTest.minFPS}, Max ${panTest.maxFPS}`);
      }

      if (zoomTest) {
        console.log(`   Zoom FPS: Avg ${zoomTest.avgFPS}, Min ${zoomTest.minFPS}, Max ${zoomTest.maxFPS}`);
      }

      if (cullingTest && cullingTest.positions) {
        const positions = cullingTest.positions;
        const avgVisible = positions.reduce((sum, p) => sum + p.visibleObjects, 0) / positions.length;
        console.log(`   Culling: Avg ${Math.round(avgVisible)} visible objects (${Math.round(avgVisible/positions[0].totalObjects*100)}% of total)`);
      }

      console.log(`\n${passed ? '✅ PASSED' : '❌ FAILED'}: All performance requirements ${passed ? 'met' : 'not met'}`);

      if (!passed) {
        console.log('\n⚠️  Performance issues detected:');
        if (staticTest && staticTest.minFPS < 55) console.log('   - Static FPS below 55');
        if (panTest && panTest.minFPS < 55) console.log('   - Pan FPS below 55');
        if (zoomTest && zoomTest.minFPS < 55) console.log('   - Zoom FPS below 55');
      }
    } else {
      console.log('   ⚠️  Could not retrieve test results');
    }

    // Get latency stats
    console.log('\n📡 Latency Monitoring:');
    const latencyStats = await page.evaluate(() => {
      if (typeof getLatencyStats === 'function') {
        return getLatencyStats();
      }
      return null;
    });

    if (latencyStats) {
      console.log(`   Total messages: ${latencyStats.totalMessages}`);
      console.log(`   Average latency: ${latencyStats.averageLatency?.toFixed(2) || 0}ms`);
      console.log(`   P50: ${latencyStats.p50?.toFixed(2) || 0}ms | P95: ${latencyStats.p95?.toFixed(2) || 0}ms | P99: ${latencyStats.p99?.toFixed(2) || 0}ms`);
      console.log(`   High latency warnings: ${latencyStats.warningCount || 0}`);

      const latencyGood = (latencyStats.p95 || 0) < 100;
      console.log(`   ${latencyGood ? '✅' : '⚠️'} Latency: ${latencyGood ? 'GOOD' : 'NEEDS IMPROVEMENT'}`);
    } else {
      console.log('   ℹ️  No latency data available (no operations performed)');
    }

    console.log('\n✨ Test run complete!\n');

  } catch (error) {
    console.error('❌ Test failed:', error.message);

    // Take screenshot for debugging
    await page.screenshot({ path: 'test-error.png' });
    console.log('   Screenshot saved to test-error.png');
  } finally {
    // Keep browser open for 5 seconds to see results
    console.log('Closing browser in 5 seconds...');
    await new Promise(resolve => setTimeout(resolve, 5000));
    await browser.close();
  }
}

// Check if server is running
async function checkServer() {
  try {
    // Check backend health endpoint (frontend is Vite dev server)
    const response = await fetch(`http://localhost:8080/health`);
    const data = await response.json();
    return data.status === 'healthy';
  } catch (error) {
    return false;
  }
}

// Main execution
(async () => {
  console.log('Checking if server is running...');
  const serverRunning = await checkServer();

  if (!serverRunning) {
    console.error('❌ Server is not running at', SERVER_URL);
    console.log('Please start the server first:');
    console.log('  cd backend && ./start.sh');
    process.exit(1);
  }

  console.log('✅ Server is running\n');

  await runPerformanceTests();
})();
